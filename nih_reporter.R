# nih_reporter.R
# =============================================================================
# Core engine for pulling NIH-funded project data (funding / "outlay" amounts
# plus associated metadata) from the public NIH RePORTER v2 API, and for running
# title / terms / abstract text searches.
#
# NIH RePORTER API v2 (no key required):
#     POST https://api.reporter.nih.gov/v2/projects/search
#
# Handled here:
#   * NIH asks for <= 1 request / second  -> built-in throttle (default 1.0s).
#   * A result set is capped at 10,000 records; max offset is 9,999.
#     -> nih_iter_all() transparently splits large queries by fiscal year, then
#        by award-amount band, so complete sets above 10k are still retrievable.
#   * Transient 429 / 5xx errors -> exponential-backoff retry.
#
# Dependencies: httr, jsonlite   (install.packages(c("httr","jsonlite")))
#
# Terminology: RePORTER exposes *obligated award* amounts (award_amount,
# direct_cost_amt, indirect_cost_amt, per-Institute/Center fundings). Those are
# used here as the "outlay / funding" figures. True Treasury-style outlays (cash
# disbursed over time) live in USASpending.gov (see README).
# =============================================================================

library(httr)
library(jsonlite)

API_URL        <- "https://api.reporter.nih.gov/v2/projects/search"
MAX_RESULT_SET <- 10000L   # a single query surfaces at most 10k records
MAX_OFFSET     <- 9999L    # highest record index reachable via offset

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
.nz    <- function(x) if (is.null(x) || length(x) == 0) NA else x

# "General outlays" (funding) + the other project data shipped in the same
# release. RePORTER include-field (PascalCase) names.
OUTLAY_INCLUDE_FIELDS <- c(
  # identifiers
  "ApplId", "SubprojectId", "FiscalYear", "ProjectNum", "CoreProjectNum",
  "ProjectSerialNum",
  # funding / "outlay" figures
  "AwardAmount", "DirectCostAmt", "IndirectCostAmt", "TotalCost",
  "AgencyIcFundings",
  # administering / funding agency
  "AgencyIcAdmin", "AdministeringIc", "AwardType", "ActivityCode",
  "FundingMechanism", "CfdaCode", "ArraFunded", "CovidResponse",
  # organization
  "Organization", "OrgName", "OrgCity", "OrgState", "OrgCountry", "DeptType",
  # people
  "PrincipalInvestigators", "ContactPiName", "ProgramOfficers",
  # dates
  "ProjectStartDate", "ProjectEndDate", "BudgetStart", "BudgetEnd",
  "AwardNoticeDate",
  # review / categorisation
  "StudySection", "FullStudySection", "SpendingCategoriesDesc",
  # descriptive text (searchable fields)
  "ProjectTitle", "PrefTerms", "Terms", "PhrText",
  # status / link
  "IsActive", "ProjectDetailUrl"
)

SEARCH_FIELDS   <- c("projecttitle", "terms", "abstracttext")
ALL_TEXT_FIELDS <- "projecttitle,terms,abstracttext"

# TRUE only when this file was the one launched via `Rscript <name>` (not when
# it is source()d from another script or an interactive session).
.called_as <- function(name) {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) == 1 && basename(sub("^--file=", "", fa)) == name
}

# -----------------------------------------------------------------------------
# Client
# -----------------------------------------------------------------------------
new_client <- function(min_interval = 1.0, max_retries = 5L, timeout = 90) {
  structure(
    list(min_interval = min_interval, max_retries = max_retries,
         timeout = timeout, state = new.env(parent = emptyenv())),
    class = "nih_client"
  )
}

# Single network boundary: POST a payload, honour throttle + retries.
nih_post <- function(client, payload) {
  st <- client$state
  if (is.null(st$last_call)) st$last_call <- 0
  json_body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  last_err <- NULL
  for (attempt in seq_len(client$max_retries)) {
    wait <- client$min_interval - (as.numeric(Sys.time()) - st$last_call)
    if (wait > 0) Sys.sleep(wait)
    resp <- tryCatch(
      httr::POST(API_URL, body = json_body,
                 httr::content_type_json(), httr::accept_json(),
                 httr::timeout(client$timeout)),
      error = function(e) e
    )
    st$last_call <- as.numeric(Sys.time())

    if (inherits(resp, "error")) {
      last_err <- conditionMessage(resp)
      backoff <- min(2^attempt, 30)
      message(sprintf("request error (%s); retry %d/%d in %ds",
                      last_err, attempt, client$max_retries, backoff))
      Sys.sleep(backoff); next
    }

    code <- httr::status_code(resp)
    if (code == 200) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    }
    if (code %in% c(429L, 500L, 502L, 503L, 504L)) {
      backoff <- min(2^attempt, 30)
      message(sprintf("HTTP %d; retry %d/%d in %ds",
                      code, attempt, client$max_retries, backoff))
      last_err <- sprintf("HTTP %d", code)
      Sys.sleep(backoff); next
    }
    # Non-retryable (e.g. 400 bad field) -> surface immediately.
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("HTTP %d: %s", code, substr(body, 1, 500)))
  }
  stop(sprintf("exhausted retries: %s", last_err %||% "unknown"))
}

# Cheap total for a criteria block (pulls a single record).
nih_count <- function(client, criteria) {
  data <- nih_post(client, list(criteria = criteria, offset = 0L, limit = 1L))
  as.integer(data$meta$total %||% 0)
}

# One page, with automatic fallback if an include-field name is rejected (400).
nih_page <- function(client, criteria, include_fields, offset, limit) {
  payload <- list(criteria = criteria, offset = offset, limit = limit)
  if (!is.null(include_fields)) payload$include_fields <- as.list(include_fields)
  tryCatch(
    nih_post(client, payload),
    error = function(e) {
      if (!is.null(include_fields) && grepl("HTTP 400", conditionMessage(e))) {
        message("include_fields rejected; retrying with all fields")
        payload$include_fields <- NULL
        nih_post(client, payload)
      } else stop(e)
    }
  )
}

# Page through one <=10k slice, invoking callback(record) for each record.
nih_paginate <- function(client, criteria, include_fields, page_size, total, callback) {
  fetched <- 0L; offset <- 0L
  cap <- min(total, MAX_RESULT_SET)
  while (fetched < cap && offset <= MAX_OFFSET) {
    limit <- min(page_size, cap - fetched)
    data <- nih_page(client, criteria, include_fields, offset, limit)
    results <- data$results
    if (is.null(results) || length(results) == 0) break
    for (rec in results) callback(rec)
    fetched <- fetched + length(results)
    offset  <- offset  + length(results)
  }
  invisible(fetched)
}

# Retrieve EVERY matching project, working around the 10k cap by splitting the
# query (fiscal year first, then award-amount band). callback(record) per record.
nih_iter_all <- function(client, criteria, include_fields = NULL,
                         page_size = 500L, callback) {
  total <- nih_count(client, criteria)
  if (total == 0) return(invisible(0L))
  if (total <= MAX_RESULT_SET) {
    message(sprintf("fetching %d records", total))
    return(nih_paginate(client, criteria, include_fields, page_size, total, callback))
  }

  rng <- criteria$award_notice_date
  if (!is.null(rng)) {
    yrs <- .notice_years(rng)
    if (length(yrs) > 1) {
      message(sprintf("%d records > %d; splitting across %d award-notice years",
                      total, MAX_RESULT_SET, length(yrs)))
      n <- 0L
      for (y in yrs) {
        sub <- criteria
        sub$award_notice_date <- .clamp_year(rng, y)
        n <- n + nih_iter_all(client, sub, include_fields, page_size, callback)
      }
      return(invisible(n))
    }
  }

  message(sprintf("%d records > %d for a single slice; splitting by award amount",
                  total, MAX_RESULT_SET))
  .split_by_amount(client, criteria, include_fields, page_size,
                   low = 0, high = NULL, callback)
}

# Calendar years spanned by an award-notice-date range.
.notice_years <- function(rng) {
  as.integer(substr(rng$from_date, 1, 4)):as.integer(substr(rng$to_date, 1, 4))
}
# One calendar year's window, clamped to the parent range so splitting never
# widens beyond the original range (%Y-%m-%d strings compare correctly).
.clamp_year <- function(rng, year) {
  yf <- sprintf("%d-01-01", year); yt <- sprintf("%d-12-31", year)
  list(from_date = if (yf < rng$from_date) rng$from_date else yf,
       to_date   = if (yt > rng$to_date)   rng$to_date   else yt)
}

.split_by_amount <- function(client, criteria, include_fields, page_size,
                             low, high, callback) {
  sub <- criteria
  amt <- list(min_amount = low)
  if (!is.null(high)) amt$max_amount <- high
  sub$award_amount_range <- amt

  total <- nih_count(client, sub)
  if (total == 0) return(invisible(0L))
  if (total <= MAX_RESULT_SET)
    return(nih_paginate(client, sub, include_fields, page_size, total, callback))

  if (is.null(high)) {                       # unbounded top band: probe upward
    high2 <- max(low * 2, 1000000)
    n1 <- .split_by_amount(client, criteria, include_fields, page_size, low, high2, callback)
    n2 <- .split_by_amount(client, criteria, include_fields, page_size, high2, NULL, callback)
    return(invisible(n1 + n2))
  }
  mid <- (low + high) %/% 2
  if (mid <= low)                            # cannot subdivide further
    return(nih_paginate(client, sub, include_fields, page_size, MAX_RESULT_SET, callback))
  n1 <- .split_by_amount(client, criteria, include_fields, page_size, low, mid, callback)
  n2 <- .split_by_amount(client, criteria, include_fields, page_size, mid, high, callback)
  invisible(n1 + n2)
}

# -----------------------------------------------------------------------------
# Criteria helper
# -----------------------------------------------------------------------------
# Award-notice-date range helpers. In RePORTER, "Award Notice Date" is the date
# the Notice of Award was issued. Selecting a year means the CALENDAR year
# (Jan 1 - Dec 31) of that notice date -- NOT the NIH federal fiscal year
# (Oct 1 - Sep 30). No fiscal_years filter is applied, so all fiscal years are
# included; each record still carries its own fiscal_year in the output.
notice_year_range <- function(year) {
  list(from_date = sprintf("%d-01-01", as.integer(year)),
       to_date   = sprintf("%d-12-31", as.integer(year)))
}
notice_span_range <- function(years) {
  list(from_date = sprintf("%d-01-01", min(as.integer(years))),
       to_date   = sprintf("%d-12-31", max(as.integer(years))))
}
# One calendar month's award-notice window, e.g. notice_month_range(2024, 3)
# -> 2024-03-01 to 2024-03-31 (handles month length / leap years correctly).
notice_month_range <- function(year, month) {
  first <- as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(month)))
  last  <- seq(first, by = "1 month", length.out = 2)[2] - 1
  list(from_date = format(first, "%Y-%m-%d"), to_date = format(last, "%Y-%m-%d"))
}

# Build criteria matching `term` in the given field(s).
#
# match_mode controls how multi-word terms are handled:
#   "phrase" (DEFAULT) — exact adjacent phrase, e.g. "NovaSeq X" must appear as
#       written. This is what you want for platform/product names.
#   "and" — all words must appear, but can be anywhere in the text. This matches
#       the RePORTER website's default behavior (e.g. typing NovaSeq X with the
#       "And" operator). WARNING: for a term like "NovaSeq X" this produces many
#       false positives because "X" matches X-ray, X chromosome, etc.
#   "or" — any of the words can appear.
#
# Matching is case-insensitive in all modes.
text_search_criteria <- function(term, search_field = ALL_TEXT_FIELDS,
                                 notice_range = NULL,
                                 match_mode = c("phrase", "and", "or")) {
  match_mode <- match.arg(match_mode)
  if (match_mode == "phrase") {
    ats <- list(operator = "advanced", search_field = search_field,
                search_text = sprintf('"%s"', term))
  } else {
    ats <- list(operator = match_mode, search_field = search_field,
                search_text = term)
  }
  criteria <- list(advanced_text_search = ats)
  if (!is.null(notice_range)) criteria$award_notice_date <- notice_range
  criteria
}

# -----------------------------------------------------------------------------
# Flatten one nested project record into a single-row named list
# (same 38 columns as the Python version).
# -----------------------------------------------------------------------------
FLAT_COLUMNS <- c(
  "appl_id","subproject_id","fiscal_year","project_num","core_project_num",
  "activity_code","award_type","funding_mechanism",
  "award_amount","direct_cost_amt","indirect_cost_amt","total_cost",
  "ic_fundings_total","ic_fundings_detail",
  "administering_ic","agency_ic_admin",
  "org_name","org_city","org_state","org_country","dept_type",
  "contact_pi_name","principal_investigators","program_officers",
  "project_start_date","project_end_date","budget_start","budget_end",
  "award_notice_date",
  "study_section","full_study_section","spending_categories",
  "cfda_code","arra_funded","covid_response","is_active",
  "project_title","pref_terms","project_detail_url"
)

.join_key <- function(items, key, sep = "; ") {
  if (is.null(items) || length(items) == 0) return("")
  vals <- vapply(items, function(it) {
    if (is.list(it)) as.character(.nz(it[[key]])) else as.character(it)
  }, character(1))
  paste(vals[!is.na(vals)], collapse = sep)
}

.join_vec <- function(items, sep = "; ") {
  if (is.null(items) || length(items) == 0) return("")
  paste(vapply(items, function(x) as.character(.nz(x)), character(1)), collapse = sep)
}

flatten_record <- function(rec) {
  org  <- rec$organization %||% list()
  icf  <- rec$agency_ic_fundings %||% list()
  ic_total <- if (length(icf)) sum(vapply(icf, function(f)
    as.numeric(f$total_cost %||% 0), numeric(1))) else NA
  ic_detail <- if (length(icf))
    paste(vapply(icf, function(f)
      sprintf("%s:%s", f$abbreviation %||% "?", f$total_cost %||% 0), character(1)),
      collapse = "; ") else ""
  admin <- rec$agency_ic_admin %||% list()
  fss   <- rec$full_study_section

  list(
    appl_id                = .nz(rec$appl_id),
    subproject_id          = .nz(rec$subproject_id),
    fiscal_year            = .nz(rec$fiscal_year),
    project_num            = .nz(rec$project_num),
    core_project_num       = .nz(rec$core_project_num),
    activity_code          = .nz(rec$activity_code),
    award_type             = .nz(rec$award_type),
    funding_mechanism      = .nz(rec$funding_mechanism),
    award_amount           = .nz(rec$award_amount),
    direct_cost_amt        = .nz(rec$direct_cost_amt),
    indirect_cost_amt      = .nz(rec$indirect_cost_amt),
    total_cost             = .nz(rec$total_cost),
    ic_fundings_total      = .nz(ic_total),
    ic_fundings_detail     = ic_detail,
    administering_ic       = .nz(rec$administering_ic %||% admin$abbreviation),
    agency_ic_admin        = .nz(if (is.list(admin)) admin$name else admin),
    org_name               = .nz(org$org_name %||% rec$org_name),
    org_city               = .nz(org$org_city %||% rec$org_city),
    org_state              = .nz(org$org_state %||% rec$org_state),
    org_country            = .nz(org$org_country %||% rec$org_country),
    dept_type              = .nz(org$dept_type %||% rec$dept_type),
    contact_pi_name        = .nz(rec$contact_pi_name),
    principal_investigators= .join_key(rec$principal_investigators, "full_name"),
    program_officers       = .join_key(rec$program_officers, "full_name"),
    project_start_date     = .nz(rec$project_start_date),
    project_end_date       = .nz(rec$project_end_date),
    budget_start           = .nz(rec$budget_start),
    budget_end             = .nz(rec$budget_end),
    award_notice_date      = .nz(rec$award_notice_date),
    study_section          = .nz(rec$study_section),
    full_study_section     = .nz(if (is.list(fss)) fss$name else fss),
    spending_categories    = .join_vec(rec$spending_categories_desc),
    cfda_code              = .nz(rec$cfda_code),
    arra_funded            = .nz(rec$arra_funded),
    covid_response         = .join_vec(rec$covid_response),
    is_active              = .nz(rec$is_active),
    project_title          = .nz(rec$project_title),
    pref_terms             = .nz(rec$pref_terms),
    project_detail_url     = .nz(rec$project_detail_url)
  )
}
