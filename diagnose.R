# diagnose.R
# =============================================================================
# Diagnostics for reconciling API results against the RePORTER website.
#
# Use these when your numbers don't match the site. They let you:
#   1. See the EXACT JSON payload being sent, so you can paste it into the API's
#      Try-it-out page and confirm the result there.
#   2. Verify that returned records actually contain the phrase (guards against
#      silently invalid search_field values).
#   3. Try alternative modeling choices: subprojects on/off, active-projects
#      flag on/off, calendar year vs. NIH fiscal year.
#
# Usage:
#   source("nih_reporter.R"); source("search_terms.R"); source("diagnose.R")
#   client <- new_client()
#
#   # Show the payload and total for one query
#   show_payload(client, "NovaSeq X", years = 2024)
#
#   # Compare 8 modeling variants against the site (which combination matches?)
#   compare_variants(client, "NovaSeq X", years = 2024)
#
#   # Pull the first N matching records and verify the phrase is present
#   verify_matches(client, "NovaSeq X", years = 2024, n = 20)
# =============================================================================

if (!exists("nih_iter_all")) stop("source('nih_reporter.R') first")

# Print the JSON payload for a query and report the meta.total the API returns.
show_payload <- function(client, term, years = NULL,
                         search_field = ALL_TEXT_FIELDS,
                         include_active = FALSE,
                         exclude_subprojects = FALSE,
                         use_fiscal_years = FALSE) {
  crit <- list(advanced_text_search = list(
    operator = "advanced", search_field = search_field,
    search_text = sprintf('"%s"', term)))
  if (!is.null(years)) {
    if (use_fiscal_years) crit$fiscal_years <- as.list(as.integer(years))
    else                  crit$award_notice_date <- notice_span_range(years)
  }
  if (include_active)      crit$include_active_projects <- TRUE
  if (exclude_subprojects) crit$exclude_subprojects     <- TRUE
  
  payload <- list(criteria = crit, offset = 0L, limit = 1L)
  cat("---- Payload ----\n",
      jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
      "\n-----------------\n", sep = "")
  data <- nih_post(client, payload)
  total <- as.integer(data$meta$total %||% 0)
  cat(sprintf("meta.total = %s\n", format(total, big.mark = ",")))
  invisible(total)
}

# Run 8 combinations and print a compact table. Whichever row matches the site
# tells us which flags the site is using by default.
compare_variants <- function(client, term, years = NULL) {
  grid <- expand.grid(
    date_basis = c("award_notice_date", "fiscal_years"),
    include_active = c(FALSE, TRUE),
    exclude_subprojects = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  out <- data.frame()
  for (i in seq_len(nrow(grid))) {
    r <- grid[i, ]
    tot <- tryCatch(
      show_payload_silent(client, term, years,
                          search_field = ALL_TEXT_FIELDS,
                          include_active = r$include_active,
                          exclude_subprojects = r$exclude_subprojects,
                          use_fiscal_years = (r$date_basis == "fiscal_years")),
      error = function(e) NA_integer_)
    out <- rbind(out, cbind(r, total = tot))
  }
  cat(sprintf("\nCounts for \"%s\" (%s):\n", term,
              if (is.null(years)) "all time" else paste(range(years), collapse = "-")))
  print(out, row.names = FALSE)
  invisible(out)
}

show_payload_silent <- function(client, term, years, search_field,
                                include_active, exclude_subprojects,
                                use_fiscal_years) {
  crit <- list(advanced_text_search = list(
    operator = "advanced", search_field = search_field,
    search_text = sprintf('"%s"', term)))
  if (!is.null(years)) {
    if (use_fiscal_years) crit$fiscal_years <- as.list(as.integer(years))
    else                  crit$award_notice_date <- notice_span_range(years)
  }
  if (include_active)      crit$include_active_projects <- TRUE
  if (exclude_subprojects) crit$exclude_subprojects     <- TRUE
  data <- nih_post(client, list(criteria = crit, offset = 0L, limit = 1L))
  as.integer(data$meta$total %||% 0)
}

# Pull the first n matching records with full text, then confirm the phrase
# is actually present (case-insensitive) in title, terms, or abstract.
verify_matches <- function(client, term, years = NULL, n = 20) {
  crit <- text_search_criteria(term, ALL_TEXT_FIELDS,
                               if (is.null(years)) NULL else notice_span_range(years))
  data <- nih_post(client, list(
    criteria = crit, offset = 0L, limit = as.integer(n),
    include_fields = list("ApplId", "ProjectTitle", "PrefTerms", "Terms",
                          "AbstractText", "AwardNoticeDate", "AwardAmount")))
  recs <- data$results %||% list()
  cat(sprintf("Returned %d of %s matches. Checking phrase presence...\n\n",
              length(recs), format(data$meta$total %||% 0, big.mark = ",")))
  needle <- tolower(term)
  out <- lapply(recs, function(r) {
    title    <- as.character(r$project_title %||% "")
    pref     <- as.character(r$pref_terms    %||% "")
    tms      <- as.character(r$terms         %||% "")
    abst     <- as.character(r$abstract_text %||% "")
    data.frame(
      appl_id       = r$appl_id %||% NA,
      notice_date   = substr(as.character(r$award_notice_date %||% ""), 1, 10),
      award_amount  = r$award_amount %||% NA,
      in_title      = grepl(needle, tolower(title),  fixed = TRUE),
      in_pref_terms = grepl(needle, tolower(pref),   fixed = TRUE),
      in_terms      = grepl(needle, tolower(tms),    fixed = TRUE),
      in_abstract   = grepl(needle, tolower(abst),   fixed = TRUE),
      stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, out)
  df$any_match <- df$in_title | df$in_pref_terms | df$in_terms | df$in_abstract
  cat("Per-record phrase presence:\n"); print(df, row.names = FALSE)
  hit_rate <- mean(df$any_match)
  cat(sprintf("\nPhrase hit rate: %.1f%% (%d/%d)\n",
              100 * hit_rate, sum(df$any_match), nrow(df)))
  if (hit_rate < 0.9)
    cat("WARNING: many returned records do NOT contain the phrase in any field.\n",
        "         The search_field or operator may not be doing what you expect.\n")
  invisible(df)
}

# Pull the FULL matching set and print every record with both cost fields and
# which text field it matched. The two sums at the bottom show award_amount vs
# total_cost, so you can see which one equals the figure on the RePORTER site.
reconcile <- function(client, term, years = NULL) {
  crit <- text_search_criteria(term, ALL_TEXT_FIELDS,
                               if (is.null(years)) NULL else notice_span_range(years))
  recs <- list()
  nih_iter_all(client, crit,
               include_fields = c("ApplId", "ProjectNum", "CoreProjectNum", "ActivityCode",
                                  "SubprojectId", "FiscalYear", "AwardNoticeDate",
                                  "AwardAmount", "DirectCostAmt", "IndirectCostAmt", "TotalCost",
                                  "ProjectTitle", "PrefTerms", "Terms", "AbstractText"),
               callback = function(r) recs[[length(recs) + 1L]] <<- r)
  
  needle <- tolower(term)
  rows <- lapply(recs, function(r) data.frame(
    appl_id      = r$appl_id %||% NA,
    project_num  = r$project_num %||% NA,
    activity     = r$activity_code %||% NA,
    is_subproj   = !is.null(r$subproject_id) && !is.na(r$subproject_id),
    fy           = r$fiscal_year %||% NA,
    notice       = substr(as.character(r$award_notice_date %||% ""), 1, 10),
    award_amount = r$award_amount %||% NA,
    total_cost   = r$total_cost %||% NA,
    in_title     = grepl(needle, tolower(r$project_title %||% ""), fixed = TRUE),
    in_terms     = grepl(needle, tolower(paste(r$pref_terms %||% "", r$terms %||% "")), fixed = TRUE),
    in_abstract  = grepl(needle, tolower(r$abstract_text %||% ""), fixed = TRUE),
    stringsAsFactors = FALSE))
  df <- do.call(rbind, rows)
  
  cat(sprintf("\n%d matching records for \"%s\" (%s)\n", nrow(df), term,
              if (is.null(years)) "all time" else paste(range(years), collapse = "-")))
  if (nrow(df)) print(df, row.names = FALSE)
  cat(sprintf("\nSum award_amount = %s\nSum total_cost   = %s\n",
              format(sum(df$award_amount, na.rm = TRUE), big.mark = ","),
              format(sum(df$total_cost,   na.rm = TRUE), big.mark = ",")))
  invisible(df)
}