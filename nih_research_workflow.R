# NIH research workflow
# R >= 4.1. Put this file beside NIH_Search_Inputs.xlsx.
# Install once in RStudio:
# install.packages(c("dplyr", "tidyr", "purrr", "stringr", "readr", "readxl",
#   "ggplot2", "lubridate", "httr", "jsonlite", "openxlsx", "digest", "base64enc"))
# Daily use: edit NIH_Search_Inputs.xlsx and source run_nih_report.R.
# Sourcing this module loads the functions. The runner controls retrieval and export.

required_packages <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "readxl",
  "ggplot2", "lubridate", "httr", "jsonlite", "openxlsx", "digest", "base64enc")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install packages first: install.packages(c(",
  paste(shQuote(missing_packages), collapse = ", "), "))", call. = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(lubridate)
})

nih_admin_codes <- str_split(paste(
  "NCI NEI NHLBI NHGRI NIA NIAAA NIAID NIAMS NIBIB NICHD NIDCD NIDCR NIDDK",
  "NIDA NIEHS NIGMS NIMH NIMHD NINDS NINR NLM NCATS NCCIH FIC OD CC CIT CSR NCRR NCMHD"), " ")[[1]]
reporter_url <- "https://api.reporter.nih.gov/v2/projects/search"
treasury_url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/mts/mts_table_5"
fy_today <- function() lubridate::year(Sys.Date()) + (lubridate::month(Sys.Date()) >= 10)
sum_known <- function(x) if (length(x) && all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
divide <- function(x, y) x / if_else(is.na(y) | y == 0, NA_real_, as.double(y))
join_text <- function(x) paste(sort(unique(x[!is.na(x) & x != ""])), collapse = "; ")
abort <- function(...) stop(..., call. = FALSE)

# 1. Excel configuration --------------------------------------------------------

read_input_sheet <- function(path, sheet, columns) {
  data <- readxl::read_excel(path, sheet = sheet, skip = 5, col_types = "text",
                            .name_repair = "minimal")
  if (anyDuplicated(names(data)) || !all(columns %in% names(data)))
    abort(sheet, " headers must be in row 6 and include ", paste(columns, collapse = ", "))
  data |> select(all_of(columns)) |>
    mutate(across(everything(), ~na_if(str_trim(.x), ""))) |>
    filter(if_any(everything(), ~!is.na(.x)))
}

nih_pull_settings <- function(report_start_fy, report_end_fy, history_start_fy, period_mode,
                              through_mmdd, activity_codes, admin_institutes, include_s10,
                              include_cash, search_fields) {
  year_value <- function(value) {
    if (length(value) != 1L || is.na(value) || !str_detect(as.character(value), "^[0-9]{4}$"))
      abort("Fiscal years must be four-digit years in run_nih_report.R.")
    as.integer(value)
  }
  if (!is.logical(include_s10) || length(include_s10) != 1L || is.na(include_s10) ||
      !is.logical(include_cash) || length(include_cash) != 1L || is.na(include_cash))
    abort("Set include_s10 and include_cash to TRUE or FALSE.")
  search_fields <- match.arg(search_fields, c("Title + abstract", "Title", "All text"))
  config <- list(start = year_value(report_start_fy), end = year_value(report_end_fy),
                 history = year_value(history_start_fy), mode = toupper(period_mode),
                 through = through_mmdd, include_s10 = include_s10, include_cash = include_cash,
                 search_fields = search_fields)
  if (identical(toupper(config$through), "AUTO")) {
    # Use the same cutoff in every comparison year. February 28 is shared by all years.
    cutoff <- format(Sys.time(), "%m-%d", tz = "America/New_York")
    config$through <- if (cutoff == "02-29") "02-28" else cutoff
  }
  if (!(config$history >= 1985 && config$history <= config$start &&
        config$start <= config$end && config$end <= fy_today())) abort("Check the fiscal-year sequence in run_nih_report.R.")
  if (!config$mode %in% c("FULL_FY", "YTD")) abort("period_mode must be FULL_FY or YTD.")
  if (config$mode == "FULL_FY" && config$end >= fy_today())
    abort("The final fiscal year is in progress. Select YTD and a through_mmdd cutoff.")
  if (config$mode == "YTD" && (is.na(config$through) ||
      !str_detect(config$through, "^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$")))
    abort("For YTD, enter through_mmdd as text such as 10-31.")
  split_codes <- function(codes) {
    if (!is.character(codes) || !length(codes) || anyNA(codes))
      abort("Use ALL or a character vector of codes.")
    codes |> str_split(",") |> unlist(use.names = FALSE) |> str_trim() |> toupper() |> unique()
  }
  config$activities <- split_codes(activity_codes)
  config$ics <- split_codes(admin_institutes)
  if (identical(config$ics, "ALL")) config$ics <- nih_admin_codes
  if (!all(config$ics %in% nih_admin_codes)) abort("Unknown administering institute code in run_nih_report.R.")
  if (!identical(config$activities, "ALL") &&
      !all(str_detect(config$activities, "^[A-Z][0-9]{2}$"))) abort("Use activity codes such as R01,R21 or ALL.")
  periods <- tibble(fiscal_year = seq(config$history, config$end)) |>
    mutate(report_period = fiscal_year >= config$start,
      period_start = as.Date(sprintf("%d-10-01", fiscal_year - 1L)),
      period_end = as.Date(sprintf("%d-09-30", fiscal_year)))
  if (config$mode == "YTD") {
    period_ends <- sprintf("%d-%s", periods$fiscal_year -
      as.integer(substr(config$through, 1, 2) >= "10"), config$through)
    ends <- as.Date(period_ends, format = "%Y-%m-%d")
    if (anyNA(ends) || any(format(ends, "%Y-%m-%d") != period_ends))
      abort("The YTD cutoff must be a valid date in every requested year.")
    periods$period_end <- if_else(periods$report_period, ends, periods$period_end)
    if (any(periods$period_end[periods$report_period] > Sys.Date())) abort("The YTD cutoff is in the future.")
  }
  config$periods <- periods
  config
}

read_nih_inputs <- function(path, settings) {
  if (missing(settings)) abort("Supply the pull settings from run_nih_report.R.")
  config <- settings

  keywords <- read_input_sheet(path, "Keywords", c("Enabled", "Topic", "Ticker", "Kind", "Phrase")) |>
    mutate(Enabled = str_to_upper(Enabled), Ticker = coalesce(Ticker, ""))
  if (anyNA(keywords$Enabled) || !all(keywords$Enabled %in% c("YES", "NO")))
    abort("Each Keywords row needs Enabled set to Yes or No.")
  active <- keywords |> filter(Enabled == "YES") |> select(-Enabled) |> distinct()
  if (!nrow(active)) abort("Enable at least one keyword row.")
  if (anyNA(active) || any(str_detect(active$Phrase, '["\\r\\n]')) ||
      any(str_length(active$Phrase) < 2)) abort("Complete enabled keyword rows. Enter a phrase without quotation marks.")
  if (!all(active$Kind %in% c("Platform", "Modality", "Diagnostics")))
    abort("Kind must be Platform, Modality or Diagnostics.")
  if (any(active$Topic == "__S10__")) abort("__S10__ is reserved for instrument reviews.")
  topics <- active |> distinct(Topic, Ticker, Kind)
  if (anyDuplicated(topics$Topic)) abort("Use the same Ticker and Kind for every alias of a Topic.")
  active <- active |> mutate(Fields = config$search_fields, search_field = recode(Fields, "Title + abstract" = "projecttitle,abstracttext",
    "Title" = "projecttitle", "All text" = "all"))
  reviews <- read_input_sheet(path, "Reviews", c("Application ID", "Topic", "Decision",
    "Instrument", "Vendor", "Assessment", "Notes")) |>
    rename(appl_id = `Application ID`, topic = Topic, decision = Decision,
           instrument = Instrument, vendor = Vendor, assessment = Assessment, notes = Notes)
  if (nrow(reviews)) {
    if (anyNA(reviews$appl_id) || anyNA(reviews$topic) || anyNA(reviews$decision) ||
        !all(str_detect(reviews$appl_id, "^[0-9]+$")) ||
        !all(reviews$decision %in% c("Include", "Exclude")) ||
        anyDuplicated(reviews[c("appl_id", "topic")]))
      abort("Reviews needs a unique Application ID and Topic with Decision Include or Exclude.")
    s10 <- reviews |> filter(topic == "__S10__")
    if (anyNA(s10$assessment) || !all(s10$assessment %in%
        c("Acquisition", "Upgrade", "Existing equipment", "Other", "Unreviewed")))
      abort("Set an Assessment for each __S10__ review.")
  }
  list(config = config, keywords = active, reviews = reviews, settings = settings, input_path = path)
}

# 2. NIH retrieval with retries and complete-count checks -----------------------

new_nih_client <- function() new.env(parent = emptyenv())

nih_request <- function(client, payload) {
  for (attempt in seq_len(6L)) {
    elapsed <- if (is.null(client$last_call)) Inf else as.numeric(Sys.time()) - client$last_call
    if (elapsed < 1.05) Sys.sleep(1.05 - elapsed)
    response <- tryCatch(httr::POST(reporter_url,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      httr::content_type_json(), httr::accept_json(), httr::timeout(120)), error = identity)
    client$last_call <- as.numeric(Sys.time())
    if (!inherits(response, "error")) {
      code <- httr::status_code(response)
      if (code == 200) {
        parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                                    simplifyVector = FALSE)
        if (length(parsed$meta$total) != 1 || !is.numeric(parsed$meta$total) ||
            is.null(parsed$results)) abort("NIH returned an unexpected response schema.")
        return(parsed)
      }
      if (!code %in% c(429, 500, 502, 503, 504)) httr::stop_for_status(response)
    }
    reason <- if (inherits(response, "error")) conditionMessage(response) else paste("HTTP", code)
    if (attempt == 6L) {
      years <- paste(unlist(payload$criteria$fiscal_years), collapse = ", ")
      abort("RePORTER POST failed after 6 attempts: ", reason,
            " [FY ", years, ", offset ", payload$offset %||% 0L,
            ", limit ", payload$limit %||% 1L, "]")
    }
    retry_after <- if (inherits(response, "error")) NA_real_ else
      suppressWarnings(as.numeric(httr::headers(response)[["retry-after"]]))
    wait <- max(c(min(2^attempt, 30), retry_after), na.rm = TRUE)
    message("RePORTER attempt ", attempt, " failed: ", reason, ". Retrying in ", wait, " seconds.")
    Sys.sleep(wait)
  }
}

nih_page <- function(client, criteria, offset = 0L, limit = 500L) {
  nih_request(client, list(criteria = criteria, offset = offset, limit = limit,
                           sort_field = "appl_id", sort_order = "asc"))
}

fetch_projects <- function(client, criteria) {
  first <- nih_page(client, criteria)
  total <- first$meta$total
  if (total > 15000L) {
    if (length(criteria$agencies) <= 1L)
      abort("A single institute has more than 15,000 matches. Narrow the phrase or activity codes. Use NIH ExPORTER for a bulk census.")
    records <- map(criteria$agencies, function(ic) {
      part <- criteria
      part$agencies <- list(ic)
      fetch_projects(client, part)$records
    }) |> flatten()
  } else {
    records <- first$results
    offset <- length(records)
    while (offset < total) {
      page <- nih_page(client, criteria, offset, min(500L, total - offset))
      if (page$meta$total != total || !length(page$results))
        abort("NIH changed during pagination or a page was missing. Rerun the extract.")
      records <- c(records, page$results)
      offset <- length(records)
    }
  }
  ids <- map_chr(records, ~as.character(.x$appl_id %||% NA))
  if (length(records) != total || anyNA(ids) || anyDuplicated(ids))
    abort("Unique application count differs from the NIH total. No completed report was published.")
  if (any(map_lgl(records, ~!is.null(.x$subproject_id)))) abort("Unexpected subproject returned.")
  if (length(records) && (!all(map_int(records, "fiscal_year") %in% unlist(criteria$fiscal_years)) ||
      !all(map_chr(records, ~.x$agency_ic_admin$abbreviation) %in% unlist(criteria$agencies))))
    abort("NIH returned an award outside the requested fiscal year or institutes.")
  list(records = records, total = total)
}

award_schema <- tibble(appl_id = character(), fiscal_year = integer(), core_grant = character(),
  project_number = character(), title = character(), institution = character(), org_id = character(),
  contact_pi = character(), ic = character(), activity = character(), application_type = character(),
  award_usd = double(), direct_usd = double(), indirect_usd = double(), multiyear = logical(),
  notice_date = as.Date(character()), budget_end = as.Date(character()), project_end = as.Date(character()),
  abstract = character(), terms = character(), url = character())

flatten_award <- function(r) {
  scalar <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
  day <- function(x) as.Date(substr(scalar(x), 1, 10))
  tibble(appl_id = scalar(r$appl_id), fiscal_year = as.integer(scalar(r$fiscal_year)),
    core_grant = scalar(r$core_project_num), project_number = scalar(r$project_num),
    title = scalar(r$project_title), institution = scalar(r$organization$org_name),
    org_id = scalar(r$organization$org_ipf_code), contact_pi = scalar(r$contact_pi_name),
    ic = scalar(r$agency_ic_admin$abbreviation), activity = scalar(r$activity_code),
    application_type = scalar(r$project_num_split$appl_type_code),
    award_usd = as.numeric(scalar(r$award_amount)), direct_usd = as.numeric(scalar(r$direct_cost_amt)),
    indirect_usd = as.numeric(scalar(r$indirect_cost_amt)),
    multiyear = case_when(str_to_upper(scalar(r$multiyear_award_flag)) %in% c("Y", "TRUE", "1") ~ TRUE,
      str_to_upper(scalar(r$multiyear_award_flag)) %in% c("N", "FALSE", "0") ~ FALSE, TRUE ~ NA),
    notice_date = day(r$award_notice_date), budget_end = day(r$budget_end), project_end = day(r$project_end_date),
    abstract = scalar(r$abstract_text), terms = join_text(c(r$pref_terms, r$terms)), url = scalar(r$project_detail_url))
}

base_criteria <- function(config, fy, s10 = FALSE) {
  criteria <- list(fiscal_years = as.list(as.integer(fy)), agencies = as.list(config$ics),
                   is_agency_admin = TRUE, exclude_subprojects = TRUE)
  activities <- if (s10) "S10" else config$activities
  if (!identical(activities, "ALL")) criteria$activity_codes <- as.list(activities)
  criteria
}

extract_evidence <- function(title, abstract, terms, phrase, fields) {
  text <- c(Title = title, Abstract = abstract, Terms = terms) |> coalesce("") |> str_squish()
  names(text) <- c("Title", "Abstract", "Terms")
  if (fields == "Title") text <- text["Title"]
  if (fields == "Title + abstract") text <- text[c("Title", "Abstract")]
  positions <- str_locate(text, fixed(phrase, ignore_case = TRUE))[, 1]
  hit <- which(!is.na(positions))
  if (!length(hit)) return(tibble(literal_fields = "API hit requires review", excerpt = NA_character_))
  i <- hit[[1]]
  tibble(literal_fields = paste(names(text)[hit], collapse = ", "),
         excerpt = str_sub(text[[i]], max(1, positions[[i]] - 90), positions[[i]] + nchar(phrase) + 150))
}

pull_research <- function(inputs) {
  config <- inputs$config
  client <- new_nih_client()
  awards <- list(award_schema)
  evidence <- list(tibble(appl_id = character(), fiscal_year = integer(), topic = character(),
                         phrase = character(), literal_fields = character(), excerpt = character()))
  queries <- list()
  denominators <- list()
  full_denominators <- list()
  s10_ids <- character()
  for (fy in seq(config$history, config$end)) {
    base <- base_criteria(config, fy)
    period <- config$periods |> filter(fiscal_year == fy)
    denominator_filter <- base
    if (config$mode == "YTD" && fy >= config$start)
      denominator_filter$award_notice_date <- list(from_date = as.character(period$period_start),
                                                   to_date = as.character(period$period_end))
    if (config$mode == "YTD" && fy >= config$start && fy < fy_today())
      full_denominators[[length(full_denominators) + 1L]] <- tibble(fiscal_year = fy,
        eligible_applications = nih_page(client, base, limit = 1L)$meta$total)
    denominator <- nih_page(client, denominator_filter, limit = 1L)$meta$total
    denominators[[length(denominators) + 1L]] <- tibble(fiscal_year = fy, eligible_applications = denominator)
    queries[[length(queries) + 1L]] <- tibble(fiscal_year = fy, topic = "__UNIVERSE__",
      phrase = "Eligible applications", total = denominator,
      criteria_json = as.character(jsonlite::toJSON(denominator_filter, auto_unbox = TRUE)))
    if (config$include_s10 && fy >= config$start) {
      criteria <- base_criteria(config, fy, s10 = TRUE)
      pulled <- fetch_projects(client, criteria)
      message("FY", fy, "  All S10 instruments  ", pulled$total, " applications")
      rows <- bind_rows(award_schema, map(pulled$records, flatten_award) |> list_rbind())
      awards[[length(awards) + 1L]] <- rows
      s10_ids <- c(s10_ids, rows$appl_id)
      queries[[length(queries) + 1L]] <- tibble(fiscal_year = fy, topic = "__S10__", phrase = "All S10",
        total = pulled$total, criteria_json = as.character(jsonlite::toJSON(criteria, auto_unbox = TRUE)))
    }
  }
  query_cache <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(inputs$keywords))) {
    keyword <- inputs$keywords[i, ]
    criteria <- base_criteria(config, config$history:config$end)
    criteria$advanced_text_search <- list(operator = "advanced", search_field = keyword$search_field,
                                           search_text = paste0('"', keyword$Phrase, '"'))
    key <- paste(keyword$search_field, keyword$Phrase, sep = "|")
    if (exists(key, envir = query_cache, inherits = FALSE)) {
      pulled <- query_cache[[key]]
    } else {
      pulled <- fetch_projects(client, criteria)
      if (sum(inputs$keywords$Phrase == keyword$Phrase & inputs$keywords$search_field == keyword$search_field) > 1)
        query_cache[[key]] <- pulled
    }
    message("All years  ", keyword$Phrase, "  ", pulled$total, " applications")
    rows <- bind_rows(award_schema, map(pulled$records, flatten_award) |> list_rbind())
    awards[[length(awards) + 1L]] <- rows
    quotes <- pmap(rows |> select(title, abstract, terms),
      function(title, abstract, terms) extract_evidence(title, abstract, terms, keyword$Phrase, keyword$Fields)) |>
      list_rbind()
    if (nrow(rows)) evidence[[length(evidence) + 1L]] <- bind_cols(
      rows |> select(appl_id, fiscal_year) |> mutate(topic = keyword$Topic, phrase = keyword$Phrase), quotes)
    queries[[length(queries) + 1L]] <- tibble(fiscal_year = config$history:config$end, topic = keyword$Topic,
      phrase = keyword$Phrase, total = tabulate(match(rows$fiscal_year, config$history:config$end), nbins = config$end-config$history+1L),
      criteria_json = as.character(jsonlite::toJSON(criteria, auto_unbox = TRUE)))
  }
  awards <- bind_rows(awards) |> distinct()
  if (anyDuplicated(awards[c("fiscal_year", "appl_id")]))
    abort("The same award changed between queries. Rerun to obtain a consistent snapshot.")
  list(awards = awards, evidence = bind_rows(evidence), queries = bind_rows(queries),
       denominators = bind_rows(denominators), full_denominators = bind_rows(full_denominators), s10_ids = unique(s10_ids))
}

# 3. Research metrics and analyst review ---------------------------------------

summarise_topics <- function(hits, inputs, denominators) {
  config <- inputs$config
  first_seen <- hits |> filter(!is.na(org_id)) |> group_by(topic, org_id) |>
    summarise(first_observed_fy = first(sort(fiscal_year), default = NA_integer_), .groups = "drop")
  hits <- hits |> left_join(first_seen, by = c("topic", "org_id"))
  current <- hits |> filter(in_period, fiscal_year >= config$start)
  counts <- current |> group_by(topic, fiscal_year) |> summarise(
    applications = n(), core_grants = n_distinct(core_grant, na.rm = TRUE),
    institutions = n_distinct(org_id, na.rm = TRUE),
    first_observed_institutions = n_distinct(org_id[first_observed_fy == first(fiscal_year)], na.rm = TRUE),
    new_awards = sum(application_type == "1", na.rm = TRUE),
    renewals = sum(application_type == "2", na.rm = TRUE),
    continuations = sum(application_type == "5", na.rm = TRUE),
    known_award_usd = sum_known(award_usd), known_direct_usd = sum_known(direct_usd),
    missing_award_amount = sum(is.na(award_usd)), missing_direct_cost = sum(is.na(direct_usd)),
    missing_org_id = sum(is.na(org_id)), multiyear_awards = sum(multiyear, na.rm = TRUE),
    missing_multiyear_flag = sum(is.na(multiyear)), .groups = "drop")
  grid <- crossing(topic = unique(inputs$keywords$Topic), fiscal_year = seq(config$start, config$end))
  summary <- grid |> left_join(counts, by = c("topic", "fiscal_year")) |>
    mutate(known_award_usd = if_else(is.na(applications), 0, known_award_usd),
           known_direct_usd = if_else(is.na(applications), 0, known_direct_usd),
           across(c(applications:continuations, missing_award_amount:missing_multiyear_flag), ~coalesce(.x, 0L)),
           first_observed_institutions = if_else(fiscal_year == config$history,
                                                  NA_integer_, first_observed_institutions)) |>
    left_join(denominators, by = "fiscal_year") |>
    mutate(share_of_eligible = divide(applications, eligible_applications)) |>
    arrange(topic, fiscal_year) |> group_by(topic) |>
    mutate(applications_yoy = divide(applications, lag(applications)) - 1,
      institutions_yoy = divide(institutions, lag(institutions)) - 1,
      award_usd_yoy = if_else(missing_award_amount == 0 & lag(missing_award_amount) == 0,
                               divide(known_award_usd, lag(known_award_usd)) - 1, NA_real_),
      share_change_pp = 100 * (share_of_eligible - lag(share_of_eligible))) |> ungroup() |>
    left_join(inputs$keywords |> distinct(Topic, Ticker, Kind), by = c("topic" = "Topic"))
  if (any(summary$applications > summary$eligible_applications)) abort("Topic count exceeds the eligible universe.")
  institutions <- current |> filter(!is.na(org_id)) |>
    group_by(topic, org_id, fiscal_year) |> summarise(institution = first(institution),
      first_observed_fy = first(first_observed_fy), applications = n(),
      known_award_usd = sum_known(award_usd), missing_award_amount = sum(is.na(award_usd)),
      contact_pis = join_text(contact_pi), .groups = "drop") |>
    mutate(observation = case_when(first_observed_fy == config$history ~ "Present in baseline year",
      first_observed_fy == fiscal_year ~ "First observed in lookback", TRUE ~ "Previously observed")) |>
    arrange(desc(fiscal_year), topic, desc(applications))
  list(summary = summary, institutions = institutions, hits = hits, current = current)
}

s10_candidates <- function(title, abstract, keywords) {
  title <- coalesce(title, "")
  sentences <- str_split(str_squish(coalesce(abstract, "")), "(?<=[.!?])\\s+")[[1]]
  acquisition_sentences <- sentences[str_detect(sentences, regex(
    "\\b(purchas(e|ing)|acquir(e|ing)|acquisition|request|upgrad(e|ing)|replac(e|ing))\\b", ignore_case = TRUE))]
  title_matches <- keywords |> filter(str_detect(title, fixed(Phrase, ignore_case = TRUE)))
  sentence_matches <- keywords |> filter(map_lgl(Phrase, ~any(str_detect(acquisition_sentences,
                                                                            fixed(.x, ignore_case = TRUE)))))
  matched <- bind_rows(title_matches, sentence_matches) |> distinct(Topic, Ticker)
  matched_sentences <- acquisition_sentences[map_lgl(acquisition_sentences,
    ~any(str_detect(.x, fixed(keywords$Phrase, ignore_case = TRUE))))]
  excerpt_sentences <- if (length(matched_sentences)) matched_sentences else acquisition_sentences
  tibble(candidate_topics = join_text(matched$Topic), candidate_tickers = join_text(matched$Ticker),
    screen = case_when(nrow(title_matches) > 0 ~ "Keyword in title",
      nrow(sentence_matches) > 0 ~ "Keyword in acquisition sentence", TRUE ~ "Read instrument description"),
    acquisition_excerpt = if (length(excerpt_sentences)) str_sub(excerpt_sentences[[1]], 1, 1000) else NA_character_)
}

analyse_research <- function(raw, inputs) {
  config <- inputs$config
  awards <- raw$awards |> left_join(config$periods, by = "fiscal_year") |>
    mutate(in_period = if (config$mode == "FULL_FY") TRUE else
      !is.na(notice_date) & notice_date >= period_start & notice_date <= period_end,
      study_language = str_extract_all(coalesce(abstract, ""), regex(
        "(?:clinical|analytical) validat[a-z]*|assay development|develop[^.!?]{0,50}assay|trial endpoint|(?:minimal|molecular) residual disease",
        ignore_case = TRUE)) |> map_chr(join_text),
      trial_ids = str_extract_all(coalesce(abstract, ""), "\\bNCT[0-9]{8}\\b") |> map_chr(join_text))
  evidence <- raw$evidence |>
    left_join(inputs$reviews |> select(appl_id, topic, decision, notes), by = c("appl_id", "topic")) |>
    mutate(included = coalesce(decision, "Include") == "Include") |>
    left_join(awards, by = c("appl_id", "fiscal_year"))
  hits <- evidence |> filter(included) |> distinct(topic, appl_id, fiscal_year, .keep_all = TRUE)
  metrics <- summarise_topics(hits, inputs, raw$denominators)
  s10 <- awards |> filter(appl_id %in% raw$s10_ids, in_period)
  candidates <- pmap(s10 |> select(title, abstract),
                    ~s10_candidates(..1, ..2, inputs$keywords)) |> list_rbind()
  if (!nrow(s10)) candidates <- s10_candidates("", "", inputs$keywords)[0, ]
  s10 <- bind_cols(s10, candidates) |> left_join(
    inputs$reviews |> filter(topic == "__S10__") |> select(-topic), by = "appl_id") |>
    mutate(assessment = coalesce(assessment, "Unreviewed"), decision = coalesce(decision, "Include")) |>
    arrange(desc(fiscal_year), assessment != "Unreviewed", title)
  s10_summary <- s10 |> group_by(fiscal_year) |> summarise(applications = n(),
    known_award_usd = sum_known(award_usd), missing_award_amount = sum(is.na(award_usd)),
    reviewed_applications = sum(assessment != "Unreviewed"),
    acquisitions_reviewed = sum(assessment == "Acquisition" & decision == "Include"),
    upgrades_reviewed = sum(assessment == "Upgrade" & decision == "Include"), .groups = "drop")
  if (config$include_s10) {
    s10_summary <- tibble(fiscal_year = seq(config$start, config$end)) |>
      left_join(s10_summary, by = "fiscal_year") |>
      mutate(known_award_usd = if_else(is.na(applications), 0, known_award_usd),
        across(c(applications, missing_award_amount:upgrades_reviewed), ~coalesce(.x, 0L)))
  }
  unused_reviews <- inputs$reviews |> anti_join(bind_rows(
    raw$evidence |> distinct(appl_id, topic), tibble(appl_id = raw$s10_ids, topic = "__S10__")),
    by = c("appl_id", "topic"))
  list(summary = metrics$summary, institutions = metrics$institutions,
       hits = metrics$hits, current = metrics$current, evidence = evidence,
       awards = awards, s10 = s10, s10_summary = s10_summary, unused_reviews = unused_reviews,
       diagnostic_studies = metrics$current |> filter(topic %in%
         inputs$keywords$Topic[inputs$keywords$Kind == "Diagnostics"]))
}

# 4. Monthly cash and changes between identical search scopes -------------------

pull_cash <- function(start_fy) {
  start <- sprintf("%d-10-01", start_fy - 2L)
  response <- httr::RETRY("GET", treasury_url, query = list(
    filter = paste0("classification_desc:eq:National Institutes of Health,record_date:gte:", start),
    sort = "record_date", `page[size]` = 1000), httr::timeout(90), times = 5, quiet = TRUE)
  httr::stop_for_status(response)
  parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  if (!is.data.frame(parsed$data) || !nrow(parsed$data) ||
      nrow(parsed$data) != as.integer(parsed$meta[["total-count"]])) abort("Incomplete Treasury response.")
  cash <- as_tibble(parsed$data) |> transmute(month = lubridate::floor_date(as.Date(record_date), "month"),
    fiscal_year = as.integer(record_fiscal_year), net_outlays_usd = as.numeric(current_month_net_outly_amt),
    fytd_usd = as.numeric(current_fytd_net_outly_amt), prior_fytd_usd = as.numeric(prior_fytd_net_outly_amt))
  if (anyNA(cash$net_outlays_usd) || anyDuplicated(cash$month)) abort("Treasury has missing or duplicate months.")
  if (nrow(cash) != length(seq(min(cash$month), max(cash$month), by = "month"))) abort("Treasury months are discontinuous.")
  cash |> mutate(prior_month_year = month %m-% lubridate::years(1)) |>
    left_join(cash |> select(month, prior_year_usd = net_outlays_usd), by = c("prior_month_year" = "month")) |>
    mutate(net_outlays_bn = net_outlays_usd / 1e9, month_yoy = divide(net_outlays_usd, prior_year_usd) - 1,
      fytd_yoy = divide(fytd_usd, prior_fytd_usd) - 1,
      trailing_3m_usd = net_outlays_usd + lag(net_outlays_usd) + lag(net_outlays_usd, 2),
      trailing_3m_yoy = divide(trailing_3m_usd, lag(trailing_3m_usd, 12)) - 1) |>
    select(-prior_month_year) |> filter(fiscal_year >= start_fy)
}

compare_snapshot <- function(awards, previous) {
  empty <- tibble(appl_id = character(), fiscal_year = integer(), change = character(),
    prior_award_usd = double(), current_award_usd = double(), dollar_change = double(), title = character(), url = character())
  if (is.null(previous)) return(empty)
  select_fields <- function(x) x |>
    select(appl_id, fiscal_year, award_usd, title, url, budget_end, project_end, abstract) |>
    mutate(present = TRUE)
  full_join(select_fields(previous), select_fields(awards), by = c("appl_id", "fiscal_year"),
            suffix = c("_old", "_new")) |>
    mutate(change = case_when(is.na(present_old) ~ "Newly returned",
      is.na(present_new) ~ "No longer returned",
      coalesce(award_usd_old, -Inf) != coalesce(award_usd_new, -Inf) ~ "Award amount revised",
      coalesce(as.character(budget_end_old), "") != coalesce(as.character(budget_end_new), "") |
      coalesce(as.character(project_end_old), "") != coalesce(as.character(project_end_new), "") |
      coalesce(title_old, "") != coalesce(title_new, "") |
      coalesce(abstract_old, "") != coalesce(abstract_new, "") ~ "Description or dates revised",
      TRUE ~ "Unchanged")) |> filter(change != "Unchanged") |>
    transmute(appl_id, fiscal_year, change, prior_award_usd = award_usd_old,
      current_award_usd = award_usd_new, dollar_change = award_usd_new - award_usd_old,
      title = coalesce(title_new, title_old), url = coalesce(url_new, url_old))
}

# 5. Charts and Excel output ----------------------------------------------------

research_theme <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(plot.title = element_text(face = "bold", colour = "#182B49", size = 15),
      plot.subtitle = element_text(colour = "#52647B", margin = margin(b = 14)),
      plot.caption = element_text(colour = "#52647B", hjust = 0),
      strip.text = element_text(face = "bold", colour = "#182B49", hjust = 0),
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      axis.title = element_blank(), plot.margin = margin(16, 20, 16, 16))
}

make_charts <- function(result, directory) {
  summary <- result$analysis$summary
  period_text <- if (result$inputs$config$mode == "FULL_FY") "Full federal fiscal years" else
    paste("Notices through", result$inputs$config$through, "in each fiscal year")
  top_topics <- summary |> group_by(topic) |> summarise(n = sum(applications), .groups = "drop") |>
    arrange(desc(n), topic) |> slice_head(n = 9)
  summary <- summary |> semi_join(top_topics, by = "topic")
  trend <- ggplot(summary, aes(fiscal_year, applications, group = topic)) +
    geom_line(colour = "#2868A0", linewidth = 0.9) +
    geom_point(colour = "#2868A0", size = 2.8) +
    geom_text(aes(label = applications), vjust = -0.8, size = 3.4, colour = "#182B49") +
    facet_wrap(vars(topic), scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = sort(unique(summary$fiscal_year))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.23))) +
    labs(title = "Most frequently matched topics", subtitle = period_text,
      caption = "One application per topic. Panel scales vary. Topics may overlap.") + research_theme()
  trend_path <- file.path(directory, "topic_trends.png")
  ggsave(trend_path, trend, width = 12, height = max(4.5, ceiling(n_distinct(summary$topic) / 3) * 2.4),
         dpi = 180, bg = "white")
  chart_paths <- list(trends = trend_path)
  if (nrow(result$cash)) {
    cash <- result$cash |> slice_tail(n = 36)
    plot <- ggplot(cash, aes(month, net_outlays_bn)) +
      geom_col(fill = "#BCCEDF", width = 23) + geom_line(colour = "#182B49", linewidth = 0.7) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
      scale_y_continuous(labels = function(x) paste0("$", format(x, trim = TRUE), "B"),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(title = "NIH monthly net cash outlays", subtitle = "Treasury Monthly Treasury Statement, Table 5",
           caption = "Monthly cash outlays include the full NIH line. Latest available month shown.") + research_theme()
    chart_paths$cash <- file.path(directory, "cash_outlays.png")
    ggsave(chart_paths$cash, plot, width = 12, height = 4.3, dpi = 180, bg = "white")
  }
  chart_paths
}

column_labels <- c(in_period = "In comparison period", research_kind = "Research kind",
  fytd_usd = "Fiscal YTD ($)", fytd_yoy = "Fiscal YTD YoY", net_outlays_usd = "Monthly net outlays ($)",
  month_yoy = "Monthly YoY", trailing_3m_usd = "Trailing 3 months ($)", trailing_3m_yoy = "Trailing 3 months YoY",
  missing_award_amount = "Awards missing amount", trial_ids = "Trial IDs", study_language = "Study language",
  ic = "NIH institute", activity = "Activity code", appl_id = "Application ID", fiscal_year = "Fiscal year", topic = "Topic", Ticker = "Ticker",
  applications = "Applications", applications_yoy = "Applications YoY", institutions = "Institutions",
  first_observed_institutions = "First observed institutions", known_award_m = "Matched awards ($M)",
  share_of_eligible = "Share of eligible", new_awards = "New Type 1 awards",
  award_usd = "Award ($)", direct_usd = "Direct costs ($)", indirect_usd = "Indirect costs ($)",
  known_award_usd = "Known awards ($)", known_direct_usd = "Known direct costs ($)",
  eligible_applications = "Eligible applications", institution = "Institution", contact_pi = "Contact PI",
  title = "Project title", url = "RePORTER link", core_grant = "Core grant", org_id = "Institution ID",
  notice_date = "Notice date", budget_end = "Budget end", project_end = "Project end",
  acquisition_excerpt = "Acquisition excerpt", candidate_topics = "Candidate topics",
  candidate_tickers = "Candidate tickers", instrument = "Reviewed instrument", vendor = "Reviewed vendor",
  assessment = "Analyst assessment", decision = "Decision", notes = "Analyst notes")
nice_names <- function(names) {
  known <- column_labels[names]
  if_else(is.na(known), str_to_sentence(str_replace_all(names, "_", " ")), unname(known))
}

write_report_xlsx <- function(result, directory, charts = NULL) {
  wb <- openxlsx::createWorkbook(creator = "NIH Research Workflow")
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial", fontColour = "#182B49")
  header <- openxlsx::createStyle(fgFill = "#182B49", fontColour = "#FFFFFF",
    textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE)
  title_style <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#182B49")
  note_style <- openxlsx::createStyle(fontSize = 10, fontColour = "#52647B")
  number <- openxlsx::createStyle(numFmt = '#,##0;(#,##0);0')
  percent <- openxlsx::createStyle(numFmt = '0.0%;(0.0%);0.0%')
  day <- openxlsx::createStyle(numFmt = "mm/dd/yy")
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  add_table <- function(name, data, title, note) {
    openxlsx::addWorksheet(wb, name, gridLines = FALSE, zoom = 85)
    openxlsx::writeData(wb, name, title, startRow = 2)
    openxlsx::addStyle(wb, name, title_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, name, note, startRow = 3)
    openxlsx::addStyle(wb, name, note_style, rows = 3, cols = 1)
    data <- data |> mutate(across(where(is.character), ~str_sub(.x, 1, 32000)))
    visible <- data
    names(visible) <- nice_names(names(data))
    if (nrow(data) + 6 > 1048576) abort("Excel row limit exceeded. Narrow the input or export returned tibbles to CSV.")
    openxlsx::writeDataTable(wb, name, visible, startRow = 6, tableStyle = "TableStyleMedium2",
      headerStyle = header, withFilter = TRUE, keepNA = FALSE)
    openxlsx::setRowHeights(wb, name, rows = 6, heights = 34)
    openxlsx::setColWidths(wb, name, cols = seq_along(data), widths = 18)
    long <- which(names(data) %in% c("title", "excerpt", "abstract", "acquisition_excerpt", "notes", "criteria_json"))
    label <- which(names(data) %in% c("topic", "topics", "phrases", "period", "institution", "contact_pi", "instrument", "vendor", "screen", "change"))
    if (length(label)) openxlsx::setColWidths(wb, name, cols = label, widths = 30)
    if (length(long)) openxlsx::setColWidths(wb, name, cols = long, widths = 65)
    if (nrow(data)) {
      rows <- seq_len(nrow(data)) + 6L
      openxlsx::addStyle(wb, name, openxlsx::createStyle(valign = "top"),
                         rows, seq_along(data), gridExpand = TRUE, stack = TRUE)
      numeric_cols <- which(map_lgl(data, is.numeric) & names(data) != "fiscal_year")
      if (length(numeric_cols)) openxlsx::addStyle(wb, name, number, rows, numeric_cols, gridExpand = TRUE, stack = TRUE)
      year_cols <- which(str_detect(names(data), "_fy$|^fiscal_year$"))
      if (length(year_cols)) openxlsx::addStyle(wb, name, openxlsx::createStyle(numFmt = "0"),
                                               rows, year_cols, gridExpand = TRUE, stack = TRUE)
      percentage_cols <- which(str_detect(names(data), "_yoy$|^share_of_eligible$"))
      if (length(percentage_cols)) openxlsx::addStyle(wb, name, percent, rows, percentage_cols, gridExpand = TRUE, stack = TRUE)
      if ("share_of_eligible" %in% names(data)) openxlsx::addStyle(wb, name,
        openxlsx::createStyle(numFmt = "0.000%"), rows, match("share_of_eligible", names(data)),
        gridExpand = TRUE, stack = TRUE)
      date_cols <- which(map_lgl(data, ~inherits(.x, "Date")))
      if (length(date_cols)) openxlsx::addStyle(wb, name, day, rows, date_cols, gridExpand = TRUE, stack = TRUE)
      if ("month" %in% names(data)) openxlsx::addStyle(wb, name, openxlsx::createStyle(numFmt = "mmm-yy"),
        rows, match("month", names(data)), gridExpand = TRUE, stack = TRUE)
      decimal_cols <- which(str_detect(names(data), "_pp$|_bn$|_m$"))
      if (length(decimal_cols)) openxlsx::addStyle(wb, name, openxlsx::createStyle(numFmt = "0.0"),
                                                 rows, decimal_cols, gridExpand = TRUE, stack = TRUE)
      wrap_cols <- union(long, which(names(data) %in% c("institution", "contact_pi", "screen",
        "study_language", "candidate_topics", "candidate_tickers", "observation")))
      if (length(wrap_cols)) {
        openxlsx::addStyle(wb, name, wrap, rows, wrap_cols, gridExpand = TRUE, stack = TRUE)
        openxlsx::setRowHeights(wb, name, rows, heights = 48)
      }
      if ("url" %in% names(data)) {
        links <- data$url
        class(links) <- "hyperlink"
        openxlsx::writeData(wb, name, links, startCol = match("url", names(data)), startRow = 7,
                            colNames = FALSE, rowNames = FALSE)
        openxlsx::setColWidths(wb, name, cols = match("url", names(data)), widths = 47)
      }
      for (col in percentage_cols) openxlsx::conditionalFormatting(wb, name, cols = col, rows = rows,
        type = "colourScale", style = c("#F1D5D1", "#FFFFFF", "#D8E7F3"))
    } else {
      openxlsx::writeData(wb, name, "No records in this view.", startRow = 9)
    }
    openxlsx::freezePane(wb, name, firstActiveRow = 7, firstActiveCol = 2)
    openxlsx::pageSetup(wb, name, orientation = "landscape", fitToWidth = 1, fitToHeight = 0)
  }
  a <- result$analysis
  cfg <- result$inputs$config
  ytd <- cfg$mode == "YTD"
  cutoff <- format(tail(cfg$periods$period_end, 1), "%B %d")
  basis <- if (ytd) paste("YTD through", cutoff) else "Full FY"
  current <- a$current |> distinct(fiscal_year, appl_id, .keep_all = TRUE)
  totals <- result$raw$denominators |> filter(fiscal_year >= cfg$start) |>
    left_join(current |> count(fiscal_year, name = "matched"), by = "fiscal_year") |>
    mutate(matched = coalesce(matched, 0L), share = divide(matched, eligible_applications))
  last <- slice_tail(totals, n = 1)
  prev <- filter(totals, fiscal_year == cfg$end - 1L)
  fmt <- function(x) if (!length(x) || is.na(x)) "n/a" else format(x, big.mark = ",", trim = TRUE)
  fy <- function(x) paste0("FY", substr(x, 3, 4))
  sheet <- "Overview"
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE, zoom = 85)
  openxlsx::setColWidths(wb, sheet, 1:12, 11.5)
  openxlsx::setRowHeights(wb, sheet, 1:39, 19)
  band <- function(row, cols, text, fill, colour, size, bold = FALSE, height = NULL) {
    openxlsx::mergeCells(wb, sheet, cols, row)
    openxlsx::writeData(wb, sheet, text, startRow = row, startCol = min(cols))
    st <- openxlsx::createStyle(fgFill = fill, fontColour = colour, fontSize = size,
      textDecoration = if (bold) "bold" else NULL, valign = "center", indent = 1)
    openxlsx::addStyle(wb, sheet, st, rows = row, cols = cols, gridExpand = TRUE)
    if (!is.null(height)) openxlsx::setRowHeights(wb, sheet, row, height)
  }
  navy <- "#18344B"; teal <- "#126B68"; pale <- "#E9F3F1"
  band(1, 1:12, "LIFE SCIENCE TOOLS & DIAGNOSTICS", navy, "#ABDCD4", 10, TRUE, 23)
  band(2, 1:12, paste("NIH funding monitor  | ", fy(cfg$end)), navy, "#FFFFFF", 23, TRUE, 40)
  band(3, 1:12, paste(basis, "in each fiscal year   •   Retrieved", substr(result$retrieved_at, 1, 10)),
       navy, "#DFEAF0", 11, height = 26)
  card <- function(cols, label, value, detail) {
    band(5, cols, label, pale, teal, 10, TRUE, 22)
    band(6, cols, value, pale, navy, 22, TRUE, 39)
    band(7, cols, detail, pale, navy, 10, height = 24)
  }
  prior_label <- paste0(fy(cfg$end - 1L), " same period: ")
  previous_value <- function(x) if (nrow(prev)) paste0(prior_label, fmt(x)) else "First year displayed"
  card(1:4, "MATCHED APPLICATIONS", fmt(last$matched), previous_value(prev$matched))
  card(5:8, "ELIGIBLE NIH APPLICATIONS", fmt(last$eligible_applications),
       previous_value(prev$eligible_applications))
  cash <- result$cash
  if (nrow(cash)) {
    latest <- slice_tail(cash, n = 1)
    cash_delta <- ifelse(is.na(latest$month_yoy), "YoY unavailable", sprintf("%+.1f%% YoY", 100 * latest$month_yoy))
    card(9:12, "MONTHLY NIH CASH OUTLAYS", sprintf("$%.2fB", latest$net_outlays_bn),
         paste(format(latest$month, "%b %Y"), cash_delta, sep = "  |  "))
  } else card(9:12, "MATCHED SHARE", ifelse(is.na(last$share), "n/a", sprintf("%.2f%%", last$share*100)),
              "Of eligible applications")
  band(9, 1:12, "Grant activity and cash flow", navy, "white", 14, TRUE, 28)
  shown <- totals |> mutate(year = factor(fy(fiscal_year), levels = fy(fiscal_year)))
  grant_plot <- ggplot(shown, aes(year, matched)) +
    geom_col(aes(fill = fiscal_year == cfg$end), width = .55, show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "#ADC6CE", "TRUE" = teal)) +
    geom_text(aes(label = matched), vjust = -.45, size = 3.8, colour = navy) +
    scale_y_continuous(expand = expansion(mult = c(0, .2)), limits = c(0, max(1,shown$matched)*1.05)) +
    labs(title = "Applications matching the watchlist", subtitle = basis) + research_theme()
  insert_plot <- function(plot, filename, col) {
    path <- file.path(directory, filename)
    ggsave(path, plot, width = 5.45, height = 2.95, dpi = 160, bg = "white")
    openxlsx::insertImage(wb, sheet, path, startRow = 10, startCol = col, width = 5.45, height = 2.95)
  }
  insert_plot(grant_plot, "matched_applications.png", 1)
  if (nrow(cash)) {
    series <- slice_tail(cash, n = 24)
    cash_plot <- ggplot(series, aes(month, net_outlays_bn)) +
      geom_line(linewidth = .85, colour = teal) + geom_point(data = slice_tail(series, n = 1), colour = teal, size = 2.4) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
      scale_y_continuous(labels = function(x) sprintf("$%.1fB", x)) +
      labs(title = "Monthly net cash outlays", subtitle = "U.S. Treasury  |  National Institutes of Health") + research_theme()
    insert_plot(cash_plot, "monthly_outlays.png", 7)
  } else {
    eligible_plot <- ggplot(shown, aes(year, eligible_applications)) + geom_col(fill = teal, width = .55) +
      labs(title = "Eligible applications", subtitle = basis) + research_theme()
    insert_plot(eligible_plot, "eligible_applications.png", 7)
  }
  band(22, 1:12, "Topics with the largest changes in matched applications", navy, "white", 13, TRUE, 28)
  movers <- a$summary |> arrange(topic, fiscal_year) |> group_by(topic) |>
    mutate(previous = lag(applications), delta = applications - previous) |> ungroup() |>
    filter(fiscal_year == cfg$end, applications > 0 | coalesce(previous, 0L) > 0) |>
    arrange(desc(abs(delta)), desc(applications), topic) |> slice_head(n = 5)
  columns <- list(1:4, 5:6, 7:8, 9:10, 11:12)
  labels <- c("Topic", fy(cfg$end-1L), fy(cfg$end), "Change (#)", "Institutions¹")
  walk2(columns, labels, ~band(23, .x, .y, "#DDE8EB", navy, 11, TRUE))
  for (j in seq_len(nrow(movers))) {
    m <- movers[j, ]
    values <- c(m$topic, fmt(m$previous), fmt(m$applications), ifelse(is.na(m$delta), "n/a", sprintf("%+d", m$delta)), fmt(m$institutions))
    walk2(columns, values, ~band(23+j, .x, .y, if (j %% 2) "#F2F6F7" else "#FFFFFF", navy, 11))
  }
  if (!nrow(movers)) band(24, 1:12, "No topic matches in the comparison periods.", "#F2F6F7", navy, 11)
  summary <- a$summary |> mutate(period = basis)
  if (ytd && nrow(result$raw$full_denominators)) {
    full_inputs <- result$inputs
    full_inputs$config$end <- max(result$raw$full_denominators$fiscal_year)
    full_hits <- a$hits |> select(-first_observed_fy) |> filter(fiscal_year <= full_inputs$config$end) |> mutate(in_period = TRUE)
    full <- summarise_topics(full_hits, full_inputs, result$raw$full_denominators)$summary
    summary <- bind_rows(summary, mutate(full, period = "Full FY"))
  }
  next_row <- 25L + max(1L, nrow(movers))
  past <- summary |> filter(period == "Full FY", Kind == "Platform", fiscal_year < fy_today())
  years <- tail(sort(unique(past$fiscal_year)), 3)
  if (length(years) >= 2L && any(past$applications > 0)) {
    ranks <- past |> filter(fiscal_year %in% years) |> group_by(topic) |>
      arrange(fiscal_year, .by_group = TRUE) |>
      summarise(latest = last(applications), total = sum(applications), .groups = "drop") |>
      filter(total > 0) |> arrange(desc(latest), desc(total), topic) |> slice_head(n = 5)
    band(next_row, 1:12, "Most frequently mentioned platforms in completed fiscal years", navy, "white", 13, TRUE, 28)
    year_cols <- if (length(years) == 3L) list(5:6, 7:8, 9:10) else list(5:7, 8:10)
    cols <- c(list(1:4), year_cols, list(11:12))
    walk2(cols, c("Platform", fy(years), "Change (#)"), ~band(next_row+1L, .x, .y, "#DDE8EB", navy, 11, TRUE))
    for (j in seq_len(nrow(ranks))) {
      v <- past |> filter(topic == ranks$topic[j], fiscal_year %in% years) |> arrange(fiscal_year)
      values <- c(ranks$topic[j], map_chr(v$applications, fmt), sprintf("%+d", tail(diff(v$applications), 1)))
      walk2(cols, values, ~band(next_row+1L+j, .x, .y, if (j %% 2) "#F2F6F7" else "#FFFFFF", navy, 11))
    }
    next_row <- next_row + nrow(ranks) + 3L
  }
  band(next_row+0L, 1:12, "¹ Distinct recipients in the latest period. An application can match several topics. The headline counts it once.", "#F2F6F7", navy, 10)
  band(next_row+1L, 1:12, "Recent RePORTER notices can arrive late. Read changes alongside the eligible count, especially early in the fiscal year.", "#F2F6F7", navy, 10)
  if (nrow(prev) && !is.na(last$share) && !is.na(prev$share)) {
    text <- sprintf("Watchlist share of eligible applications was %.2f%% in %s versus %.2f%% in %s.",
                    100*last$share, fy(cfg$end), 100*prev$share, fy(cfg$end-1L))
    band(next_row+2L, 1:12, text, "#F2F6F7", navy, 10)
  }
  band(next_row+4L, 1:12, "Use Topic trends for comparisons and Awards for institution, project, keyword and source detail.", "#FFFFFF", teal, 11, TRUE)
  openxlsx::pageSetup(wb, sheet, orientation = "landscape", fitToWidth = 1, fitToHeight = 1)
  add_table("Topic trends", summary |> arrange(topic, period, fiscal_year) |>
    select(topic, Ticker, Kind, period, fiscal_year, applications, applications_yoy, institutions,
      first_observed_institutions, eligible_applications, share_of_eligible, new_awards,
      known_award_usd, missing_award_amount), "Topic trends",
    "Filter Period before comparing years. Counts deduplicate aliases within a topic. Known awards ($) includes the entire grant.")
  # Keep a single award row when a grant mentions multiple products or aliases.
  tags <- a$evidence |> filter(included) |> left_join(result$inputs$keywords |> distinct(Topic, Kind), by = c("topic" = "Topic")) |>
    group_by(fiscal_year, appl_id) |> summarise(topics = join_text(topic), phrases = join_text(phrase),
      research_kind = join_text(Kind), .groups = "drop")
  awards <- a$awards |> inner_join(tags, by = c("fiscal_year", "appl_id")) |>
    filter(report_period, fiscal_year < fy_today() | in_period) |>
    arrange(desc(fiscal_year), desc(notice_date), appl_id) |>
    select(fiscal_year, in_period, institution, topics, title, award_usd, notice_date, appl_id, phrases,
      research_kind, activity, ic, contact_pi, org_id, core_grant, direct_usd, study_language, trial_ids, url, abstract)
  add_table("Awards", awards, "Awards matching the watchlist",
    "One row per application. Filter In comparison period = TRUE to match the overview. Search Topics or Institution to inspect exposure.")
  if (cfg$include_s10) {
    all_s10 <- a$awards |> filter(appl_id %in% result$raw$s10_ids, report_period, fiscal_year < fy_today() | in_period)
    candidate_rows <- all_s10 |> select(title, abstract) |>
      pmap(~s10_candidates(..1, ..2, result$inputs$keywords)) |> list_rbind()
    if (!nrow(all_s10)) candidate_rows <- s10_candidates("", "", result$inputs$keywords)[0, ]
    s10 <- bind_cols(all_s10, candidate_rows) |> left_join(result$inputs$reviews |> filter(topic=="__S10__") |> select(-topic), by="appl_id") |>
      arrange(desc(fiscal_year), desc(notice_date), appl_id) |>
      select(fiscal_year, in_period, title, institution, award_usd, notice_date, candidate_topics, candidate_tickers,
             acquisition_excerpt, instrument, vendor, assessment, decision, appl_id, url)
    add_table("Instrument grants", s10, "S10 shared instrument grants",
      "NIH instrumentation awards across the selected institutes. Candidate vendors require confirmation in the linked award.")
  }
  if (nrow(cash)) add_table("Cash outlays", cash |> arrange(desc(month)) |>
    select(month, fiscal_year, net_outlays_usd, month_yoy, fytd_usd, fytd_yoy, trailing_3m_usd, trailing_3m_yoy),
    "Monthly NIH cash outlays", "Treasury Monthly Treasury Statement, Table 5. Monthly and fiscal YTD dollars come directly from the NIH net outlays line.")
  # Some openxlsx releases add drawing links to sheets with no drawings.
  # Keep the embedded overview charts and remove unused drawing placeholders.
  for (i in seq_along(wb$worksheets_rels)) {
    rels <- wb$worksheets_rels[[i]]
    unused <- grepl('/vmlDrawing"', rels, fixed = TRUE)
    if (!length(wb$drawings[[i]])) unused <- unused | grepl('/drawing"', rels, fixed = TRUE)
    wb$worksheets_rels[[i]] <- rels[!unused]
  }
  openxlsx::saveWorkbook(wb, file.path(directory, "NIH_Report.xlsx"), overwrite = FALSE)
}

html_escape <- function(x) {
  x |> as.character() |> str_replace_all("&", "&amp;") |> str_replace_all("<", "&lt;") |>
    str_replace_all(">", "&gt;") |> str_replace_all('"', "&quot;")
}
html_table <- function(data) {
  header <- paste0("<th>", html_escape(names(data)), "</th>", collapse = "")
  rows <- pmap_chr(data, function(...) paste0("<tr>", paste0("<td>",
    html_escape(coalesce(as.character(c(...)), "n.a.")), "</td>", collapse = ""), "</tr>"))
  paste0("<div class='table'><table><thead><tr>", header, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table></div>")
}

write_briefing <- function(result, directory, charts) {
  config <- result$inputs$config
  fmt <- function(x, digits = 0) if_else(is.na(x), "n.a.", formatC(x, format = "f", digits = digits, big.mark = ","))
  pct <- function(x, digits = 1) if_else(is.na(x), "n.a.", paste0(fmt(100 * x, digits), "%"))
  summary <- result$analysis$summary |> filter(fiscal_year == config$end) |>
    transmute(Topic = topic, Applications = fmt(applications), YoY = pct(applications_yoy),
      Institutions = fmt(institutions), `First observed` = fmt(first_observed_institutions),
      `Matched awards ($M)` = fmt(if_else(missing_award_amount == 0, known_award_usd / 1e6, NA_real_), 1),
      `Share of eligible` = pct(share_of_eligible, 3))
  image_html <- function(path) paste0("<img alt='NIH trend chart' src='data:image/png;base64,",
                                     base64enc::base64encode(path), "'>")
  s10 <- result$analysis$s10_summary
  s10_html <- if (nrow(s10)) html_table(s10 |> transmute(`Fiscal year` = fiscal_year,
    Applications = applications, `Award funding ($M)` = fmt(if_else(missing_award_amount == 0,
      known_award_usd / 1e6, NA_real_), 1),
    Reviewed = reviewed_applications, `Acquisitions reviewed` = acquisitions_reviewed,
    `Upgrades reviewed` = upgrades_reviewed)) else "<p>No S10 records in the selected scope.</p>"
  cash_html <- ""
  if (nrow(result$cash)) {
    latest <- slice_tail(result$cash, n = 1)
    cash_html <- paste0("<section><h2>NIH cash outlays</h2><p>", format(latest$month, "%B %Y"),
      " net outlays were <strong>$", fmt(latest$net_outlays_bn, 3), "B</strong>, ", pct(latest$month_yoy),
      " versus the same month last year. Fiscal-year-to-date growth was ", pct(latest$fytd_yoy),
      ".</p>", image_html(charts$cash), "</section>")
  }
  period <- if (config$mode == "FULL_FY") "Full federal fiscal years" else paste("Notices through", config$through, "each fiscal year")
  html <- paste0("<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width'>",
    "<title>NIH research monitor</title><style>",
    "body{margin:0;background:#f2f4f7;color:#182b49;font-family:Arial,sans-serif;line-height:1.55}",
    "main{max-width:1150px;margin:36px auto;background:white;padding:40px 48px}",
    "h1{font-size:32px;letter-spacing:-.8px;margin:0 0 8px}h2{font-size:21px;margin:0 0 12px}",
    "header{border-bottom:3px solid #182b49;padding-bottom:23px;margin-bottom:26px}",
    ".meta,.note{color:#52647b;font-size:13px}.table{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:13px}",
    "th{background:#182b49;color:white;font-weight:600}th,td{padding:12px 13px;text-align:right;border-bottom:1px solid #e4e9ef}",
    "th:first-child,td:first-child{text-align:left}tbody tr:nth-child(even){background:#f5f7fa}",
    "section{margin:32px 0}img{display:block;width:100%;height:auto}a{color:#2168a8}",
    "@media print{body{background:white}main{margin:0;padding:12px}section{break-inside:avoid}}",
    "@media(max-width:700px){main{margin:0;padding:22px}h1{font-size:27px}}",
    "</style><main><header><h1>NIH research monitor</h1><div class='meta'>FY", config$start, "–", config$end,
    " · ", html_escape(period), " · Retrieved ", html_escape(result$retrieved_at), "</div></header>",
    "<section><h2>FY", config$end, " topic summary</h2>", html_table(summary),
    "<p class='note'>First observed uses the FY", config$history,
    " onward lookback. Matched dollars represent full awards. Topic rows overlap. n.a. indicates an unavailable comparison.</p>",
    image_html(charts$trends), "</section><section><h2>Shared instrument awards</h2>", s10_html,
    "<p class='note'>The Instrument grants tab includes every instrument in scope. Candidate matches and analyst-reviewed acquisitions are shown separately.</p></section>",
    cash_html, "<section><h2>Changes since the previous extract</h2><p>", html_escape(result$comparison_note),
    "</p><p>", nrow(result$changes), " changed applications in the retrieved union.</p></section>",
    "<footer class='note'>NIH-administered parent applications. Recent periods may be revised as NIH posts awards. ",
    "<a href='https://api.reporter.nih.gov/'>RePORTER</a> · ",
    "<a href='https://report.nih.gov/faqs'>Funding definitions</a> · ",
    "<a href='https://fiscaldata.treasury.gov/datasets/monthly-treasury-statement/'>Treasury</a>",
    "</footer></main></html>")
  writeLines(html, file.path(directory, "NIH_Briefing.html"), useBytes = TRUE)
}

# 6. Public functions used by run_nih_report.R ----------------------------------

collect_nih_research <- function(inputs) {
  started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  message("Reading ", nrow(inputs$keywords), " phrases across FY", inputs$config$history,
          "–", inputs$config$end, ". NIH requests are throttled to about one per second.")
  raw <- pull_research(inputs)
  analysis <- analyse_research(raw, inputs)
  cash <- tibble()
  if (inputs$config$include_cash) cash <- pull_cash(inputs$config$start)
  list(inputs = inputs, raw = raw, analysis = analysis, cash = cash,
       started_at = started, retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

save_nih_excel <- function(result, output_file) {
  if (!identical(tolower(tools::file_ext(output_file)), "xlsx"))
    abort("Choose an output filename ending in .xlsx.")
  folder <- dirname(output_file)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(folder)) abort("Cannot create output folder: ", folder)
  destination <- file.path(normalizePath(folder, winslash = "/"), basename(output_file))
  same_input <- identical(tolower(normalizePath(destination, winslash = "/", mustWork = FALSE)),
                          tolower(normalizePath(result$inputs$input_path, winslash = "/")))
  if (same_input) abort("Choose a report filename different from the input workbook.")
  # Build alongside the destination so an interrupted export leaves the previous report intact.
  stage <- tempfile(".nih_excel_", tmpdir = folder)
  if (!dir.create(stage)) abort("Cannot write in output folder: ", folder)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  write_report_xlsx(result, stage)
  if (!file.rename(file.path(stage, "NIH_Report.xlsx"), destination))
    abort("Could not publish the workbook. Close it in Excel and check write access: ", destination)
  result$output_file <- destination
  message("Saved Excel report: ", destination)
  invisible(result)
}

# Extended export for existing callers that also use snapshots and the HTML briefing.
run_nih <- function(input_file = "NIH_Search_Inputs.xlsx",
                    output_dir = file.path(dirname(input_file), "NIH_Results"), settings) {
  inputs <- read_nih_inputs(input_file, settings)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) abort("Cannot create the output folder.")
  # Review decisions are part of the scope so analyst edits cannot appear as NIH changes.
  signature <- digest::digest(list(version = 1L, config = inputs$config,
    keywords = arrange(inputs$keywords, Topic, Phrase, Fields),
    reviews = arrange(inputs$reviews, appl_id, topic)), algo = "sha256")
  prior <- NULL
  snapshots <- list.files(output_dir, pattern = "^snapshot[.]rds$", recursive = TRUE, full.names = TRUE)
  if (length(snapshots)) {
    snapshots <- snapshots[order(file.info(snapshots)$mtime, decreasing = TRUE)]
    for (path in snapshots) {
      candidate <- readRDS(path)
      if (identical(candidate$signature, signature)) { prior <- candidate; break }
    }
  }
  result <- collect_nih_research(inputs)
  raw <- result$raw
  changes <- compare_snapshot(raw$awards, if (is.null(prior)) NULL else prior$raw$awards)
  comparison_note <- if (is.null(prior)) "First completed extract for these inputs. A later identical-scope run will show changes." else
    paste("Compared with", prior$retrieved_at, ". Newly returned and absent records reflect changes in the extract.")
  result$signature <- signature
  result$changes <- changes
  result$comparison_note <- comparison_note
  stage <- tempfile(".nih_build_", tmpdir = output_dir)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  charts <- make_charts(result, stage)
  write_report_xlsx(result, stage, charts)
  write_briefing(result, stage, charts)
  # Keep the source tables once. Derived views can be rebuilt with analyse_research().
  saveRDS(result[c("signature", "started_at", "retrieved_at", "inputs", "raw", "cash")],
          file.path(stage, "snapshot.rds"))
  readr::write_csv(raw$queries, file.path(stage, "query_log.csv"))
  file.copy(input_file, file.path(stage, "Inputs_used.xlsx"))
  destination <- tempfile(paste0("NIH_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"), tmpdir = output_dir)
  if (!file.rename(stage, destination)) abort("Could not publish the completed output folder.")
  result$output_folder <- normalizePath(destination)
  message("Completed. Open ", file.path(result$output_folder, "NIH_Report.xlsx"))
  message("Briefing available at ", file.path(result$output_folder, "NIH_Briefing.html"))
  invisible(result)
}
