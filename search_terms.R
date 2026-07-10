#!/usr/bin/env Rscript
# search_terms.R
# =============================================================================
# Search NIH RePORTER project TITLES, TERMS, and ABSTRACTS for target phrases and
# report match counts. Defaults to: NovaSeq X, Illumina, PacBio.
#
# For each term:
#   title    - projects whose TITLE contains the phrase
#   terms    - projects whose PROJECT TERMS contain the phrase
#   abstract - projects whose ABSTRACT contains the phrase
#   any      - projects matching in ANY of the three fields (deduplicated total)
# Per-field columns overlap (a project can match several fields); "any" is the
# deduplicated headline count.
#
# Run as a script:
#   Rscript search_terms.R
#   Rscript search_terms.R --fiscal-years 2022,2023,2024,2025 --by-year
#   Rscript search_terms.R --terms "NovaSeq X,Illumina,PacBio" --with-funding
#
# Or interactively:
#   source("nih_reporter.R"); source("search_terms.R")
#   count_terms(new_client(), c("NovaSeq X","Illumina","PacBio"))
# =============================================================================

if (!exists("nih_count")) source("nih_reporter.R")

DEFAULT_TERMS <- c("NovaSeq X", "Illumina", "PacBio")
FIELD_MAP     <- c(title = "projecttitle", terms = "terms", abstract = "abstracttext")

# Sum funding across all projects matching the phrase in any field, over the
# award-notice-date span of `years` (NULL = all time). Defaults to total_cost
# (direct + indirect, rolled up for multi-project grants) -- the figure RePORTER
# shows as the search's total funding. Set cost_field = "award_amount" for the
# narrower per-record award figure.
term_funding <- function(client, term, years = NULL, cost_field = "total_cost") {
  acc <- new.env(parent = emptyenv()); acc$total <- 0
  rng  <- if (is.null(years)) NULL else notice_span_range(years)
  crit <- text_search_criteria(term, ALL_TEXT_FIELDS, rng)
  nih_iter_all(client, crit,
               include_fields = c("ProjectNum", "AwardAmount", "TotalCost"),
               callback = function(rec) {
                 val <- rec[[cost_field]] %||% rec$award_amount %||% 0
                 acc$total <- acc$total + val
               })
  acc$total
}

# Return a one-row data.frame of counts for a single term, over the award-notice
# span of `years`.
counts_for <- function(client, term, years = NULL, with_funding = FALSE) {
  rng <- if (is.null(years)) NULL else notice_span_range(years)
  row <- list(term = term)
  for (nm in names(FIELD_MAP))
    row[[nm]] <- nih_count(client, text_search_criteria(term, FIELD_MAP[[nm]], rng))
  row[["any"]] <- nih_count(client, text_search_criteria(term, ALL_TEXT_FIELDS, rng))
  if (with_funding)
    row[["total_cost"]] <- term_funding(client, term, years)
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

# Main entry point: returns the summary data.frame (invisibly) and prints tables.
# `years` are CALENDAR years of the award-notice date (Jan 1 - Dec 31 each).
count_terms <- function(client, terms = DEFAULT_TERMS, years = NULL,
                        by_year = FALSE, with_funding = FALSE,
                        output = "term_counts.csv") {
  scope <- if (is.null(years)) "all award-notice dates"
  else sprintf("award notice %d-01-01 to %d-12-31",
               min(years), max(years))
  message(sprintf("NIH RePORTER term counts (%s)", scope))
  
  summary <- do.call(rbind, lapply(
    terms, function(t) counts_for(client, t, years, with_funding)))
  
  cat("\nMatch counts (projects)\n")
  print(summary, row.names = FALSE)
  utils::write.csv(summary, output, row.names = FALSE)
  message(sprintf("Wrote summary to %s", output))
  
  if (by_year && !is.null(years)) {
    yr <- do.call(rbind, lapply(sort(years), function(y) {
      r <- list(notice_year = y)
      for (t in terms)
        r[[t]] <- nih_count(client,
                            text_search_criteria(t, ALL_TEXT_FIELDS, notice_year_range(y)))
      as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)
    }))
    cat("\nAny-field matches by award-notice year\n")
    print(yr, row.names = FALSE)
    yr_path <- sub("\\.csv$", "_by_year.csv", output)
    utils::write.csv(yr, yr_path, row.names = FALSE)
    message(sprintf("Wrote yearly breakdown to %s", yr_path))
  }
  invisible(summary)
}

# -----------------------------------------------------------------------------
# Minimal CLI (base R; no extra packages) — supports "--flag value", "--flag",
# and "--flag=value". Multi-value options are comma-separated.
# -----------------------------------------------------------------------------
.parse_args <- function(defaults, flags = character(0)) {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- defaults
  for (f in flags) opts[[f]] <- FALSE
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (startsWith(a, "--")) {
      key <- sub("^--", "", a)
      if (grepl("=", key, fixed = TRUE)) {
        kv <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[kv[1]]] <- kv[2]; i <- i + 1L; next
      }
      if (key %in% flags) { opts[[key]] <- TRUE; i <- i + 1L; next }
      opts[[key]] <- args[[i + 1L]]; i <- i + 2L; next
    }
    i <- i + 1L
  }
  opts
}
.split_csv <- function(x) if (is.null(x) || x == "") NULL else trimws(strsplit(x, ",", fixed = TRUE)[[1]])

if (.called_as("search_terms.R")) {
  o <- .parse_args(
    defaults = list(`terms` = paste(DEFAULT_TERMS, collapse = ","),
                    `years` = NULL, `output` = "term_counts.csv",
                    `min-interval` = "1"),
    flags = c("by-year", "with-funding")
  )
  yrs <- .split_csv(o[["years"]]); if (!is.null(yrs)) yrs <- as.integer(yrs)
  client <- new_client(min_interval = as.numeric(o[["min-interval"]]))
  count_terms(client,
              terms        = .split_csv(o[["terms"]]),
              years        = yrs,
              by_year      = isTRUE(o[["by-year"]]),
              with_funding = isTRUE(o[["with-funding"]]),
              output       = o[["output"]])
}