#!/usr/bin/env Rscript
# scrape_outlays.R
# =============================================================================
# Pull NIH project funding ("outlay") data plus the other useful fields shipped
# in the same RePORTER release, and stream them to a flat CSV.
#
# Run as a script:
#   Rscript scrape_outlays.R --fiscal-years 2024,2025 --output outlays_24_25.csv
#   Rscript scrape_outlays.R --fiscal-years 2025 --activity-codes R01
#   Rscript scrape_outlays.R --fiscal-years 2024,2025 --text "NovaSeq X"
#
# Or interactively:
#   source("nih_reporter.R"); source("scrape_outlays.R")
#   scrape_outlays(new_client(), fiscal_years = c(2024, 2025))
# =============================================================================

if (!exists("nih_iter_all")) source("nih_reporter.R")

# NA-coalescing for numeric sums (NA/empty -> 0).
na0 <- function(x) if (length(x) == 0 || is.na(x)) 0 else x

# `years` are CALENDAR years of the award-notice date (Jan 1 - Dec 31 each);
# no fiscal_years filter is applied, so all fiscal years are included.
build_criteria <- function(years = NULL, activity_codes = NULL,
                           agencies = NULL, text = NULL) {
  rng <- if (is.null(years)) NULL else notice_span_range(years)
  if (!is.null(text) && nzchar(text)) {
    criteria <- text_search_criteria(text, ALL_TEXT_FIELDS, rng)
  } else {
    criteria <- list()
    if (!is.null(rng)) criteria$award_notice_date <- rng
  }
  if (!is.null(activity_codes)) criteria$activity_codes <- as.list(activity_codes)
  if (!is.null(agencies))       criteria$agencies       <- as.list(agencies)
  criteria
}

scrape_outlays <- function(client, years, activity_codes = NULL,
                           agencies = NULL, text = NULL,
                           output = "nih_outlays.csv", page_size = 500L,
                           all_fields = FALSE) {
  old_opts <- options(scipen = 999)          # write amounts as 500000, not 5e+05
  on.exit(options(old_opts), add = TRUE)
  
  criteria <- build_criteria(years, activity_codes, agencies, text)
  include  <- if (all_fields) NULL else OUTLAY_INCLUDE_FIELDS
  
  total <- nih_count(client, criteria)
  message(sprintf("Matching projects: %s", format(total, big.mark = ",")))
  if (total == 0) { message("Nothing to write."); return(invisible(0L)) }
  
  con <- file(output, "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(paste(FLAT_COLUMNS, collapse = ","), con)
  
  acc <- new.env(parent = emptyenv())
  acc$n <- 0L; acc$award <- 0; acc$direct <- 0; acc$indirect <- 0
  
  write_row <- function(rec) {
    row <- flatten_record(rec)
    df  <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)[FLAT_COLUMNS]
    utils::write.table(df, con, sep = ",", col.names = FALSE, row.names = FALSE,
                       na = "", qmethod = "double")
    acc$n        <- acc$n + 1L
    acc$award    <- acc$award    + na0(as.numeric(row$award_amount))
    acc$direct   <- acc$direct   + na0(as.numeric(row$direct_cost_amt))
    acc$indirect <- acc$indirect + na0(as.numeric(row$indirect_cost_amt))
    if (acc$n %% 2000L == 0L) message(sprintf("  ...%s written",
                                              format(acc$n, big.mark = ",")))
  }
  
  nih_iter_all(client, criteria, include, page_size = page_size, callback = write_row)
  
  message(sprintf("\nWrote %s rows to %s", format(acc$n, big.mark = ","), output))
  message("Funding totals for this pull:")
  message(sprintf("  award_amount  : $%s", format(round(acc$award),    big.mark = ",")))
  message(sprintf("  direct_cost   : $%s", format(round(acc$direct),   big.mark = ",")))
  message(sprintf("  indirect_cost : $%s", format(round(acc$indirect), big.mark = ",")))
  invisible(acc$n)
}

# -----------------------------------------------------------------------------
# Minimal CLI (base R; no extra packages). Multi-value options comma-separated.
# -----------------------------------------------------------------------------
.parse_args <- function(defaults, flags = character(0)) {
  args <- commandArgs(trailingOnly = TRUE); opts <- defaults
  for (f in flags) opts[[f]] <- FALSE
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (startsWith(a, "--")) {
      key <- sub("^--", "", a)
      if (grepl("=", key, fixed = TRUE)) {
        kv <- strsplit(key, "=", fixed = TRUE)[[1]]; opts[[kv[1]]] <- kv[2]; i <- i + 1L; next
      }
      if (key %in% flags) { opts[[key]] <- TRUE; i <- i + 1L; next }
      opts[[key]] <- args[[i + 1L]]; i <- i + 2L; next
    }
    i <- i + 1L
  }
  opts
}
.split_csv <- function(x) if (is.null(x) || x == "") NULL else trimws(strsplit(x, ",", fixed = TRUE)[[1]])

if (.called_as("scrape_outlays.R")) {
  o <- .parse_args(
    defaults = list(`years` = NULL, `activity-codes` = NULL,
                    `agencies` = NULL, `text` = NULL,
                    `output` = "nih_outlays.csv", `page-size` = "500",
                    `min-interval` = "1"),
    flags = c("all-fields")
  )
  if (is.null(o[["years"]]))
    stop("--years is required (award-notice calendar years), e.g. --years 2024,2025")
  yrs <- as.integer(.split_csv(o[["years"]]))
  client <- new_client(min_interval = as.numeric(o[["min-interval"]]))
  scrape_outlays(client,
                 years          = yrs,
                 activity_codes = .split_csv(o[["activity-codes"]]),
                 agencies       = .split_csv(o[["agencies"]]),
                 text           = o[["text"]],
                 output         = o[["output"]],
                 page_size      = as.integer(o[["page-size"]]),
                 all_fields     = isTRUE(o[["all-fields"]]))
}