# NIH Outlay Scraper (R)

An R system for pulling NIH project **funding ("outlay") data** and associated
metadata from the public **NIH RePORTER v2 API**, plus a text-search tool that
counts target phrases across project **titles**, **terms**, and **abstracts**.

No API key is required. This is the R port of the Python version and produces
identical output.

## Files

| File | Purpose |
|------|---------|
| `nih_reporter.R` | Core engine: API client, throttle, retries, and automatic handling of the 10,000-record cap (splits large queries by fiscal year, then by award-amount band). Also `flatten_record()` and `text_search_criteria()`. |
| `scrape_outlays.R` | Pull funding/outlay figures + associated fields, streamed to a flat CSV. |
| `search_terms.R` | Count *NovaSeq X*, *Illumina*, *PacBio* (or any phrases) across title / terms / abstract. |

## Install dependencies

```r
install.packages(c("httr", "jsonlite"))
```

Only `httr` and `jsonlite` are needed — the CLIs use base R for argument parsing,
so no other packages are required.

## Usage

### As command-line scripts (`Rscript`)

Multi-value options are **comma-separated** (e.g. `2024,2025`).

```bash
# Term counts – all years, default terms (NovaSeq X, Illumina, PacBio)
Rscript search_terms.R

# Restrict years and break the totals down by year
Rscript search_terms.R --fiscal-years 2022,2023,2024,2025 --by-year

# Also total award_amount for matching projects (slower)
Rscript search_terms.R --fiscal-years 2023,2024,2025 --with-funding

# Outlay/funding pull for FY2024–FY2025
Rscript scrape_outlays.R --fiscal-years 2024,2025 --output outlays_24_25.csv

# One year, R01s only
Rscript scrape_outlays.R --fiscal-years 2025 --activity-codes R01

# Only projects mentioning a platform (title/terms/abstract)
Rscript scrape_outlays.R --fiscal-years 2024,2025 --text "NovaSeq X"
```

### Interactively (source the functions)

```r
source("nih_reporter.R")
source("search_terms.R")
source("scrape_outlays.R")

client <- new_client()                       # min_interval = 1s by default

# Returns a data.frame you can inspect / plot
counts <- count_terms(client,
                      terms = c("NovaSeq X", "Illumina", "PacBio"),
                      fiscal_years = 2020:2025,
                      by_year = TRUE)

# Stream a funding pull to CSV
scrape_outlays(client, fiscal_years = c(2024, 2025),
               output = "outlays_24_25.csv")
```

## Term-count output

For each term:

- **title** — projects whose *title* contains the phrase
- **terms** — projects whose *project terms* contain the phrase
- **abstract** — projects whose *abstract* contains the phrase
- **any** — projects matching in **any** of the three fields (deduplicated headline count)

Per-field columns overlap (a project can match several fields); **any** is the
deduplicated project count. Writes `term_counts.csv` (and `term_counts_by_year.csv`
with `--by-year`).

## How the target terms are matched

Searches use `operator = "advanced"` with the phrase wrapped in double quotes,
e.g. `"NovaSeq X"`. This forces **exact-phrase** matching so a two-word term like
*NovaSeq X* is matched as an adjacent phrase, not two separate tokens. Matching is
case-insensitive.

## "Outlays" vs. what RePORTER actually releases

RePORTER exposes **obligated award amounts** at the project level
(`award_amount`, direct/indirect costs, per-Institute/Center fundings). That is
the funding data NIH releases per project and is what this tool treats as
"outlays." Strict Treasury-style **outlays** (actual cash disbursed over time)
live in **USASpending.gov**; joining on the CFDA/award identifiers this tool
captures is the natural extension if you need those.

## API etiquette / limits (handled for you)

- NIH asks for **≤ 1 request/second** — throttled by default (`min_interval` /
  `--min-interval`). Run large jobs on weekends or 9pm–5am ET per NIH guidance.
- A single result set is capped at **10,000 records** (max offset 9,999).
  `nih_iter_all()` splits the query automatically to retrieve complete larger sets.
- Transient 429/5xx responses are retried with exponential backoff.

## Note on running from a restricted sandbox

The API host is `api.reporter.nih.gov`. In a network-egress sandbox, add that host
to the allowlist first, otherwise requests return `403 host_not_allowed`. On a
normal machine no such step is needed.

## Existing package alternative

If you'd rather use a maintained package for the querying layer,
[`repoRter.nih`](https://cran.r-project.org/package=repoRter.nih) wraps the same
v2 API (`make_req()` / `get_nih_data()`). This project is self-contained and adds
the automatic >10k splitting and the title/terms/abstract count workflow on top.
