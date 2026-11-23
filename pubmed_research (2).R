# Tools & Diagnostics | PubMed research monitor | R >= 4.1
# Install once: install.packages(c("dplyr","tidyr","purrr","stringr","readr","readxl",
#   "httr","jsonlite","xml2","openxlsx"))
# Keep this script beside PubMed_Search_Inputs.xlsx in your working directory.
# source("pubmed_research.R")
# Sys.setenv(NCBI_EMAIL = "your actual email")  # optional NCBI_API_KEY in .Renviron
# result <- run_pubmed("PubMed_Search_Inputs.xlsx")
# Edit the Search panel sheet, save Excel, then run the line above again.
# Each cell accepts comma-separated terms. R constructs the PubMed query.
# Results are written to a new directory under pubmed_output on each run.
# Default periods are two complete 28-day windows ending yesterday.
# Use end and days arguments for historical comparisons or backfills.
# Inspect generated queries without calling PubMed: pubmed_panel()
# Existing callers can still supply a data frame with topic, scope, tickers, query.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(xml2)
})

# Convert a comma-separated cell into an OR group. Spaces inside a term remain
# part of its phrase. Commas are always delimiters, including inside quotes.
keyword_group <- function(value, field, row, column) {
  terms <- str_split(value, fixed(","))[[1]] |> str_squish()
  terms <- str_replace_all(terms, "[\u201c\u201d]", '"')
  terms <- str_replace(terms, '^"(.*)"$', "\\1") |> str_squish()
  terms <- terms[nzchar(terms)]
  terms <- terms[!duplicated(str_to_lower(terms))]
  if (!length(terms)) return("")
  if (any(str_detect(terms, '["\\[\\]]'))) {
    stop("Search panel row ", row, ", ", column,
         ": enter plain comma-separated terms. Remove query tags and embedded quotes.",
         call. = FALSE)
  }
  paste0("(", paste0('"', terms, '"[', field, "]", collapse = " OR "), ")")
}

# The workbook owns every search term. The reader opens it without saving it.
# Header row 5 is fixed. New rows, blank rows and column reordering are supported.
# Rows sharing a Topic are alternative ways to match that topic, combined by OR.
pubmed_panel <- function(path = "PubMed_Search_Inputs.xlsx") {
  if (!file.exists(path)) stop("Input workbook not found: ", path, call. = FALSE)
  if (!"Search panel" %in% readxl::excel_sheets(path))
    stop("Input workbook needs a sheet named 'Search panel'.", call. = FALSE)
  rows <- readxl::read_xlsx(path, sheet = "Search panel", skip = 4,
    col_types = "text", .name_repair = "minimal")
  required <- c("Enabled", "Topic", "Scope", "Tickers", "Keywords", "Also include",
                "Exclude", "Search in", "Also include 2")
  missing <- setdiff(required, names(rows))
  if (length(missing)) stop("Search panel header row 5 is missing: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  rows <- as_tibble(rows) |>
    select(all_of(required)) |>
    mutate(across(everything(), ~ str_squish(replace_na(as.character(.x), ""))),
           excel_row = row_number() + 5L) |>
    filter(if_any(all_of(required), ~ nzchar(.x)))
  bad_enabled <- !str_to_lower(rows$Enabled) %in% c("", "yes", "no")
  if (any(bad_enabled)) stop("Choose Yes or No in Enabled at Search panel row(s) ",
    paste(rows$excel_row[bad_enabled], collapse = ", "), call. = FALSE)
  rows <- rows |> filter(str_to_lower(Enabled) != "no")
  if (!nrow(rows)) stop("Enable at least one search in the input workbook.", call. = FALSE)
  rows <- rows |> mutate(
    Scope = if_else(Scope == "", "Theme", Scope),
    `Search in` = if_else(`Search in` == "", "Title + abstract", `Search in`),
    Scope = recode(str_to_lower(Scope), "named" = "Named",
                   "theme" = "Theme", "institution" = "Institution", .default = NA_character_),
    `Search in` = recode(str_to_lower(`Search in`),
      "title + abstract" = "tiab", "author affiliation" = "ad", .default = NA_character_))
  bad <- !nzchar(rows$Topic) | !nzchar(rows$Keywords) | is.na(rows$Scope) | is.na(rows$`Search in`)
  if (any(bad)) stop("Check Topic, Keywords, Scope and Search in at Search panel row(s) ",
    paste(rows$excel_row[bad], collapse = ", "), call. = FALSE)
  # Shared topics must have consistent labels so ticker attribution stays stable.
  conflicts <- rows |> group_by(topic_key = str_to_lower(Topic)) |>
    summarise(labels = n_distinct(Topic, Scope, Tickers), .groups = "drop") |>
    filter(labels > 1)
  if (nrow(conflicts)) stop("Rows sharing a Topic must use identical Topic, Scope and Tickers: ",
    paste(conflicts$topic_key, collapse = ", "), call. = FALSE)
  rows <- rows |> mutate(query = pmap_chr(
    list(Keywords, `Also include`, `Also include 2`, Exclude, `Search in`, excel_row),
    function(keywords, also, also2, exclude, field, row) {
      primary <- keyword_group(keywords, field, row, "Keywords")
      if (!nzchar(primary)) stop("Add a keyword at Search panel row ", row, call. = FALSE)
      groups <- c(primary, keyword_group(also, "tiab", row, "Also include"),
                   keyword_group(also2, "tiab", row, "Also include 2"))
      query <- paste(groups[nzchar(groups)], collapse = " AND ")
      excluded <- keyword_group(exclude, "tiab", row, "Exclude")
      if (nzchar(excluded)) query <- paste0("(", query, ") NOT ", excluded)
      paste0("(", query, ")")
    }))
  rows |> group_by(topic = Topic, scope = Scope, tickers = Tickers) |>
    summarise(query = paste(unique(query), collapse = " OR "), .groups = "drop")
}

# GETs are serial. RETRY observes Retry-After on HTTP 429 and uses backoff.
ncbi <- function(endpoint, args, email, key) {
  Sys.sleep(if (nzchar(key)) 0.12 else 0.36)
  args <- c(list(db = "pubmed", tool = "tools_dx_publications"), args)
  if (nzchar(email)) args$email <- email
  if (nzchar(key)) args$api_key <- key
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", endpoint, ".fcgi")
  response <- httr::RETRY("GET", url, query = args, httr::timeout(90),
                          times = 5, pause_min = 1, quiet = TRUE)
  if (httr::http_error(response)) stop("NCBI ", endpoint, " HTTP ", httr::status_code(response))
  httr::content(response, as = "text", encoding = "UTF-8")
}

search_window <- function(query, lo, hi, email, key) {
  # Exclude book records. The parser handles journal articles (PubmedArticle).
  term <- sprintf('(%s) AND ("%s"[crdt] : "%s"[crdt]) NOT booksdocs[sb]',
                  query, format(lo, "%Y/%m/%d"), format(hi, "%Y/%m/%d"))
  raw <- ncbi("esearch", list(term = term, retmode = "json", retmax = 9999), email, key)
  result <- jsonlite::fromJSON(raw)$esearchresult
  if (is.null(result$count) || length(result$ERROR) || length(result$errorlist$fieldsnotfound))
    stop("Invalid PubMed response for ", query)
  issues <- unique(as.character(unlist(c(result$warninglist, result$errorlist))))
  issues <- issues[!grepl("no items found", issues, ignore.case = TRUE)]
  if (length(issues)) message("PubMed query note: ", query, " | ", paste(issues, collapse = ", "))
  n <- as.integer(result$count)
  if (n > 9999L) {
    if (lo == hi) stop("More than 9,999 hits on one day. Narrow this query: ", query)
    mid <- lo + floor(as.numeric(hi - lo) / 2)
    halves <- list(search_window(query, lo, mid, email, key),
                   search_window(query, mid + 1, hi, email, key))
    answer <- bind_rows(halves)
    attr(answer, "query_note") <- join_text(unlist(map(halves, attr, "query_note")))
    return(answer)
  }
  ids <- as.character(result$idlist)
  if (length(unique(ids)) != n) stop("Incomplete PMID list for ", query)
  answer <- tibble(pmid = ids)
  attr(answer, "query_note") <- join_text(issues)
  answer
}

join_text <- function(x) paste(unique(x[!is.na(x) & nzchar(x)]), collapse = " | ")
first_text <- function(node, path) str_squish(xml_text(xml_find_first(node, path)))
all_text <- function(node, path) str_squish(xml_text(xml_find_all(node, path)))
parse_paper <- function(node) {
  a <- xml_find_first(node, "./MedlineCitation/Article")
  pd <- xml_find_first(a, "Journal/JournalIssue/PubDate")
  created <- xml_find_first(node, "PubmedData/History/PubMedPubDate[@PubStatus='entrez']")
  grants <- xml_find_all(a, "GrantList/Grant")
  grant_table <- tibble(grant_id = map_chr(grants, first_text, "GrantID"),
                        agency = map_chr(grants, first_text, "Agency"),
                        country = map_chr(grants, first_text, "Country"))
  authors <- xml_find_all(a, "AuthorList/Author")
  author_text <- function(x) coalesce(first_text(x, "CollectiveName"),
    str_squish(paste(coalesce(first_text(x, "ForeName"), ""), coalesce(first_text(x, "LastName"), ""))))
  pmid <- first_text(node, "MedlineCitation/PMID")
  trial_ids <- str_extract_all(str_to_upper(paste(join_text(all_text(a, "DataBankList/DataBank/AccessionNumberList/AccessionNumber")),
    join_text(all_text(a, "Abstract/AbstractText")))), "NCT[0-9]{8}")[[1]]
  tibble(pmid, trial_ids = join_text(trial_ids),
    conflicts = join_text(all_text(node, "MedlineCitation/CoiStatement")),
    title = first_text(a, "ArticleTitle"), journal = first_text(a, "Journal/Title"),
    pub_date_as_listed = join_text(xml_text(xml_children(pd))),
    created = if (inherits(created,"xml_missing")) NA_character_ else
      sprintf("%04d-%02d-%02d", as.integer(first_text(created,"Year")),
              as.integer(first_text(created,"Month")), as.integer(first_text(created,"Day"))),
    abstract = join_text(all_text(a, "Abstract/AbstractText")),
    first_author = if (length(authors)) author_text(authors[[1]]) else NA_character_,
    first_affiliation = first_text(a, "AuthorList/Author[1]/AffiliationInfo/Affiliation"),
    affiliations = join_text(all_text(a, "AuthorList/Author/AffiliationInfo/Affiliation")),
    publication_types = join_text(all_text(a, "PublicationTypeList/PublicationType")),
    notices = join_text(xml_attr(xml_find_all(node, ".//CommentsCorrections"), "RefType")),
    mesh = join_text(all_text(node, "MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName")),
    doi = first_text(node, "PubmedData/ArticleIdList/ArticleId[@IdType='doi']"),
    pmc = first_text(node, "PubmedData/ArticleIdList/ArticleId[@IdType='pmc']"),
    url = paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/"), grants = list(grant_table))
}

fetch_batch <- function(ids, email, key) {
  xml <- ncbi("efetch", list(id = paste(ids, collapse = ","), retmode = "xml"), email, key)
  nodes <- xml_find_all(read_xml(xml, options = "NONET"), "/PubmedArticleSet/PubmedArticle")
  records <- map_dfr(nodes, parse_paper)
  if (!setequal(records$pmid, ids) || nrow(records) != length(ids))
    stop("EFetch returned incomplete records. Rerun this window.")
  records
}

run_pubmed <- function(panel = "PubMed_Search_Inputs.xlsx", end = Sys.Date() - 1, days = 28L,
                       out = "pubmed_output", email = Sys.getenv("NCBI_EMAIL"),
                       key = Sys.getenv("NCBI_API_KEY")) {
  if (is.character(panel) && length(panel) == 1L) panel <- pubmed_panel(panel)
  end <- as.Date(end)
  stopifnot(length(end) == 1L, !is.na(end), length(days) == 1L, is.finite(days),
            days >= 1, days == floor(days), nrow(panel) > 0,
            all(c("topic","scope","tickers","query") %in% names(panel)),
            !anyDuplicated(panel$topic), all(!is.na(panel$query) & nzchar(panel$query)))
  windows <- tibble(period = c("prior","recent"), hi = c(end - days,end),
                    lo = hi - days + 1)
  jobs <- crossing(panel, windows)
  searched <- jobs |>
    mutate(matches = pmap(list(query,lo,hi,topic), function(query,lo,hi,topic) {
      message(topic, " | ", lo, " to ", hi)
      search_window(query,lo,hi,email,key)
    }), query_note = map_chr(matches, ~ join_text(attr(.x,"query_note"))))
  jobs <- select(searched, -matches)
  hits <- searched |>
    unnest(matches) |> distinct(topic, period, pmid, .keep_all = TRUE)
  notes <- jobs |> group_by(topic) |> summarise(query_note = join_text(query_note), .groups = "drop")
  trends <- jobs |> select(topic, scope, tickers, period) |>
    left_join(count(hits, topic, period, name = "papers"), by = c("topic","period")) |>
    mutate(papers = replace_na(papers, 0L)) |>
    pivot_wider(names_from = period, values_from = papers) |>
    mutate(change = recent - prior, pct_change = if_else(prior > 0, change / prior, NA_real_)) |>
    arrange(desc(change), desc(recent)) |> left_join(notes, by = "topic")
  ids <- unique(hits$pmid)
  # The prototype is also the output schema for a completely empty time window.
  proto <- parse_paper(read_xml("<PubmedArticle><MedlineCitation><Article/></MedlineCitation></PubmedArticle>"))[0,]
  papers <- bind_rows(c(list(proto), unname(map(split(ids, ceiling(seq_along(ids)/100)),
                                        fetch_batch, email = email, key = key))))
  grants <- papers |> select(pmid, grants) |>
    unnest(grants, ptype = tibble(grant_id = character(), agency = character(), country = character()))
  papers <- papers |> select(-grants)
  labels <- hits |> group_by(pmid, period) |>
    summarise(topics = join_text(topic), named = join_text(topic[scope == "Named"]),
      named_matches = n_distinct(tickers[scope == "Named"]), watch_tickers = join_text(tickers),
      .groups = "drop")
  review <- papers |> inner_join(labels, by = "pmid") |>
    mutate(text = str_to_lower(paste(title, abstract)),
      comparison = str_detect(text, "head.to.head|benchmark|comparative performance|direct comparison"),
      clinical_evidence = str_detect(text, "clinical utility|prospective|multicent(er|re)|external validation"),
      review_article = str_detect(publication_types, "Review|Meta-Analysis"),
      notice = str_detect(paste(notices, publication_types), regex("retract|expressionofconcern|expression of concern|erratum|corrected", TRUE)),
      read_first = case_when(notice ~ "Check notice", named_matches >= 2 & comparison ~ "Compare platforms",
        clinical_evidence & named_matches > 0 ~ "Assess clinical evidence",
        comparison ~ "Review comparison", TRUE ~ "Other"),
      review_order = match(read_first, c("Check notice","Compare platforms","Assess clinical evidence","Review comparison","Other"))) |>
    arrange(review_order, desc(named_matches), desc(created), pmid) |>
    select(-text, -review_order) |>
    relocate(period, read_first, title, topics, named, watch_tickers, url)
  # text flags indicate articles worth reading; direction and strength require review.
  result <- list(trends = trends, papers = review, matches = hits, grants = grants, queries = jobs)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".build-", tmpdir = out)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  iwalk(result, ~ readr::write_csv(.x, file.path(stage, paste0(.y, ".csv")), na = ""))
  wb <- openxlsx::createWorkbook()
  date_label <- paste(windows$lo, windows$hi, sep = " to ", collapse = "  /  ")
  sheets <- list(Overview = trends, Reading = review |> filter(period == "recent") |>
    select(read_first, title, named, topics, first_author, first_affiliation, journal,
           url, publication_types, review_article, notices, abstract, pmid, trial_ids),
    Grants = grants)
  titles <- c(paste("PubMed additions | Prior / recent:", date_label),
              "Recent papers | Flags guide reading; confirm study design and vendor role in the paper.",
              "Acknowledged grants | Join via PMID. Multiple grants can support one paper.")
  for (i in seq_along(sheets)) {
    sheet <- names(sheets)[i]
    tbl <- sheets[[i]]
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, titles[i], startRow = 1)
    openxlsx::mergeCells(wb, sheet, cols = 1:max(2,ncol(tbl)), rows = 1)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fontColour = "#FFFFFF",
      fgFill = "#153D52", textDecoration = "bold"), rows = 1, cols = 1:ncol(tbl), gridExpand = TRUE)
    openxlsx::writeData(wb, sheet, tbl, startRow = 3, withFilter = TRUE,
      headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#DAEDEB"))
    openxlsx::freezePane(wb, sheet, firstActiveRow = 4)
    widths <- if (i == 1) c(34,16,41,12,12,12,16,55) else if (i == 2)
      c(23,80,38,46,23,50,36,48,38,16,26,80,13,24) else c(14,23,55,25)
    openxlsx::setColWidths(wb, sheet, seq_along(widths), widths)
    if (i == 1 && nrow(tbl)) {
      rr <- 4:(3+nrow(tbl))
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "0.0%"), rows = rr, cols = 7, gridExpand = TRUE)
      openxlsx::conditionalFormatting(wb, sheet, cols = 6, rows = rr, type = "dataBar", style = "#459A9A")
    }
  }
  openxlsx::saveWorkbook(wb, file.path(stage,"PubMed_review.xlsx"))
  dest <- tempfile(paste0("pubmed_",end,"_"), tmpdir = out)
  if (!file.rename(stage, dest)) stop("Could not commit output")
  message("Wrote ", normalizePath(dest))
  result$directory <- dest
  invisible(result)
}

# NIH RePORTER linkage. Pass the grant and project number columns as strings.
# For example: link_nih(result$grants, projects, "project_num")
# The normalized key keeps activity, institute and serial, stripping application
# type, support year and suffix. A core project can match several award years.
# Use the returned candidate links to inspect awards; never sum award dollars
# across PMIDs or assign a paper to a particular funding year from this key.
link_nih <- function(grants, projects, project_column) {
  stopifnot(length(project_column) == 1, project_column %in% names(projects))
  clean <- function(x) str_replace_all(str_to_upper(x), "[^A-Z0-9]", "")
  core <- function(x) str_extract(clean(x), "[A-Z][0-9A-Z][0-9][A-Z]{2}[0-9]{6}")
  grants |> mutate(core_project = core(grant_id)) |>
    filter(!is.na(core_project)) |>
    inner_join(projects |> mutate(core_project = core(.data[[project_column]])),
               by = "core_project", relationship = "many-to-many")
}
