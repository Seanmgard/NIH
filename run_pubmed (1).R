# PubMed pull | Open this file in RStudio and click Source.
# Keep pubmed_research.R and PubMed_Search_Inputs.xlsx in project_folder.
# Edit keywords in Excel and save the workbook before running.

# SETTINGS --------------------------------------------------------------------
project_folder <- "."  # Your working directory, or e.g. "C:/Research/PubMed"
input_workbook <- file.path(project_folder, "PubMed_Search_Inputs.xlsx")
output_folder <- file.path(project_folder, "pubmed_output")

end_date <- Sys.Date() - 1     # Yesterday, or e.g. as.Date("2026-11-20")
days_per_period <- 28L        # Compare the latest 28 days with the preceding 28

ncbi_email <- Sys.getenv("NCBI_EMAIL")     # Or enter your email in quotes
ncbi_api_key <- Sys.getenv("NCBI_API_KEY") # Or enter your key locally in quotes
# An empty API key uses the slower public-access rate.

# RUN -------------------------------------------------------------------------
workflow_script <- file.path(project_folder, "pubmed_research.R")
required_files <- c(workflow_script, input_workbook)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Check project_folder and input_workbook. Missing file(s): ",
       paste(missing_files, collapse = ", "), call. = FALSE)
}

packages <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "readxl",
              "httr", "jsonlite", "xml2", "openxlsx")
missing_packages <- packages[!vapply(packages, requireNamespace,
                                    logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install the missing packages once, then rerun:\ninstall.packages(c(",
       paste(shQuote(missing_packages), collapse = ", "), "))", call. = FALSE)
}

source(workflow_script, local = TRUE)
result <- run_pubmed(
  panel = input_workbook,
  end = end_date,
  days = days_per_period,
  out = output_folder,
  email = ncbi_email,
  key = ncbi_api_key
)

# The results are also available in R as result$trends, result$papers and
# result$grants. result$directory gives the saved report folder.
