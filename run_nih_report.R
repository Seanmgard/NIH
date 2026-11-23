# Open this file in RStudio and click Source. Set workflow_dir once for your team.
workflow_dir <- "."  # Folder containing these scripts and the input workbook, e.g. "C:/Research/NIH"
input_file   <- file.path(workflow_dir, "NIH_Search_Inputs.xlsx")
output_file  <- file.path(workflow_dir, "NIH_Results", "NIH_Report.xlsx")

source(file.path(workflow_dir, "nih_research_workflow.R"), encoding = "UTF-8")

# Pull settings. Edit keywords and aliases in NIH_Search_Inputs.xlsx.
settings <- nih_pull_settings(
  report_start_fy = 2024,
  report_end_fy   = 2027,
  history_start_fy = 2022,           # Lookback for first-observed institutions
  period_mode     = "YTD",           # YTD or FULL_FY (completed years)
  through_mmdd    = "AUTO",          # Today's month/day or a fixed cutoff, e.g. "11-21"
  search_fields   = "Title + abstract", # Also accepts "Title" or "All text"
  activity_codes  = "ALL",           # Or c("R01", "R21")
  admin_institutes = "ALL",          # Or c("NCI", "NHGRI")
  include_s10     = TRUE,            # Shared instrument grants
  include_cash    = TRUE             # Treasury monthly NIH outlays
)

result <- input_file |>
  read_nih_inputs(settings) |>
  collect_nih_research() |>
  save_nih_excel(output_file)
