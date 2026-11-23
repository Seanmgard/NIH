# Open this file in RStudio and click Source. Set workflow_dir once for your team.
workflow_dir <- "."  # Folder containing these scripts and the input workbook, e.g. "C:/Research/NIH"
input_file   <- file.path(workflow_dir, "NIH_Search_Inputs.xlsx")
output_file  <- file.path(workflow_dir, "NIH_Results", "NIH_Report.xlsx")

source(file.path(workflow_dir, "nih_research_workflow.R"), encoding = "UTF-8")

# Edit keywords and fiscal years in Excel, save, then rerun.
result <- input_file |>
  read_nih_inputs() |>
  collect_nih_research() |>
  save_nih_excel(output_file)
