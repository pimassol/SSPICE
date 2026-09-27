# ============================================================
# SSPICE - MODULE 03: ROUTING CHECK
# ============================================================
#
# Purpose:
#   Validate the prepared record-level data against the routing
#   structure represented by the canonical DAG and its transposed
#   incidence matrix Bt.
#
# Required inputs:
#   output/DAG/questionnaire_DAG.rds
#   output/prepared-data/prepared_questionnaire_data.rds
#   data-raw/metadata/items_mapping.csv
#
# Outputs:
#   output/routing-check/routing_check_results.rds
#   output/routing-check/full_report.csv
#   output/routing-check/summary_by_record.csv
#   output/routing-check/summary_by_variable.csv
#   output/routing-check/execution_time.csv
#
# Important:
#   Run this script with the SSPICE package root as the working
#   directory. The script detects the package root from the current
#   working directory and does not require RStudio or devtools.
#
# ============================================================


# ------------------------------------------------------------
# 1. Verify and load the package under development
# ------------------------------------------------------------

if (!file.exists("DESCRIPTION")) {
  stop(
    "DESCRIPTION was not found. Set the SSPICE package root as the working directory."
  )
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "Package 'pkgload' is required to run this development script."
  )
}

pkgload::load_all(
  path = ".",
  reset = TRUE,
  export_all = FALSE,
  helpers = FALSE,
  quiet = FALSE
)


# ------------------------------------------------------------
# 2. Load project configuration
# ------------------------------------------------------------

config_file <- file.path(
  "scripts",
  "00_config.R"
)

if (!file.exists(config_file)) {
  stop(
    paste0(
      "Configuration file not found: ",
      config_file,
      "."
    )
  )
}

source(config_file)


# ------------------------------------------------------------
# 3. Resolve and verify the required input files
# ------------------------------------------------------------

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

prepared_data_file <- file.path(
  config$prepared_data_output_dir,
  "prepared_questionnaire_data.rds"
)

required_files <- c(
  dag_file,
  prepared_data_file,
  config$items_mapping_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Missing required files: ",
      paste(
        missing_files,
        collapse = ", "
      ),
      "."
    )
  )
}


# ------------------------------------------------------------
# 4. Load the canonical DAG and prepared data
# ------------------------------------------------------------

dag_object <- readRDS(
  dag_file
)

prepared_data <- readRDS(
  prepared_data_file
)

required_dag_elements <- c(
  "routes_numeric",
  "Bt",
  "varnames"
)

missing_dag_elements <- setdiff(
  required_dag_elements,
  names(dag_object)
)

if (length(missing_dag_elements) > 0L) {
  stop(
    paste0(
      "The DAG object is missing required elements: ",
      paste(
        missing_dag_elements,
        collapse = ", "
      ),
      "."
    )
  )
}

if (!identical(
  names(prepared_data),
  dag_object$varnames
)) {
  stop(
    "Prepared data columns are not aligned with the DAG variables."
  )
}

if (!config$record_id %in% names(prepared_data)) {
  stop(
    paste0(
      "Record identifier not found in prepared data: ",
      config$record_id,
      "."
    )
  )
}


# ------------------------------------------------------------
# 5. Create the routing-check output directory
# ------------------------------------------------------------

dir.create(
  config$routing_check_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 6. Run routing validation and measure execution time
# ------------------------------------------------------------

start_time <- Sys.time()

routing_results <- routing_check(
  dset_data = prepared_data,
  routes = dag_object$routes_numeric,
  Bt = dag_object$Bt,
  varkey = config$record_id,
  items.maps = basename(
    config$items_mapping_file
  ),
  path.items.maps = dirname(
    config$items_mapping_file
  )
)

end_time <- Sys.time()

elapsed_seconds <- as.numeric(
  difftime(
    end_time,
    start_time,
    units = "secs"
  )
)

routing_results$elapsed_seconds <- elapsed_seconds
routing_results$started_at <- start_time
routing_results$completed_at <- end_time


# ------------------------------------------------------------
# 7. Save the complete R object
# ------------------------------------------------------------

saveRDS(
  routing_results,
  file = file.path(
    config$routing_check_output_dir,
    "routing_check_results.rds"
  )
)


# ------------------------------------------------------------
# 8. Save detailed and aggregated reports
# ------------------------------------------------------------

utils::write.csv(
  routing_results$full_report,
  file = file.path(
    config$routing_check_output_dir,
    "full_report.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  routing_results$summary_by_rec,
  file = file.path(
    config$routing_check_output_dir,
    "summary_by_record.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  routing_results$summary_by_var,
  file = file.path(
    config$routing_check_output_dir,
    "summary_by_variable.csv"
  ),
  row.names = FALSE,
  na = ""
)

execution_time <- data.frame(
  start_time = as.character(start_time),
  end_time = as.character(end_time),
  elapsed_seconds = elapsed_seconds,
  stringsAsFactors = FALSE
)

utils::write.csv(
  execution_time,
  file = file.path(
    config$routing_check_output_dir,
    "execution_time.csv"
  ),
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# 9. Calculate execution summary
# ------------------------------------------------------------

record_ids_with_errors <- unique(routing_results$full_report[[config$record_id]])

record_ids_with_errors <- record_ids_with_errors[
  !is.na(record_ids_with_errors)
]

variables_with_errors <- unique(
  routing_results$full_report$varname_label
)

variables_with_errors <- variables_with_errors[
  !is.na(variables_with_errors)
]


# ------------------------------------------------------------
# 10. Print result examples and execution summary
# ------------------------------------------------------------

cat(
  "\nSSPICE routing check\n",
  "  Records checked: ",
  nrow(prepared_data),
  "\n  Violations: ",
  nrow(routing_results$full_report),
  "\n  Records with violations: ",
  length(record_ids_with_errors),
  "\n  Variables with violations: ",
  length(variables_with_errors),
  "\n  Elapsed seconds: ",
  round(elapsed_seconds, 2L),
  "\n  Output directory: ",
  config$routing_check_output_dir,
  "\n",
  sep = ""
)

cat(
  "\nExample of routing violations:\n"
)

print(
  head(
    routing_results$full_report
  )
)

cat(
  "\nExample of violations by record:\n"
)

print(
  head(
    routing_results$summary_by_rec
  )
)

cat(
  "\nExample of violations by variable:\n"
)

print(
  head(
    routing_results$summary_by_var
  )
)


# ------------------------------------------------------------
# 11. Verify that every expected output was created
# ------------------------------------------------------------

expected_output_files <- file.path(
  config$routing_check_output_dir,
  c(
    "routing_check_results.rds",
    "full_report.csv",
    "summary_by_record.csv",
    "summary_by_variable.csv",
    "execution_time.csv"
  )
)

stopifnot(
  all(
    file.exists(expected_output_files)
  )
)

cat(
  "\nAll routing-check output files were created successfully.\n"
)
