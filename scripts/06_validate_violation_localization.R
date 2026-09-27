# ============================================================
# LOCALIZE AND, WHEN POSSIBLE, VALIDATE ROUTING VIOLATIONS
# ============================================================
#
# Required inputs:
#   output/DAG/questionnaire_DAG.rds
#   output/prepared-data/prepared_questionnaire_data.rds
#   data-raw/metadata/items_mapping.csv
#
# Optional input:
#   the ground-truth report configured in 00_config.R
#
# Outputs are written to output/localization-validation.
# Run this script with PROJECT as the working directory.
# ============================================================

source(file.path("scripts", "00_config.R"))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required during package development.")
}

pkgload::load_all(
  path = ".",
  reset = TRUE,
  export_all = FALSE,
  helpers = FALSE,
  quiet = FALSE
)

localize_internal <- getFromNamespace(
  "localize_routing_violations",
  "SSPICE"
)

validate_internal <- getFromNamespace(
  "validate_violation_localization",
  "SSPICE"
)

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

prepared_data_file <- file.path(
  config$prepared_data_output_dir,
  "prepared_questionnaire_data.rds"
)

routing_results_file <- file.path(
  config$routing_check_output_dir,
  "routing_check_results.rds"
)

required_files <- c(
  dag_file,
  prepared_data_file,
  config$items_mapping_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Missing required files: ",
      paste(missing_files, collapse = ", "),
      "."
    )
  )
}

dag_object <- readRDS(dag_file)
prepared_data <- readRDS(prepared_data_file)
record_id <- config$record_id

if (is.null(record_id) || length(record_id) != 1L || !nzchar(record_id)) {
  stop("config$record_id must contain one non-empty column name.")
}

if (!record_id %in% names(prepared_data)) {
  stop(
    paste0(
      "The record identifier '",
      record_id,
      "' is absent from the prepared data."
    )
  )
}

if (!identical(names(prepared_data), dag_object$varnames)) {
  stop("Prepared data columns are not aligned with the DAG variables.")
}

read_delimited_auto <- function(path, na_strings) {
  first_line <- readLines(path, n = 1L, warn = FALSE)
  separators <- c(";", ",", "\t")
  counts <- vapply(
    separators,
    function(separator) {
      lengths(
        regmatches(
          first_line,
          gregexpr(separator, first_line, fixed = TRUE)
        )
      )
    },
    integer(1L)
  )

  utils::read.csv(
    path,
    sep = separators[which.max(counts)],
    na.strings = na_strings,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

ground_truth_report <- NULL

if (!is.null(config$violation_report_file) &&
    length(config$violation_report_file) == 1L &&
    nzchar(config$violation_report_file) &&
    file.exists(config$violation_report_file)) {
  candidate_report <- read_delimited_auto(
    config$violation_report_file,
    config$missing_codes
  )

  if (record_id %in% names(candidate_report)) {
    ground_truth_report <- candidate_report
  } else {
    warning(
      "The ground-truth report was ignored because it does not contain '",
      record_id,
      "'."
    )
  }
}

dir.create(
  config$localization_validation_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

start_time <- Sys.time()

routing_results <- if (file.exists(routing_results_file)) {
  readRDS(routing_results_file)
} else {
  NULL
}

localization_results <- localize_internal(
  data = prepared_data,
  dag = dag_object,
  id_col = record_id,
  items_mapping_file = config$items_mapping_file,
  routing_results = routing_results
)

validation_results <- validate_internal(
  localization = localization_results,
  report = ground_truth_report,
  id_col = record_id
)

end_time <- Sys.time()
elapsed_seconds <- as.numeric(
  difftime(end_time, start_time, units = "secs")
)

utils::write.csv(
  localization_results$detail,
  file.path(
    config$localization_validation_output_dir,
    "localization_detail.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  localization_results$summary_by_record,
  file.path(
    config$localization_validation_output_dir,
    "localization_by_record.csv"
  ),
  row.names = FALSE,
  na = ""
)

if (isTRUE(validation_results$ground_truth_available)) {
  utils::write.csv(
    validation_results$comparison_by_record,
    file.path(
      config$localization_validation_output_dir,
      "ground_truth_comparison.csv"
    ),
    row.names = FALSE,
    na = ""
  )

  utils::write.csv(
    validation_results$metrics,
    file.path(
      config$localization_validation_output_dir,
      "localization_metrics.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}

results <- list(
  localization = localization_results,
  validation = validation_results,
  record_id = record_id,
  reused_routing_check = !is.null(routing_results),
  started_at = start_time,
  completed_at = end_time,
  elapsed_seconds = elapsed_seconds
)

saveRDS(
  results,
  file.path(
    config$localization_validation_output_dir,
    "localization_validation_results.rds"
  )
)

cat(
  "\nSSPICE violation localization\n",
  "  Record identifier: ", record_id,
  "\n  Records analyzed: ", nrow(prepared_data),
  "\n  Localized violations: ", nrow(localization_results$detail),
  "\n  Records classified as inconsistent: ",
  sum(localization_results$summary_by_record$predicted_inconsistent),
  "\n  Ground truth available: ",
  validation_results$ground_truth_available,
  "\n  Existing routing check reused: ",
  !is.null(routing_results),
  "\n  Elapsed seconds: ", round(elapsed_seconds, 2L),
  "\n  Output directory: ",
  config$localization_validation_output_dir,
  "\n",
  sep = ""
)

if (isTRUE(validation_results$ground_truth_available)) {
  print(validation_results$metrics, row.names = FALSE)
} else {
  cat("Ground-truth metrics were not calculated.\n")
}
