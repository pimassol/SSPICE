if (!exists("config", inherits = FALSE)) {
  sys.source(file.path("scripts", "00_config.R"), envir = environment())
}

quality_config_value <- function(name, default = NULL) {
  value <- config[[name]]
  if (is.null(value)) default else value
}

correction_directory <- quality_config_value(
  "correction_output_dir",
  file.path("output", "corrected-data")
)
dag_directory <- quality_config_value(
  "extended_dag_output_dir",
  file.path("output", "extended-DAG")
)
quality_output_directory <- quality_config_value(
  "correction_quality_output_dir",
  file.path(correction_directory, "quality-report")
)

required_files <- c(
  result = file.path(correction_directory, "correction_result.rds"),
  corrected = file.path(correction_directory, "corrected_questionnaire_data.rds"),
  original = file.path(dag_directory, "prepared_data_with_logical_edits.rds"),
  dag = file.path(dag_directory, "questionnaire_extended_DAG.rds")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Missing Step 11 input files:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

correction_result <- readRDS(required_files[["result"]])
corrected_data <- readRDS(required_files[["corrected"]])
original_data <- readRDS(required_files[["original"]])
dag <- readRDS(required_files[["dag"]])

record_id <- quality_config_value("record_id", dag$record_id)
if (is.null(record_id)) record_id <- names(correction_result$record_summary)[1L]

exact_file <- file.path(correction_directory, "exact_records.rds")
donor_data <- if (file.exists(exact_file)) readRDS(exact_file) else NULL

initial_check <- NULL
initial_check_file <- quality_config_value("correction_initial_check_file", NULL)
if (!is.null(initial_check_file) && file.exists(initial_check_file)) {
  initial_check <- readRDS(initial_check_file)
}

cached_initial_check_file <- file.path(
  quality_output_directory,
  "initial_routing_check.rds"
)
if (is.null(initial_check) && file.exists(cached_initial_check_file)) {
  initial_check <- readRDS(cached_initial_check_file)
}

if (is.null(initial_check)) {
  mapping_file <- quality_config_value("items_mapping_file", NULL)
  initial_check <- routing_check(
    dset_data = original_data,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = record_id,
    items.maps = if (is.null(mapping_file)) NULL else basename(mapping_file),
    path.items.maps = if (is.null(mapping_file)) NULL else dirname(mapping_file),
    logical_edit_names = dag$logical_edit_names
  )
  dir.create(quality_output_directory, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    initial_check,
    file.path(quality_output_directory, "initial_routing_check.rds")
  )
}

run_summary_file <- file.path(correction_directory, "correction_run_summary.csv")
elapsed_seconds <- NA_real_
donor_origin <- NA_character_
if (file.exists(run_summary_file)) {
  run_summary <- utils::read.csv(
    run_summary_file, stringsAsFactors = FALSE, check.names = FALSE
  )
  if ("elapsed_seconds" %in% names(run_summary) && nrow(run_summary)) {
    elapsed_seconds <- as.numeric(run_summary$elapsed_seconds[1L])
  }
  if ("donor_origin" %in% names(run_summary) && nrow(run_summary)) {
    donor_origin <- as.character(run_summary$donor_origin[1L])
  }
}

if (is.na(donor_origin) || !nzchar(donor_origin)) {
  donor_origin <- correction_result$donor_diagnostics$origin %||%
    correction_result$donor_origin %||%
    if (file.exists(exact_file)) "original_exact_records" else NA_character_
}

if (is.na(elapsed_seconds)) {
  elapsed_seconds <- correction_result$elapsed_seconds %||% NA_real_
}

quality_report <- correction_quality_report(
  original_data = original_data,
  corrected_data = corrected_data,
  correction_result = correction_result,
  dag = dag,
  initial_check = initial_check,
  donor_data = donor_data,
  record_id = record_id,
  output_dir = quality_output_directory,
  elapsed_seconds = elapsed_seconds,
  donor_origin = donor_origin
)

print(quality_report)
cat("\nQuality report directory: ", quality_output_directory, "\n", sep = "")
