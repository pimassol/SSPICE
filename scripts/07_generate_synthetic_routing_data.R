# ============================================================
# GENERATE ADMISSIBLE DATA AND CONTROLLED ROUTING VIOLATIONS
# ============================================================
#
# Inputs:
#   output/DAG/questionnaire_DAG.rds
#   data-raw/metadata/items_mapping.csv
#
# Outputs:
#   data-raw/input/synthetic_routing_data.csv
#   data-raw/input/synthetic_data_violations.csv
#   data-raw/input/report_violations_by_record.csv
#   data-raw/input/synthetic_routing_validation.rds
#
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

generate_internal <- getFromNamespace(
  "generate_admissible_records",
  "SSPICE"
)

inject_internal <- getFromNamespace(
  "inject_routing_violations",
  "SSPICE"
)

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

required_files <- c(
  dag_file,
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

n_synthetic_records <- 5000L
violation_proportion <- 0.5
optional_fill_probability <- 0.5
record_id <- config$record_id

if (is.null(record_id) || length(record_id) != 1L || !nzchar(record_id)) {
  stop("config$record_id must contain one non-empty column name.")
}

dag_object <- readRDS(dag_file)

dir.create(
  file.path("data-raw", "input"),
  recursive = TRUE,
  showWarnings = FALSE
)

start_time <- Sys.time()

synthetic_admissible <- generate_internal(
  dag = dag_object,
  n_records = n_synthetic_records,
  id_col = record_id,
  items_mapping_file = config$items_mapping_file,
  seed = config$random_seed,
  optional_fill_probability = optional_fill_probability
)

admissible_check <- routing_check(
  dset_data = synthetic_admissible,
  routes = dag_object$routes_numeric,
  Bt = dag_object$Bt,
  varkey = record_id,
  items.maps = basename(config$items_mapping_file),
  path.items.maps = dirname(config$items_mapping_file)
)

if (nrow(admissible_check$full_report) > 0L) {
  stop(
    "Synthetic generation produced ",
    nrow(admissible_check$full_report),
    " routing violations. No output was saved.",
    call. = FALSE
  )
}

injected <- inject_internal(
  data = synthetic_admissible,
  dag = dag_object,
  id_col = record_id,
  items_mapping_file = config$items_mapping_file,
  proportion = violation_proportion,
  seed = config$random_seed + 1L
)

violated_check <- routing_check(
  dset_data = injected$data,
  routes = dag_object$routes_numeric,
  Bt = dag_object$Bt,
  varkey = record_id,
  items.maps = basename(config$items_mapping_file),
  path.items.maps = dirname(config$items_mapping_file)
)

injected_ids <- injected$ground_truth[
  injected$ground_truth$is_violated,
  record_id,
  drop = TRUE
]

attempted_injections <- length(injected_ids)

detected_ids <- unique(
  violated_check$full_report[[record_id]]
)

undetected_rows <- injected$ground_truth$is_violated &
  !as.character(injected$ground_truth[[record_id]]) %in%
    as.character(detected_ids)

if (any(undetected_rows)) {
  injected$data[undetected_rows, ] <- synthetic_admissible[undetected_rows, ]
  injected$ground_truth$is_violated[undetected_rows] <- FALSE
  injected$ground_truth$injected_var[undetected_rows] <- NA_character_
  injected$ground_truth$injection_type[undetected_rows] <- NA_character_
  injected$ground_truth$original_value[undetected_rows] <- NA_character_
  injected$ground_truth$injected_value[undetected_rows] <- NA_character_
}

injected_ids <- injected$ground_truth[
  injected$ground_truth$is_violated,
  record_id,
  drop = TRUE
]

if (!length(injected_ids)) {
  stop("No detectable routing violation was generated.", call. = FALSE)
}

admissible_file <- file.path(
  "data-raw", "input", "synthetic_routing_data.csv"
)
violated_file <- file.path(
  "data-raw", "input", "synthetic_data_violations.csv"
)
ground_truth_file <- file.path(
  "data-raw", "input", "report_violations_by_record.csv"
)
objects_file <- file.path(
  "data-raw", "input", "synthetic_routing_validation.rds"
)

utils::write.csv(
  synthetic_admissible,
  admissible_file,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  injected$data,
  violated_file,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  injected$ground_truth,
  ground_truth_file,
  row.names = FALSE,
  na = ""
)

end_time <- Sys.time()

results <- list(
  admissible_data = synthetic_admissible,
  violated_data = injected$data,
  ground_truth = injected$ground_truth,
  admissible_routing_check = admissible_check,
  violated_routing_check = violated_check,
  record_id = record_id,
  seed = config$random_seed,
  attempted_injections = attempted_injections,
  retained_injections = length(injected_ids),
  started_at = start_time,
  completed_at = end_time,
  elapsed_seconds = as.numeric(
    difftime(end_time, start_time, units = "secs")
  )
)

saveRDS(results, objects_file)

cat(
  "\nSSPICE synthetic routing validation data\n",
  "  Record identifier: ", record_id,
  "\n  Generated records: ", nrow(synthetic_admissible),
  "\n  Violations before injection: ",
  nrow(admissible_check$full_report),
  "\n  Attempted injections: ", attempted_injections,
  "\n  Detectable injections retained: ", length(injected_ids),
  "\n  Records detected after injection: ", length(detected_ids),
  "\n  Routing violations after injection: ",
  nrow(violated_check$full_report),
  "\n  Elapsed seconds: ", round(results$elapsed_seconds, 2L),
  "\n  Output directory: data-raw/input\n",
  sep = ""
)
