# ============================================================
# STEP 10 - CORRECT ERRONEOUS RECORDS ON THE EXTENDED DAG+
# ============================================================

if (!exists("config", inherits = FALSE)) {
  source(file.path("scripts", "00_config.R"))
}

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

# Step 10 is an operational script, not package code. Internal helpers must
# therefore be resolved explicitly from the SSPICE namespace so that the
# script works in the global environment and in isolated pilot environments.
.sspice_partition_correction_records <- getFromNamespace(
  ".sspice_partition_correction_records",
  "SSPICE"
)
.sspice_validated_donors <- getFromNamespace(
  ".sspice_validated_donors",
  "SSPICE"
)
.sspice_build_donor_cache <- getFromNamespace(
  ".sspice_build_donor_cache",
  "SSPICE"
)
.sspice_recombine_correction_data <- getFromNamespace(
  ".sspice_recombine_correction_data",
  "SSPICE"
)
.sspice_normalize_immutable_input <- getFromNamespace(
  ".sspice_normalize_fixed_variables",
  "SSPICE"
)
.sspice_cached_routing_check <- getFromNamespace(
  "cached_routing_check",
  "SSPICE"
)

config_value <- function(name, default = NULL) {
  if (name %in% names(config)) config[[name]] else default
}

read_metadata <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  utils::read.csv(
    path,
    header = FALSE,
    sep = config$metadata_separator,
    na.strings = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    strip.white = TRUE
  )
}

read_record_data <- function(path) {
  if (tolower(tools::file_ext(path)) == "rds") return(readRDS(path))
  utils::read.csv(
    path,
    sep = config_value("data_separator", ","),
    na.strings = config_value("missing_codes", c("", ".", "NA")),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

extended_dag_dir <- config_value(
  "extended_dag_output_dir",
  file.path("output", "extended-DAG")
)
extended_dag_file <- file.path(
  extended_dag_dir,
  "questionnaire_extended_DAG.rds"
)
extended_data_file <- file.path(
  extended_dag_dir,
  "prepared_data_with_logical_edits.rds"
)
logical_edit_rules_file <- config_value(
  "logical_edit_rules_file",
  file.path("data-raw", "metadata", "logical_edit_rules.csv")
)
vaux_rules_file <- config_value(
  "vaux_file",
  file.path("data-raw", "metadata", "vaux_rules.csv")
)
output_directory <- config_value(
  "correction_output_dir",
  file.path("output", "corrected-data")
)

required_files <- c(
  extended_dag_file,
  extended_data_file,
  logical_edit_rules_file,
  vaux_rules_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Missing files required by Step 10: ",
    paste(missing_files, collapse = ", "),
    ".",
    call. = FALSE
  )
}

extended_dag <- readRDS(extended_dag_file)
extended_data <- readRDS(extended_data_file)
logical_edit_metadata <- read_metadata(logical_edit_rules_file)
vaux_metadata <- read_metadata(vaux_rules_file)

if (!identical(names(extended_data), extended_dag$varnames)) {
  stop("Step 10 data columns are not aligned with DAG+ variables.",
       call. = FALSE)
}

record_id <- config_value("record_id", extended_dag$record_id)
if (is.null(record_id) || length(record_id) != 1L ||
    !record_id %in% names(extended_data)) {
  stop("Step 10 requires a valid record_id in config or DAG+.",
       call. = FALSE)
}

correction_strata <- unique(as.character(
  config_value("correction_strata", character())
))
correction_strata <- correction_strata[
  !is.na(correction_strata) & nzchar(correction_strata)
]
missing_strata <- setdiff(correction_strata, names(extended_data))
if (length(missing_strata)) {
  stop(
    "Correction strata are missing from the data: ",
    paste(missing_strata, collapse = ", "),
    ".",
    call. = FALSE
  )
}

record_limit <- config_value("correction_record_limit", Inf)
if (!is.numeric(record_limit) || length(record_limit) != 1L ||
    is.na(record_limit) || record_limit <= 0) {
  stop("correction_record_limit must be positive or Inf.", call. = FALSE)
}

# 1. One complete initial check and immutable partition.
cached_check_file <- config_value(
  "correction_initial_check_file",
  config_value(
    "correction_donor_check_file",
    file.path(
      "output",
      "extended-routing-check",
      "extended_routing_check_results.rds"
    )
  )
)

cache_policy <- config_value("correction_cache_policy", "auto")
mapping_file <- config_value("items_mapping_file", NULL)
cache_input_files <- c(
  extended_dag = extended_dag_file,
  extended_data = extended_data_file,
  routing_metadata = config_value("routing_file", NULL),
  vaux_metadata = vaux_rules_file,
  logical_edit_metadata = logical_edit_rules_file,
  items_mapping = mapping_file
)
cache_input_files <- cache_input_files[
  !is.na(cache_input_files) & nzchar(cache_input_files)
]
initial_check_artifact <- .sspice_cached_routing_check(
  data = extended_data,
  dag = extended_dag,
  record_id = record_id,
  logical_edit_names = extended_dag$logical_edit_names,
  items_mapping_file = mapping_file,
  cache_file = cached_check_file,
  input_files = cache_input_files,
  cache_policy = cache_policy
)
initial_check <- initial_check_artifact$result

partition <- .sspice_partition_correction_records(
  data = extended_data,
  full_report = initial_check$full_report,
  record_id = record_id
)
exact_records <- partition$exact_records
erroneous_records <- partition$erroneous_records

# The limit is applied only after the EXACT/ERRONEOUS partition.
selected_erroneous_positions <- if (is.finite(record_limit)) {
  seq_len(min(nrow(erroneous_records), as.integer(record_limit)))
} else {
  seq_len(nrow(erroneous_records))
}
erroneous_records_to_correct <- erroneous_records[
  selected_erroneous_positions,
  ,
  drop = FALSE
]
selected_original_indices <- partition$erroneous_indices[
  selected_erroneous_positions
]

# 2. Immutable donor pool and one-time cache.
donor_file <- config_value("correction_donor_data_file", NULL)
if (is.null(donor_file)) {
  donor_data <- exact_records
  donor_data_validated <- TRUE
  donor_origin <- "original_exact_records"
} else {
  if (!file.exists(donor_file)) {
    stop("Configured donor file does not exist: ", donor_file, ".",
         call. = FALSE)
  }
  donor_data <- read_record_data(donor_file)
  donor_data_validated <- isTRUE(
    config_value("correction_donor_data_validated", FALSE)
  )
  donor_origin <- "external_donor_file"
}

validated_donors <- .sspice_validated_donors(
  donor_data = donor_data,
  dag = extended_dag,
  record_id = record_id,
  items_mapping_file = config_value("items_mapping_file", NULL),
  already_validated = donor_data_validated
)
donor_data <- validated_donors$data
donor_cache <- .sspice_build_donor_cache(
  donor_data = donor_data,
  dag = extended_dag,
  record_id = record_id,
  strata = correction_strata,
  immutable = TRUE
)

prior_file <- config_value("correction_priors_file", NULL)
domain_file <- config_value("correction_domains_file", NULL)
imputation_priors <- if (is.null(prior_file)) NULL else readRDS(prior_file)
correction_domains <- if (is.null(domain_file)) NULL else readRDS(domain_file)

control_arguments <- config_value("correction_control_args", list())
if (!is.list(control_arguments)) {
  stop("correction_control_args must be a list.", call. = FALSE)
}
batch_size <- config_value("correction_batch_size", 100L)
resume_batches <- config_value("correction_resume_batches", TRUE)
control_arguments <- utils::modifyList(
  control_arguments,
  list(
    batch_size = batch_size,
    resume_batches = resume_batches
  )
)
control <- do.call(
  getExportedValue("SSPICE", "correction_control"),
  control_arguments
)
if ("correction_fixed_variables" %in% names(config)) {
  stop(
    "correction_fixed_variables has been replaced by ",
    "correction_immutable_variables. Rename the configuration field.",
    call. = FALSE
  )
}
immutable_variables <- config_value("correction_immutable_variables", NULL)
imputation_method <- config_value("correction_imputation_method", "donor")

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
batch_output_directory <- file.path(output_directory, "batches")
started_at <- Sys.time()

# 3. Correct only the selected originally erroneous records.
if (nrow(erroneous_records_to_correct)) {
  correction_result <- getExportedValue("SSPICE", "correct_data")(
    data = erroneous_records_to_correct,
    dag = extended_dag,
    record_id = record_id,
    donor_data = donor_data,
    donor_data_validated = TRUE,
    donor_cache = donor_cache,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    immutable_variables = immutable_variables,
    strata = correction_strata,
    imputation_method = imputation_method,
    imputation_priors = imputation_priors,
    domains = correction_domains,
    external_data_dir = config_value("external_data_dir", "."),
    items_mapping_file = config_value("items_mapping_file", NULL),
    control = control,
    batch_output_dir = batch_output_directory
  )
} else {
  correction_result <- structure(
    list(
      corrected_data = erroneous_records_to_correct,
      record_summary = data.frame(),
      unresolved_records = data.frame(),
      donor_diagnostics = validated_donors[c("supplied", "retained")],
      control = control,
      immutable_variables = unique(c(
        .sspice_normalize_immutable_input(
          immutable_variables,
          names(extended_data)
        ),
        record_id,
        if (control$freeze_strata) correction_strata else character()
      )),
      recalculable_variables = character(),
      mode_locked_vaux = character(),
      strata = correction_strata,
      imputation_method = imputation_method,
      parallel = list(
        requested = isTRUE(control$use_parallel),
        workers_used = 1L,
        batch_size = batch_size,
        batches_total = 0L,
        batches_reused = 0L,
        checkpoint_directory = batch_output_directory
      )
    ),
    class = "sspice_correction_result"
  )
}

# 4. Recombine in original row order.
corrected_questionnaire_data <- .sspice_recombine_correction_data(
  original_data = extended_data,
  corrected_records = correction_result$corrected_data,
  corrected_indices = selected_original_indices
)

record_status <- data.frame(
  record_index = seq_len(nrow(extended_data)),
  record_identifier = extended_data[[record_id]],
  initial_status = ifelse(
    seq_len(nrow(extended_data)) %in% partition$exact_indices,
    "EXACT",
    "ERRONEOUS"
  ),
  final_status = ifelse(
    seq_len(nrow(extended_data)) %in% partition$exact_indices,
    "EXACT",
    "NOT_PROCESSED"
  ),
  stringsAsFactors = FALSE
)
names(record_status)[2L] <- record_id
if (length(selected_original_indices)) {
  record_status$final_status[selected_original_indices] <- ifelse(
    correction_result$record_summary$resolved,
    "CORRECTED",
    "UNRESOLVED"
  )
}

completed_at <- Sys.time()
elapsed_seconds <- as.numeric(
  difftime(completed_at, started_at, units = "secs")
)

run_summary <- data.frame(
  records_available = nrow(extended_data),
  records_exact = nrow(exact_records),
  records_erroneous = nrow(erroneous_records),
  records_processed = nrow(erroneous_records_to_correct),
  records_resolved = if (nrow(correction_result$record_summary)) {
    sum(correction_result$record_summary$resolved)
  } else 0L,
  records_unresolved = nrow(correction_result$unresolved_records),
  records_not_processed = nrow(erroneous_records) -
    nrow(erroneous_records_to_correct),
  donors_supplied = validated_donors$supplied,
  donors_retained = validated_donors$retained,
  donor_origin = donor_origin,
  donor_pool_immutable = TRUE,
  strata = paste(correction_strata, collapse = " | "),
  immutable_variables = paste(
    correction_result$immutable_variables,
    collapse = " | "
  ),
  recalculable_variables = paste(
    correction_result$recalculable_variables,
    collapse = " | "
  ),
  mode_locked_vaux = paste(
    correction_result$mode_locked_vaux,
    collapse = " | "
  ),
  imputation_method = paste(imputation_method, collapse = " | "),
  parallel_requested = isTRUE(correction_result$parallel$requested),
  parallel_workers = if (is.null(correction_result$parallel$workers_used)) {
    1L
  } else {
    correction_result$parallel$workers_used
  },
  batch_size = if (is.null(correction_result$parallel$batch_size)) {
    batch_size
  } else correction_result$parallel$batch_size,
  batches_total = if (is.null(correction_result$parallel$batches_total)) {
    0L
  } else correction_result$parallel$batches_total,
  batches_reused = if (is.null(correction_result$parallel$batches_reused)) {
    0L
  } else correction_result$parallel$batches_reused,
  resume_batches = resume_batches,
  routing_cache_policy = initial_check_artifact$cache$policy,
  routing_cache_used = initial_check_artifact$cache$used,
  routing_cache_refreshed = initial_check_artifact$cache$refreshed,
  routing_cache_reason = initial_check_artifact$cache$reason,
  final_temperature = control$final_temperature,
  cooling_fraction = control$cooling_fraction,
  elapsed_seconds = elapsed_seconds,
  stringsAsFactors = FALSE
)

# 5. Save complete outputs.
saveRDS(correction_result, file.path(output_directory, "correction_result.rds"))
saveRDS(corrected_questionnaire_data,
        file.path(output_directory, "corrected_questionnaire_data.rds"))
saveRDS(exact_records, file.path(output_directory, "exact_records.rds"))
saveRDS(erroneous_records, file.path(output_directory, "erroneous_records.rds"))

utils::write.csv(corrected_questionnaire_data,
                 file.path(output_directory, "corrected_questionnaire_data.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(exact_records,
                 file.path(output_directory, "exact_records.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(erroneous_records,
                 file.path(output_directory, "erroneous_records.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(correction_result$record_summary,
                 file.path(output_directory, "correction_record_summary.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(correction_result$unresolved_records,
                 file.path(output_directory, "unresolved_records.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(record_status,
                 file.path(output_directory, "correction_record_status.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(run_summary,
                 file.path(output_directory, "correction_run_summary.csv"),
                 row.names = FALSE, na = "")
if (!is.null(correction_result$batch_manifest)) {
  utils::write.csv(
    correction_result$batch_manifest,
    file.path(output_directory, "correction_batch_manifest.csv"),
    row.names = FALSE,
    na = ""
  )
}

cat(
  "\nSSPICE Step 10 data correction\n",
  "  Records available: ", run_summary$records_available,
  "\n  Exact records: ", run_summary$records_exact,
  "\n  Erroneous records: ", run_summary$records_erroneous,
  "\n  Erroneous records processed: ", run_summary$records_processed,
  "\n  Records resolved: ", run_summary$records_resolved,
  "\n  Records unresolved: ", run_summary$records_unresolved,
  "\n  Erroneous records not processed: ",
  run_summary$records_not_processed,
  "\n  Immutable donors retained: ", run_summary$donors_retained,
  "\n  Strata: ", run_summary$strata,
  "\n  Parallel correction: ", run_summary$parallel_requested,
  "\n  Parallel workers: ", run_summary$parallel_workers,
  "\n  Batch size: ", run_summary$batch_size,
  "\n  Batches completed: ", run_summary$batches_total,
  "\n  Batches reused: ", run_summary$batches_reused,
  "\n  Routing cache policy: ", run_summary$routing_cache_policy,
  "\n  Routing cache used: ", run_summary$routing_cache_used,
  "\n  Routing cache refreshed: ", run_summary$routing_cache_refreshed,
  "\n  Routing cache reason: ", run_summary$routing_cache_reason,
  "\n  Elapsed seconds: ", round(elapsed_seconds, 2L),
  "\n  Output directory: ", output_directory,
  "\n",
  sep = ""
)

print(run_summary, row.names = FALSE)
