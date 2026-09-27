# ============================================================
# SSPICE - 02 PREPARE DATA
# ============================================================

source(
  file.path(
    "scripts",
    "00_config.R"
  )
)

if (
  file.exists("DESCRIPTION") &&
  requireNamespace("pkgload", quietly = TRUE)
) {
  pkgload::load_all(
    path = ".",
    reset = TRUE,
    export_all = FALSE,
    helpers = FALSE,
    quiet = TRUE
  )
} else {
  suppressPackageStartupMessages(
    library(SSPICE)
  )
}

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

required_files <- c(
  config$questionnaire_data_file,
  config$vaux_file,
  dag_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Required files not found:\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

if (!dir.exists(config$external_data_dir)) {
  stop(
    paste0(
      "External data directory not found: ",
      config$external_data_dir
    )
  )
}

dir.create(
  config$prepared_data_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dag_object <- readRDS(
  dag_file
)

prepared_object <- prepare_data(
  data = config$questionnaire_data_file,
  dag = dag_object,
  vaux = config$vaux_file,
  external_data_dir = config$external_data_dir,
  data_separator = config$data_separator,
  metadata_separator = config$metadata_separator,
  na_strings = config$missing_codes
)

final_data_clean <- prepared_object$data

saveRDS(
  final_data_clean,
  file.path(
    config$prepared_data_output_dir,
    "prepared_questionnaire_data.rds"
  )
)

utils::write.csv(
  final_data_clean,
  file.path(
    config$prepared_data_output_dir,
    "prepared_questionnaire_data.csv"
  ),
  row.names = FALSE,
  na = ""
)

data_preparation_objects <- list(
  data_original = prepared_object$data_original,
  vaux_metadata = prepared_object$vaux_metadata,
  data_with_VAUX = prepared_object$data_with_VAUX,
  data_with_VOR = prepared_object$data_with_VOR,
  data_with_VOR_and_VXOR =
    prepared_object$data_with_VOR_and_VXOR,
  data_clean = final_data_clean
)

saveRDS(
  data_preparation_objects,
  file.path(
    config$prepared_data_output_dir,
    "data_preparation_objects.rds"
  )
)

print(prepared_object)

cat(
  "  Aligned with DAG:",
  identical(names(final_data_clean), dag_object$varnames), "\n",
  "  Output directory:", config$prepared_data_output_dir, "\n"
)
