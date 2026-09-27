# ============================================================
# SSPICE - MODULE 04: CREATE VARDOM AND RULES
# ============================================================
#
# Purpose:
#   Generate variable-domain definitions, the fixed-width record
#   layout and formal compatibility rules from the canonical DAG.
#
# Required inputs:
#   output/DAG/questionnaire_DAG.rds
#   data-raw/metadata/items_mapping.csv
#
# Main outputs:
#   output/rules/VARDOM.dat
#   output/rules/RULES.dat
#   output/rules/record_layout.csv
#
# Additional diagnostic outputs:
#   output/rules/vardom.csv
#   output/rules/rules_objects.rds
#
# Important:
#   STRUTT.dat and LISTE.dat are not generated because SSPICE
#   represents logical groups as canonical VOR and VXOR variables.
#
#   Run this script with the SSPICE package root as the working
#   directory. RStudio and devtools are not required.
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
# 3. Resolve and verify required inputs
# ------------------------------------------------------------

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

required_files <- c(
  dag_file,
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
# 4. Load and validate the canonical DAG
# ------------------------------------------------------------

dag_object <- readRDS(
  dag_file
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
  colnames(dag_object$Bt),
  dag_object$varnames
)) {
  stop(
    "The columns of Bt are not aligned with the DAG variables."
  )
}


# ------------------------------------------------------------
# 5. Create the output directory
# ------------------------------------------------------------

dir.create(
  config$rules_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 6. Retrieve the internal generators
# ------------------------------------------------------------

create_vardom_internal <- getFromNamespace(
  "create_vardom",
  "SSPICE"
)

create_rules_internal <- getFromNamespace(
  "create_rules",
  "SSPICE"
)


# ------------------------------------------------------------
# 7. Generate VARDOM and fixed-width record layout
# ------------------------------------------------------------

vardom_result <- create_vardom_internal(
  routes_df = dag_object$routes_numeric,
  Bt = dag_object$Bt,
  items.maps = basename(
    config$items_mapping_file
  ),
  path.items.maps = dirname(
    config$items_mapping_file
  ),
  output.path = config$rules_output_dir
)


# ------------------------------------------------------------
# 8. Generate formal compatibility rules
# ------------------------------------------------------------

rules <- create_rules_internal(
  Bt = dag_object$Bt,
  routes_df = dag_object$routes_numeric,
  items.maps = basename(
    config$items_mapping_file
  ),
  path.items.maps = dirname(
    config$items_mapping_file
  ),
  path_output = config$rules_output_dir
)


# ------------------------------------------------------------
# 9. Save diagnostic tabular and R outputs
# ------------------------------------------------------------

utils::write.csv(
  vardom_result$vardom,
  file = file.path(
    config$rules_output_dir,
    "vardom.csv"
  ),
  row.names = FALSE,
  na = ""
)

rules_objects <- list(
  vardom = vardom_result$vardom,
  record_layout = vardom_result$record_layout,
  rules = rules,
  routes = dag_object$routes_numeric,
  Bt = dag_object$Bt,
  varnames = dag_object$varnames
)

saveRDS(
  rules_objects,
  file = file.path(
    config$rules_output_dir,
    "rules_objects.rds"
  )
)


# ------------------------------------------------------------
# 10. Verify the generated outputs
# ------------------------------------------------------------

expected_output_files <- file.path(
  config$rules_output_dir,
  c(
    "VARDOM.dat",
    "RULES.dat",
    "record_layout.csv",
    "vardom.csv",
    "rules_objects.rds"
  )
)

stopifnot(
  all(
    file.exists(expected_output_files)
  ),
  is.list(vardom_result),
  all(
    c(
      "vardom",
      "record_layout"
    ) %in% names(vardom_result)
  ),
  nrow(vardom_result$vardom) == length(dag_object$varnames),
  nrow(vardom_result$record_layout) == length(dag_object$varnames),
  identical(
    vardom_result$record_layout$varname,
    dag_object$varnames
  ),
  length(rules) > 0L,
  identical(
    rules,
    readLines(
      file.path(
        config$rules_output_dir,
        "RULES.dat"
      ),
      warn = FALSE
    )
  )
)


# ------------------------------------------------------------
# 11. Report legacy files without deleting them
# ------------------------------------------------------------

legacy_files <- file.path(
  config$rules_output_dir,
  c(
    "STRUTT.dat",
    "LISTE.dat",
    "REGOLE.dat",
    "strutt.csv"
  )
)

legacy_files_present <- legacy_files[
  file.exists(legacy_files)
]

if (length(legacy_files_present) > 0L) {
  warning(
    paste0(
      "Legacy files are still present but were not generated or updated: ",
      paste(
        legacy_files_present,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 12. Print examples and execution summary
# ------------------------------------------------------------

cat(
  "\nSSPICE VARDOM and rules generation\n",
  "  DAG variables: ",
  length(dag_object$varnames),
  "\n  VARDOM rows: ",
  nrow(vardom_result$vardom),
  "\n  Record-layout rows: ",
  nrow(vardom_result$record_layout),
  "\n  Compatibility rules: ",
  length(rules),
  "\n  Output directory: ",
  config$rules_output_dir,
  "\n",
  sep = ""
)

cat(
  "\nExample of VARDOM:\n"
)

print(
  head(
    vardom_result$vardom,
    10L
  )
)

cat(
  "\nExample of fixed-width record layout:\n"
)

print(
  head(
    vardom_result$record_layout,
    10L
  )
)

cat(
  "\nFirst 20 compatibility rules:\n"
)

cat(
  head(
    rules,
    20L
  ),
  sep = "\n"
)

cat(
  "\n\nAll VARDOM and RULES output files were created successfully.\n"
)
