# ============================================================
# VALIDATE THE HORN THEORY INDUCED BY THE SSPICE ROUTING DAG
# ============================================================
#
# Inputs:
#   output/DAG/questionnaire_DAG.rds
#   output/rules/RULES.dat
#   data-raw/metadata/items_mapping.csv
#
# Outputs are written to output/horn-validation.
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

dag_file <- file.path(
  config$dag_output_dir,
  "questionnaire_DAG.rds"
)

rules_file <- file.path(
  config$rules_output_dir,
  "RULES.dat"
)

required_files <- c(
  dag_file,
  rules_file,
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

dir.create(
  config$horn_validation_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

start_time <- Sys.time()

horn_results <- validate_horn_theory(
  dag = dag_object,
  rules_file = rules_file,
  items_mapping_file = config$items_mapping_file,
  n_random = config$n_random_closure_tests,
  max_seed_size = config$max_seed_size,
  seed = config$random_seed,
  add_bridges = config$add_value_to_filled_bridges
)

end_time <- Sys.time()

horn_results$started_at <- start_time
horn_results$completed_at <- end_time
horn_results$elapsed_seconds <- as.numeric(
  difftime(end_time, start_time, units = "secs")
)

output_tables <- list(
  horn_summary = horn_results$summary,
  canonical_tests = horn_results$canonical_tests,
  expanded_modal_items = horn_results$expanded_items,
  elementary_horn_rules = horn_results$elementary_rules,
  horn_closure = horn_results$complete_rule_set,
  closure_tests = horn_results$closure_tests,
  closure_failures = horn_results$closure_failures,
  minimality_deletion_tests = horn_results$minimality_tests,
  stored_rule_comparison = horn_results$stored_rule_comparison
)

for (table_name in names(output_tables)) {
  utils::write.csv(
    output_tables[[table_name]],
    file = file.path(
      config$horn_validation_output_dir,
      paste0(table_name, ".csv")
    ),
    row.names = FALSE,
    na = ""
  )
}

saveRDS(
  horn_results,
  file = file.path(
    config$horn_validation_output_dir,
    "horn_validation_results.rds"
  )
)

summary_value <- function(measure) {
  horn_results$summary$value[
    horn_results$summary$measure == measure
  ]
}

cat(
  "\nSSPICE Horn-theory validation\n",
  "  Canonical tests passed: ",
  summary_value("canonical_tests_passed"),
  "\n  Closure tests passed: ",
  summary_value("closure_tests_passed"),
  "\n  Minimality deletion tests passed: ",
  summary_value("minimality_deletion_passed"),
  "\n  RULES.dat exactly reproduced: ",
  summary_value("stored_rules_exactly_equal"),
  "\n  Closure failures: ",
  nrow(horn_results$closure_failures),
  "\n  Elapsed seconds: ",
  round(horn_results$elapsed_seconds, 2L),
  "\n  Output directory: ",
  config$horn_validation_output_dir,
  "\n",
  sep = ""
)

print(horn_results$summary, row.names = FALSE)
