# ============================================================
# COUNT THE PATTERNS ADMISSIBLE UNDER THE SSPICE ROUTING DAG
# ============================================================
#
# Input:
#   output/DAG/questionnaire_DAG.rds
#
# Outputs:
#   output/admissible-patterns/admissible_pattern_count.rds
#   output/admissible-patterns/admissible_pattern_summary.csv
#   output/admissible-patterns/execution_time.csv
#
# Run this script with PROJECT as the working directory.
# ============================================================

source(file.path("scripts", "00_config.R"))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required during package development.")
}

if (!requireNamespace("gmp", quietly = TRUE)) {
  stop("Package 'gmp' is required for exact arbitrary-precision counting.")
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

output_dir <- file.path(
  "output",
  "admissible-patterns"
)

if (!file.exists(dag_file)) {
  stop("DAG object not found: ", dag_file)
}

dag_object <- readRDS(dag_file)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

start_time <- Sys.time()

admissible_count <- count_admissible_patterns(
  dag = dag_object,
  root_variables = config$root_variables,
  max_memo_states = config$max_memo_states,
  verbose = TRUE
)

end_time <- Sys.time()
elapsed_seconds <- as.numeric(
  difftime(end_time, start_time, units = "secs")
)

summary_table <- data.frame(
  measure = c(
    "n_variables",
    "n_modal_rows",
    "root_variables",
    "n_memo_states",
    "n_all_unrestricted_patterns",
    "n_admissible_patterns",
    "exact_arbitrary_precision"
  ),
  value = c(
    admissible_count$n_variables,
    admissible_count$n_modal_rows,
    paste(admissible_count$root_variables, collapse = " | "),
    admissible_count$n_memo_states,
    as.character(admissible_count$n_all_unrestricted_patterns),
    as.character(admissible_count$n_admissible_patterns),
    admissible_count$exact_arbitrary_precision
  ),
  stringsAsFactors = FALSE
)

execution_time <- data.frame(
  start_time = as.character(start_time),
  end_time = as.character(end_time),
  elapsed_seconds = elapsed_seconds,
  stringsAsFactors = FALSE
)

result <- list(
  count = admissible_count,
  summary = summary_table,
  started_at = start_time,
  completed_at = end_time,
  elapsed_seconds = elapsed_seconds
)

saveRDS(
  result,
  file.path(
    output_dir,
    "admissible_pattern_count.rds"
  )
)

utils::write.csv(
  summary_table,
  file.path(
    output_dir,
    "admissible_pattern_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  execution_time,
  file.path(
    output_dir,
    "execution_time.csv"
  ),
  row.names = FALSE,
  na = ""
)

cat(
  "\nSSPICE admissible-pattern count\n",
  "  Variables: ", admissible_count$n_variables,
  "\n  Modal rows: ", admissible_count$n_modal_rows,
  "\n  Root variables: ",
  paste(admissible_count$root_variables, collapse = " | "),
  "\n  Memoized states: ", admissible_count$n_memo_states,
  "\n  Unrestricted patterns: ",
  as.character(admissible_count$n_all_unrestricted_patterns),
  "\n  Admissible patterns: ",
  as.character(admissible_count$n_admissible_patterns),
  "\n  Exact arbitrary precision: ",
  admissible_count$exact_arbitrary_precision,
  "\n  Elapsed seconds: ", round(elapsed_seconds, 2L),
  "\n  Output directory: ", output_dir,
  "\n",
  sep = ""
)
