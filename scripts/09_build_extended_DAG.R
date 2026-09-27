# ============================================================
# BUILD THE SSPICE DAG EXTENDED WITH LOGICAL-EDIT NODES
# ============================================================

source(file.path("scripts", "00_config.R"))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required during package development.")
}

pkgload::load_all(
  path = ".", reset = TRUE, export_all = FALSE,
  helpers = FALSE, quiet = FALSE
)

logical_edit_rules_file <- if (
  "logical_edit_rules_file" %in% names(config)
) config$logical_edit_rules_file else file.path(
  "data-raw", "metadata", "logical_edit_rules.csv"
)

prepared_data_file <- file.path(
  config$prepared_data_output_dir,
  "prepared_questionnaire_data.rds"
)

output_dir <- if (
  "extended_dag_output_dir" %in% names(config)
) config$extended_dag_output_dir else file.path("output", "extended-DAG")

extended_routing_file <- file.path(output_dir, "extended_routing.csv")
required_files <- c(
  config$routing_file, logical_edit_rules_file, prepared_data_file
)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing required files: ", paste(missing_files, collapse = ", "), "."
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

create_edits_internal <- getFromNamespace(
  "create_logical_edit_variables", "SSPICE"
)
extend_routing_internal <- getFromNamespace(
  "extend_routing_with_logical_edits", "SSPICE"
)

prepared_data <- readRDS(prepared_data_file)
started_at <- Sys.time()

extended_data <- create_edits_internal(
  inpdata = prepared_data,
  logical_edits_data = logical_edit_rules_file,
  extDFpath = config$external_data_dir,
  separator = config$metadata_separator
)

extended_routing <- extend_routing_internal(
  routing = config$routing_file,
  logical_edits = logical_edit_rules_file,
  separator = config$metadata_separator,
  na_strings = config$missing_codes,
  output_file = extended_routing_file
)

logical_edit_names <- attr(extended_routing, "logical_edit_names")

extended_dag <- build_DAG(
  routing = extended_routing_file,
  separator = config$metadata_separator,
  na_strings = config$missing_codes,
  analyze = TRUE,
  stop_on_cycle = TRUE,
  max_paths_per_partition = config$max_paths_per_partition
)

# Store semantic metadata explicitly for routing_check() and localization.
extended_dag$logical_edit_names <- logical_edit_names

if (!identical(names(extended_data), extended_dag$varnames)) {
  stop(
    "Extended data are not aligned with DAG+. Missing: ",
    paste(setdiff(extended_dag$varnames, names(extended_data)), collapse = ", "),
    "; unexpected: ",
    paste(setdiff(names(extended_data), extended_dag$varnames), collapse = ", "),
    "."
  )
}

logical_edit_outcomes <- do.call(
  rbind,
  lapply(logical_edit_names, function(edit_name) {
    edit_values <- extended_data[[edit_name]]
    observed_values <- sort(unique(edit_values[!is.na(edit_values)]))
    data.frame(
      logical_edit = edit_name,
      value_1 = sum(edit_values == 1, na.rm = TRUE),
      value_2 = sum(edit_values == 2, na.rm = TRUE),
      missing_violation = sum(is.na(edit_values)),
      observed_domain = paste(observed_values, collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
)

completed_at <- Sys.time()
elapsed_seconds <- as.numeric(
  difftime(completed_at, started_at, units = "secs")
)

base_dag_file <- file.path(config$dag_output_dir, "questionnaire_DAG.rds")
base_bt_rows <- if (file.exists(base_dag_file)) {
  nrow(readRDS(base_dag_file)$Bt)
} else {
  NA_integer_
}

extended_summary <- data.frame(
  measure = c(
    "original_nodes", "extended_nodes", "logical_edit_nodes",
    "original_bt_rows", "extended_bt_rows", "extended_edges",
    "extended_is_DAG", "records", "elapsed_seconds"
  ),
  value = c(
    length(extended_dag$varnames) - length(logical_edit_names),
    length(extended_dag$varnames), length(logical_edit_names),
    base_bt_rows, nrow(extended_dag$Bt), sum(extended_dag$A != 0),
    extended_dag$graph_diagnostics$is_DAG,
    nrow(extended_data), elapsed_seconds
  ),
  stringsAsFactors = FALSE
)

saveRDS(
  extended_dag,
  file.path(output_dir, "questionnaire_extended_DAG.rds")
)
saveRDS(
  extended_data,
  file.path(output_dir, "prepared_data_with_logical_edits.rds")
)
utils::write.csv(
  logical_edit_outcomes,
  file.path(output_dir, "logical_edit_outcomes.csv"),
  row.names = FALSE, na = ""
)
utils::write.csv(
  extended_summary,
  file.path(output_dir, "extended_DAG_summary.csv"),
  row.names = FALSE, na = ""
)

cat(
  "\nSSPICE extended DAG\n",
  "  Original nodes: ",
  length(extended_dag$varnames) - length(logical_edit_names),
  "\n  Logical-edit nodes: ", length(logical_edit_names),
  "\n  Extended nodes: ", length(extended_dag$varnames),
  "\n  Extended Bt rows: ", nrow(extended_dag$Bt),
  "\n  Extended edges: ", sum(extended_dag$A != 0),
  "\n  Directed acyclic graph: ",
  extended_dag$graph_diagnostics$is_DAG,
  "\n  Data aligned with DAG+: ",
  identical(names(extended_data), extended_dag$varnames),
  "\n  Elapsed seconds: ", round(elapsed_seconds, 2L),
  "\n  Output directory: ", output_dir, "\n",
  sep = ""
)

print(logical_edit_outcomes, row.names = FALSE)
