# ============================================================
# SSPICE - 01 BUILD DAG
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

if (!file.exists(config$routing_file)) {
  stop(
    paste0(
      "Routing metadata file not found: ",
      config$routing_file
    )
  )
}

dir.create(
  config$dag_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dag_object <- build_DAG(
  routing = config$routing_file,
  separator = config$metadata_separator,
  na_strings = config$missing_codes,
  analyze = TRUE,
  stop_on_cycle = TRUE,
  max_paths_per_partition = config$max_paths_per_partition
)

saveRDS(
  dag_object,
  file.path(
    config$dag_output_dir,
    "questionnaire_DAG.rds"
  )
)

saveRDS(
  dag_object$graph_diagnostics,
  file.path(
    config$dag_output_dir,
    "graph_diagnostics.rds"
  )
)

saveRDS(
  dag_object$partitions,
  file.path(
    config$dag_output_dir,
    "graph_partitions.rds"
  )
)

utils::write.csv(
  dag_object$routes_grouped,
  file.path(
    config$dag_output_dir,
    "routing_grouped.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dag_object$routes_numeric,
  file.path(
    config$dag_output_dir,
    "routing_numeric.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dag_object$Bt,
  file.path(
    config$dag_output_dir,
    "incidence_matrix_transposed.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dag_object$A,
  file.path(
    config$dag_output_dir,
    "adjacency_matrix.csv"
  ),
  row.names = TRUE,
  na = ""
)

utils::write.csv(
  dag_object$node_diagnostics,
  file.path(
    config$dag_output_dir,
    "node_diagnostics.csv"
  ),
  row.names = FALSE,
  na = ""
)

print(dag_object)

cat(
  "  Incidence rows:", nrow(dag_object$Bt), "\n",
  "  Source nodes:",
  length(dag_object$graph_diagnostics$source_node_indices), "\n",
  "  Isolated nodes:",
  length(dag_object$graph_diagnostics$isolated_node_indices), "\n",
  "  Partitions:", length(dag_object$partitions), "\n",
  "  Output directory:", config$dag_output_dir, "\n"
)
