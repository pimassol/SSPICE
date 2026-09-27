#' Build and analyze a directed acyclic graph
#'
#' Builds the SSPICE graph representation from routing metadata. Logical OR
#' and XOR groups are coarsened before constructing the transposed incidence
#' matrix and the adjacency matrix. No record-level data are required.
#'
#' @param routing Either the path to a routing CSV file or a data frame
#'   containing the routing metadata.
#' @param separator Field separator used when `routing` is a file path.
#' @param na_strings Character values interpreted as missing values when
#'   reading a routing file.
#' @param analyze Logical value indicating whether structural diagnostics
#'   should be calculated.
#' @param stop_on_cycle Logical value indicating whether to stop when the
#'   resulting graph is not acyclic.
#' @param max_paths_per_partition Maximum estimated number of paths allowed
#'   in each graph partition.
#' @param start_node Index of the starting node used for path diagnostics.
#' @param end_node Optional index of the destination node used for path
#'   diagnostics. The last graph node is used by default.
#'
#' @return An object of class `sspice_dag`. It contains the original and
#'   transformed routing metadata, node names, the transposed incidence matrix
#'   `Bt`, the adjacency matrix `A`, and optional node-level and graph-level
#'   diagnostics.
#'
#' @examples
#' routing <- data.frame(
#'   V1 = c("Q1", "Q2"),
#'   V2 = c(1, 1),
#'   V3 = c(1, 1),
#'   V4 = c(1, 1),
#'   V5 = c(0, 0),
#'   V6 = c(0, 0)
#' )
#' dag <- build_DAG(routing)
#' dag$Bt
#' dag$A
#'
#' @export
build_DAG <- function(
  routing,
  separator = ";",
  na_strings = c("", ".", "NA", "NULL"),
  analyze = TRUE,
  stop_on_cycle = TRUE,
  max_paths_per_partition = 1e6,
  start_node = 1L,
  end_node = NULL
) {
  if (missing(routing)) {
    stop("routing must be a file path or a data frame.")
  }

  if (is.character(routing) && length(routing) == 1L) {
    if (!file.exists(routing)) {
      stop(paste0("Routing metadata file not found: ", routing))
    }

    routes_original <- utils::read.csv(
      file = routing,
      header = FALSE,
      sep = separator,
      na.strings = na_strings,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    names(routes_original) <- paste0(
      "V",
      seq_len(ncol(routes_original))
    )
  } else if (is.data.frame(routing)) {
    routes_original <- routing
  } else {
    stop("routing must be a single file path or a data frame.")
  }

  if (ncol(routes_original) < 6L) {
    stop("The routing metadata must contain at least six columns.")
  }

  required_columns <- paste0("V", 1:6)

  if (!all(required_columns %in% names(routes_original))) {
    names(routes_original) <- paste0(
      "V",
      seq_len(ncol(routes_original))
    )
  }

  if (nrow(routes_original) < 2L) {
    stop("The routing metadata must contain at least two variables.")
  }

  variable_names <- trimws(as.character(routes_original$V1))

  if (anyNA(variable_names) || any(variable_names == "")) {
    stop("Column V1 contains missing or empty variable names.")
  }

  if (anyDuplicated(variable_names)) {
    stop("Column V1 contains duplicated variable names.")
  }

  routes_original$V1 <- variable_names

  group_ids <- suppressWarnings(as.numeric(routes_original$V5))

  if (anyNA(group_ids[!is.na(routes_original$V5)])) {
    stop("Column V5 must contain numeric group identifiers.")
  }

  positive_groups <- unique(group_ids[!is.na(group_ids) & group_ids > 0])

  if (length(positive_groups) > 0L) {
    positive_group_sizes <- vapply(
      positive_groups,
      function(group_id) sum(group_ids == group_id, na.rm = TRUE),
      integer(1)
    )

    if (any(positive_group_sizes < 2L)) {
      invalid_groups <- positive_groups[positive_group_sizes < 2L]

      stop(
        paste0(
          "Each OR group must contain at least two variables. Invalid groups: ",
          paste(invalid_groups, collapse = ", "),
          "."
        )
      )
    }
  }

  routes_grouped_OR <- coarse_groups_OR(routes_original)
  routes_grouped <- coarse_groups_XOR(routes_grouped_OR)
  varnames <- as.character(routes_grouped$V1)
  routes_numeric <- convert_routes_str2num(routes_grouped)

  Bt <- create_incidence_matrix(
    routes = routes_numeric,
    vnames = varnames
  )

  A <- create_adiacency_matrix(
    B_transposed = Bt,
    vnames = varnames
  )

  dag_test <- is_DAG(A)

  if (!dag_test && isTRUE(stop_on_cycle)) {
    stop("The graph constructed from the routing metadata is not a DAG.")
  }

  node_diagnostics <- NULL
  graph_diagnostics <- list(
    is_DAG = dag_test,
    number_of_original_variables = nrow(routes_original),
    number_of_nodes = nrow(A),
    number_of_incidence_rows = nrow(Bt),
    number_of_adjacency_edges = sum(A != 0)
  )
  partitions <- NULL

  if (isTRUE(analyze)) {
    degree_results <- calculate_degrees(A)
    hits_results <- calculate_hits(A)
    source_nodes <- find_source_nodes(A)
    isolated_nodes <- find_isolated_nodes(A)

    node_diagnostics <- data.frame(
      node_index = seq_along(varnames),
      variable = varnames,
      in_degree = as.numeric(degree_results$in_degree),
      out_degree = as.numeric(degree_results$out_degree),
      total_degree = as.numeric(degree_results$total_degree),
      authority = as.numeric(hits_results$authority),
      hub = as.numeric(hits_results$hub),
      is_source = seq_along(varnames) %in% source_nodes,
      is_isolated = seq_along(varnames) %in% isolated_nodes,
      stringsAsFactors = FALSE
    )

    graph_diagnostics$source_node_indices <- source_nodes
    graph_diagnostics$source_node_names <- varnames[source_nodes]
    graph_diagnostics$isolated_node_indices <- isolated_nodes
    graph_diagnostics$isolated_node_names <- varnames[isolated_nodes]
    graph_diagnostics$hits_iterations <- hits_results$iterations
    graph_diagnostics$hits_converged <- hits_results$converged

    if (dag_test) {
      start_node <- as.integer(start_node)

      if (is.null(end_node)) {
        end_node <- nrow(A)
      }

      end_node <- as.integer(end_node)

      shortest_path <- find_shortest_path(
        A = A,
        start_node = start_node,
        end_node = end_node,
        vnames = varnames
      )

      longest_path <- find_longest_path(
        A = A,
        vnames = varnames,
        start_node = start_node,
        end_node = end_node
      )

      partitions <- graph_partitions(
        A = A,
        max_paths_per_partition = max_paths_per_partition
      )

      graph_diagnostics$shortest_path <- shortest_path
      graph_diagnostics$longest_path <- longest_path
      graph_diagnostics$number_of_partitions <- length(partitions)
    }
  }

  result <- list(
    routes_original = routes_original,
    routes_grouped_OR = routes_grouped_OR,
    routes_grouped = routes_grouped,
    routes_numeric = routes_numeric,
    varnames_original = variable_names,
    varnames = varnames,
    Bt = Bt,
    A = A,
    node_diagnostics = node_diagnostics,
    graph_diagnostics = graph_diagnostics,
    partitions = partitions
  )

  class(result) <- c("sspice_dag", "list")
  result
}

#' Print an SSPICE DAG
#'
#' @param x An object created by [build_DAG()].
#' @param ... Additional arguments currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.sspice_dag <- function(x, ...) {
  cat("SSPICE DAG\n")
  cat("  Nodes:", nrow(x$A), "\n")
  cat("  Edges:", sum(x$A != 0), "\n")
  cat("  Directed acyclic graph:", x$graph_diagnostics$is_DAG, "\n")
  invisible(x)
}
