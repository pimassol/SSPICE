#' Find a longest path in a DAG
#'
#' Finds a longest directed path between two selected nodes of a directed
#' acyclic graph.
#'
#' @param A A square numeric adjacency matrix. Rows are origin nodes and
#'   columns are destination nodes.
#' @param vnames Optional vector of node names. When omitted, matrix row names
#'   are used, followed by integer node indices when row names are unavailable.
#' @param start_node Index of the starting node.
#' @param end_node Index of the destination node.
#'
#' @return A list containing `path`, `length`, `edge_length`, and
#'   `exists_path`. For backward compatibility, `length` is the number of
#'   nodes in the path; `edge_length` is the number of traversed edges.
#' @export
find_longest_path <- function(
  A,
  vnames = NULL,
  start_node = 1L,
  end_node = nrow(A)
) {
  A <- as.matrix(A)

  if (!is.numeric(A) || nrow(A) != ncol(A)) {
    stop("A must be a square numeric adjacency matrix.")
  }

  if (nrow(A) == 0L || anyNA(A) || any(!is.finite(A))) {
    stop("A must be non-empty and contain finite values.")
  }

  if (!is_DAG(A)) {
    stop("A must represent a directed acyclic graph.")
  }

  number_of_nodes <- nrow(A)
  start_node <- as.integer(start_node)
  end_node <- as.integer(end_node)

  if (
    length(start_node) != 1L || is.na(start_node) ||
    start_node < 1L || start_node > number_of_nodes
  ) {
    stop("start_node is outside the graph.")
  }

  if (
    length(end_node) != 1L || is.na(end_node) ||
    end_node < 1L || end_node > number_of_nodes
  ) {
    stop("end_node is outside the graph.")
  }

  if (is.null(vnames)) {
    vnames <- rownames(A)
  }

  if (is.null(vnames)) {
    vnames <- as.character(seq_len(number_of_nodes))
  }

  if (length(vnames) != number_of_nodes) {
    stop("vnames must contain one name for each graph node.")
  }

  in_degree <- colSums(A != 0)
  queue <- which(in_degree == 0L)
  topological_order <- integer(0)

  while (length(queue) > 0L) {
    node <- queue[1L]
    queue <- queue[-1L]
    topological_order <- c(topological_order, node)

    successors <- which(A[node, ] != 0)

    for (successor in successors) {
      in_degree[successor] <- in_degree[successor] - 1L

      if (in_degree[successor] == 0L) {
        queue <- c(queue, successor)
      }
    }
  }

  distance <- rep(-Inf, number_of_nodes)
  parent <- rep(NA_integer_, number_of_nodes)
  distance[start_node] <- 0

  for (node in topological_order) {
    if (!is.finite(distance[node])) {
      next
    }

    successors <- which(A[node, ] != 0)

    for (successor in successors) {
      candidate_distance <- distance[node] + 1

      if (candidate_distance > distance[successor]) {
        distance[successor] <- candidate_distance
        parent[successor] <- node
      }
    }
  }

  if (!is.finite(distance[end_node])) {
    return(
      list(
        path = NULL,
        length = 0L,
        edge_length = 0L,
        exists_path = 0L
      )
    )
  }

  path_indices <- end_node

  while (path_indices[1L] != start_node) {
    previous_node <- parent[path_indices[1L]]

    if (is.na(previous_node)) {
      return(
        list(
          path = NULL,
          length = 0L,
          edge_length = 0L,
          exists_path = 0L
        )
      )
    }

    path_indices <- c(previous_node, path_indices)
  }

  list(
    path = vnames[path_indices],
    length = length(path_indices),
    edge_length = length(path_indices) - 1L,
    exists_path = 1L
  )
}
