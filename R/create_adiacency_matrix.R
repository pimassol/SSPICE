#' Create the adjacency matrix from a transposed incidence matrix
#'
#' Builds a directed adjacency matrix using the signs of the incidence
#' coefficients: `+1` identifies a source node and `-1` identifies a target
#' node. This also preserves the direction of backward edges, allowing cycles
#' to be detected correctly.
#'
#' @param B_transposed Transposed incidence matrix with nodes in columns.
#' @param vnames Character vector containing the node names.
#'
#' @return A square directed adjacency matrix.
#' @keywords internal
create_adiacency_matrix <- function(
  B_transposed,
  vnames
) {
  B_transposed <- as.matrix(B_transposed)

  if (ncol(B_transposed) != length(vnames)) {
    stop(
      "The number of incidence-matrix columns must equal length(vnames).",
      call. = FALSE
    )
  }

  number_of_nodes <- ncol(B_transposed)
  adjacency <- matrix(
    0,
    nrow = number_of_nodes,
    ncol = number_of_nodes
  )

  for (i in seq_len(nrow(B_transposed))) {
    source_nodes <- which(B_transposed[i, ] == 1)
    target_nodes <- which(B_transposed[i, ] == -1)

    if (length(source_nodes) == 0L || length(target_nodes) == 0L) {
      next
    }

    adjacency[
      rep(source_nodes, each = length(target_nodes)),
      rep(target_nodes, times = length(source_nodes))
    ] <- 1
  }

  rownames(adjacency) <- vnames
  colnames(adjacency) <- vnames
  adjacency
}
