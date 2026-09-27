#' Find isolated graph nodes
#'
#' Identifies nodes having neither incoming nor outgoing edges.
#'
#' @param A A square numeric adjacency matrix. Rows are origin nodes and
#'   columns are destination nodes.
#'
#' @return An integer vector containing the indices of isolated nodes.
#' @export
find_isolated_nodes <- function(A) {
  A <- as.matrix(A)

  if (!is.numeric(A) || nrow(A) != ncol(A)) {
    stop("A must be a square numeric adjacency matrix.")
  }

  if (nrow(A) == 0L || anyNA(A) || any(!is.finite(A))) {
    stop("A must be non-empty and contain finite values.")
  }

  which(rowSums(A != 0) == 0L & colSums(A != 0) == 0L)
}
