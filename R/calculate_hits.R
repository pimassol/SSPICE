#' Calculate HITS scores
#'
#' Calculates hub and authority scores for the nodes of a directed graph by
#' applying the HITS iteration to its adjacency matrix.
#'
#' @param A A square numeric adjacency matrix. Rows are origin nodes and
#'   columns are destination nodes.
#' @param max_iter Maximum number of iterations.
#' @param tol Convergence tolerance for the largest absolute score change.
#'
#' @return A list containing `authority`, `hub`, `iterations`, and
#'   `converged`.
#' @export
calculate_hits <- function(A, max_iter = 100L, tol = 1e-6) {
  A <- as.matrix(A)

  if (!is.numeric(A) || nrow(A) != ncol(A)) {
    stop("A must be a square numeric adjacency matrix.")
  }

  if (nrow(A) == 0L || anyNA(A) || any(!is.finite(A)) || any(A < 0)) {
    stop("A must be non-empty and contain finite non-negative values.")
  }

  max_iter <- as.integer(max_iter)

  if (length(max_iter) != 1L || is.na(max_iter) || max_iter < 1L) {
    stop("max_iter must be a positive integer.")
  }

  if (length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("tol must be a positive finite number.")
  }

  number_of_nodes <- nrow(A)
  hub <- rep(1 / sqrt(number_of_nodes), number_of_nodes)
  authority <- rep(1 / sqrt(number_of_nodes), number_of_nodes)
  converged <- FALSE
  iterations <- 0L

  if (!any(A != 0)) {
    zero_scores <- setNames(rep(0, number_of_nodes), rownames(A))

    return(
      list(
        authority = zero_scores,
        hub = zero_scores,
        iterations = 0L,
        converged = TRUE
      )
    )
  }

  for (iteration in seq_len(max_iter)) {
    new_authority <- as.numeric(crossprod(A, hub))
    authority_norm <- sqrt(sum(new_authority^2))

    if (authority_norm > 0) {
      new_authority <- new_authority / authority_norm
    }

    new_hub <- as.numeric(A %*% new_authority)
    hub_norm <- sqrt(sum(new_hub^2))

    if (hub_norm > 0) {
      new_hub <- new_hub / hub_norm
    }

    score_change <- max(
      abs(new_authority - authority),
      abs(new_hub - hub)
    )

    authority <- new_authority
    hub <- new_hub
    iterations <- iteration

    if (score_change < tol) {
      converged <- TRUE
      break
    }
  }

  names(authority) <- colnames(A)
  names(hub) <- rownames(A)

  list(
    authority = authority,
    hub = hub,
    iterations = iterations,
    converged = converged
  )
}
