# ============================================================
# FUNCTION: generate_stree
# ============================================================
# Purpose : Generate a spanning-tree representation from incidence and routing data.
# Inputs  : t_inc, routes
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

generate_stree <- function(t_inc, routes) {
  ne <- nrow(t_inc)
  nv <- ncol(t_inc)
  vtypes <- routes[, 5]
  	
  T_full <- matrix(0, nrow = ne * nv, ncol = 2)
  L_full <- integer(0)
  visited_nodes <- rep(0, nv)
  edge <- 1

  for (k in 1:ne) {
    s <- which(t_inc[k, ] == +1)
    targets <- which(t_inc[k, ] == -1)
    for (t in targets) {
      T_full[edge, ] <- c(s, t)
      edge <- edge + 1
    }
    if (visited_nodes[s] == 0) {
      visited_nodes[s] <- 1
      L_full <- c(L_full, s)
    }
  }

  T_full <- T_full[1:(edge - 1), , drop = FALSE]
  tree <- data.frame(
    origin = T_full[, 1],
    target = T_full[, 2],
    vtype_target = vtypes[T_full[, 2]]
  )

  list_nodes <- data.frame(
    node = sort(unique(c(tree$origin, tree$target)))
  )

  return(list(
    stree = tree,
    list = list_nodes
  ))
}
