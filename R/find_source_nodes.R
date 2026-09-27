# ============================================================
# FUNCTION: find_source_nodes
# ============================================================
# Purpose : Identify source nodes in the directed graph.
# Inputs  : A
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

find_source_nodes <- function(A) {
  return(which(colSums(A) == 0))  # Nodi con in-degree = 0
}
