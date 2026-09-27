# ============================================================
# FUNCTION: calculate_out_degree
# ============================================================
# Purpose : Calculate the out-degree of each graph node.
# Inputs  : A
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

calculate_out_degree <- function(A) {
  return(rowSums(A))
}
