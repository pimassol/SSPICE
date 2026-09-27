# ============================================================
# FUNCTION: if_is_a_group
# ============================================================
# Purpose : Return the size of the logical group containing a specified routing variable.
# Inputs  : var_index, vtype
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

if_is_a_group <- function(var_index, vtype) {
  v <- vtype[var_index]
  if (v != 0) {
    grp <- which(vtype == v)
    return(length(grp))
  } else {
    return(1)
  }
}
