# ============================================================
# FUNCTION: in_domain_var_item
# ============================================================
# Purpose : Check whether an item belongs to the declared domain of a variable.
# Inputs  : vardom, varname, item
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

in_domain_var_item <- function(vardom, varname, item) {
  i <- match(varname, vardom$varname); if (is.na(i)) return(FALSE)
  iv <- .parse_intervalli(vardom$intervalli[i])
  if (nrow(iv) > 0) return(any(item >= iv[,1] & item <= iv[,2]))
  mn <- suppressWarnings(as.numeric(vardom$minitem[i]))
  mx <- suppressWarnings(as.numeric(vardom$maxitem[i]))
  if (is.na(mn) || is.na(mx)) return(FALSE)
  item >= mn && item <= mx
}
