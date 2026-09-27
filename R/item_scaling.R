# ============================================================
# FUNCTION: item_scaling
# ============================================================
# Purpose : Map an original item value to its internal scaled item position.
# Inputs  : num, nitems, minitem, maxitem
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

item_scaling <- function(num, nitems, minitem, maxitem) {
  vv <- dummize(num, nitems, minitem, maxitem)
  new_item <- finditem(vv, 1, "=")
  if (length(new_item) == 0 || new_item == 0) new_item <- NA
  return(new_item)
}
