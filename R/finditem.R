# ============================================================
# FUNCTION: finditem
# ============================================================
# Purpose : Locate vector elements satisfying the requested comparison with a target item.
# Inputs  : vect, item, logop
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

finditem <- function(vect, item, logop) {
  nc <- length(vect)
  v <- rep(0, nc)
  if (is.na(item) | (item == 0)) item <- 1e-6
  vect[vect == 0] <- 1e-6
  
  if (logop == "=") {
    v[vect == item] <- 1
  } else if (logop == "<") {
    v[vect < item] <- 1
  } else if (logop == ">") {
    v[vect > item] <- 1
  } else if (logop == "<=") {
    v[vect <= item] <- 1
  } else if (logop == ">=") {
    v[vect >= item] <- 1
  } else if (logop == "<>") {
    v[vect != item] <- 1
  }
  inds <- which(v == 1)
  if (length(inds) == 0) inds <- 0
  return(inds)
}
