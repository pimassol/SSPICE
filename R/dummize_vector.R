# ============================================================
# FUNCTION: dummize_vector
# ============================================================
# Purpose : Dummy-code all elements of a record using their declared item counts.
# Inputs  : x_row, y
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

dummize_vector <- function(x_row, y) {
  
  out <- vector("list", length(x_row))
  
  for (i in seq_along(x_row)) {
    ni <- y[i]
    xi <- as.numeric(x_row[[i]])  # forziamo a numerico
    
    if (ni == 1) {
      out[[i]] <- if (!is.na(xi)) 1L else 0L
      
    } else if (ni > 1) {
      d <- rep(0L, ni)
      if (!is.na(xi)) {
        pos <- min(max(1, as.integer(xi)), ni)
        d[pos] <- 1L
      }
      out[[i]] <- d
    } else {
      out[[i]] <- integer(0)
    }
  }
  
  return(unlist(out))
}
