# ============================================================
# FUNCTION: dummize
# ============================================================
# Purpose : Convert a value into its dummy-coded representation for the specified domain.
# Inputs  : num, nitems, minitem, maxitem
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

dummize <- function(num, nitems, minitem, maxitem) {
  # Inizializza vettore binario di zeri
  dummy <- rep(0, nitems)
  
  if (!is.na(num)) {
    
    # Caso con una sola modalità: assegna 1 alla prima posizione
    if (nitems == 1) {
      dummy[1] <- 1
      return(dummy)
    } else {
      # Calcola la distanza tra le modalità
      step <- (maxitem - minitem) / (nitems - 1)
      
      # Calcola l'indice relativo (1-based), limitandolo a [1, nitems]
      pos <- round((num - minitem) / step) + 1
      pos <- max(1, min(nitems, pos))  # protezione contro fuori range
      
      dummy[pos] <- 1
    }
  }
  
  return(dummy)
}
