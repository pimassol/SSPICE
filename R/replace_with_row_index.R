# ============================================================
# FUNCTION: replace_with_row_index
# ============================================================
# Purpose : Replace mapped string values with their corresponding row indices.
# Inputs  : x, string_map
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

replace_with_row_index <- function(x, string_map) {
  if (is.na(x) || x == "<NA>") {
    return(x)   
  } else if (x %in% names(string_map)) {
    return(as.integer(string_map[x]))  # Sostituisci la stringa con il numero di riga
  } else {
    return(x)  # Mantieni inalterati i valori numerici
  }
}
