# ============================================================
# FUNCTION: convert_routes_str2num
# ============================================================
# Purpose : Convert routing references from variable names to numeric row indices.
# Inputs  : routes
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

convert_routes_str2num <- function(routes) {
  # Creare una copia del dataframe elaborata dalla funzione coarse_groups_OR
  varnames <- routes[, 1]

  # Rimuovere valori NA e stringhe vuote dai nomi delle variabili
  valid_varnames <- varnames[!is.na(varnames) & varnames != ""]
  
  # Creare una mappatura: ogni stringa viene associata alla sua riga di origine in routes
  string_map <- setNames(seq_along(valid_varnames), valid_varnames)

  # Funzione per sostituire le stringhe con l'indice di riga
  replace_with_row_index <- function(x) {
    if (is.na(x) || x == "<NA>" || x == "") {
      return(NA)   
    } else if (x %in% names(string_map)) {
      return(as.integer(string_map[x]))  # Sostituisci la stringa con il numero di riga
    } else {
      return(x)  # Mantieni inalterati i valori numerici
    }
  }
  
  # Applicare la sostituzione a tutto il dataframe
  routes[] <- lapply(routes, function(col) sapply(col, replace_with_row_index))

  # Convertire tutte le colonne in numeri interi, sostituendo stringhe vuote con NA
  routes[] <- lapply(routes, function(col) {
    col[col == ""] <- NA
    suppressWarnings(as.integer(col))  # Sopprime eventuali avvisi di coercizione
  })

  return(routes)
}
