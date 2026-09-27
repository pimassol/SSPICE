# ============================================================
# FUNCTION: clean_data
# ============================================================
# Purpose : Convert questionnaire data columns into the numeric representation used by validation.
# Inputs  : data
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

clean_data <- function(data) {
  data_clean <- data
  
  for (j in seq_along(data_clean)) {
    col <- data_clean[[j]]
    
    if (is.character(col) || is.factor(col)) {
      col_chr <- as.character(col)
      # Rimuove caratteri non ASCII (invisibili o problematici)
      col_ascii <- iconv(col_chr, from = "", to = "ASCII", sub = "")
      # Stringa vuota o NA ? NA, altrimenti 1
      col_numeric <- ifelse(nchar(trimws(col_ascii)) > 0, 1, NA)
      data_clean[[j]] <- as.numeric(col_numeric)
    } else {
      # Prova a forzare in numeric (anche se già int o num)
      data_clean[[j]] <- suppressWarnings(as.numeric(col))
    }
  }
  
  return(data_clean)
}
