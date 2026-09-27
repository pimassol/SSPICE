# ============================================================
# FUNCTION: detect_age_gates
# ============================================================
# Purpose : Detect age-related logical conditions in questionnaire text.
# Inputs  : text
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

detect_age_gates <- function(text) {
  gates <- list()
  m1 <- str_match_all(text, "(?:almeno|\\bda\\b|\\bpari o superiore a\\b)\\s*(\\d{1,2})\\s*anni")
  if (length(m1[[1]])) gates <- c(gates, as.integer(m1[[1]][,2]))
  m2 <- str_match_all(text, "(\\d{1,2})\\s*anni\\s*(?:e\\s*piu|o\\s*piu|\\+)")
  if (length(m2[[1]])) gates <- c(gates, as.integer(m2[[1]][,2]))
  m3 <- str_match_all(text, "(?:minori?\\s*di|inferiore\\s*a)\\s*(\\d{1,2})\\s*anni")
  if (length(m3[[1]])) gates <- c(gates, as.integer(m3[[1]][,2]))
  unique(as.integer(unlist(gates)))
}
