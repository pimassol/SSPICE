# ============================================================
# FUNCTION: .parse_intervalli
# ============================================================
# Purpose : Parse an encoded interval specification into its component values.
# Inputs  : s
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

.parse_intervalli <- function(s) {
  if (is.na(s) || !nzchar(s)) return(matrix(numeric(0), ncol = 2))
  n <- nchar(s); stopifnot(n %% 8 == 0)
  m <- matrix(as.integer(c(
    substring(s, seq(1, n, 8),   seq(4, n, 8)),
    substring(s, seq(5, n, 8),   seq(8, n, 8))
  )), ncol = 2)
  colnames(m) <- c("start","end")
  m
}
