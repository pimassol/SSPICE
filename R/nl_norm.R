# ============================================================
# FUNCTION: nl_norm
# ============================================================
# Purpose : Normalize natural-language text for semantic rule extraction.
# Inputs  : x
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

nl_norm <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    tolower() |>
    gsub("[ \t]+", " ", x = _) |>
    gsub("\u00A0", " ", x = _) |>
    trimws()
}
