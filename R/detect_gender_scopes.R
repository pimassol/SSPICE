# ============================================================
# FUNCTION: detect_gender_scopes
# ============================================================
# Purpose : Detect gender-related applicability conditions in questionnaire text.
# Inputs  : text
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

detect_gender_scopes <- function(text) {
  list(
    only_male   = any(grepl("solo\\s+(?:per\\s+)?uomini|maschi\\s+sol[oa]", text)),
    only_female = any(grepl("solo\\s+(?:per\\s+)?donne|femmine\\s+sol[oa]", text))
  )
}
