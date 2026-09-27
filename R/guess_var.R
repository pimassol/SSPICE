# ============================================================
# FUNCTION: guess_var
# ============================================================
# Purpose : Guess a routing variable associated with a requested semantic concept.
# Inputs  : vnames, kind
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

guess_var <- function(vnames, kind=c("eta","sesso","stato_civ","patente","gravidanza")) {
  pat <- switch(match.arg(kind),
    eta       = "(^|_)eta($|_|\\d)",
    sesso     = "sess|sex|genere",
    stato_civ = "stato.?civ|civil|m?stato",
    patente   = "patent",
    gravidanza= "grav|pregn"
  )
  i <- grep(pat, tolower(vnames))
  if (length(i)) vnames[i[1]] else NA_character_
}
