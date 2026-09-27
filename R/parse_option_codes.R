# ============================================================
# FUNCTION: parse_option_codes
# ============================================================
# Purpose : Extract questionnaire option codes associated with target keywords.
# Inputs  : text_block, target_keywords
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

parse_option_codes <- function(text_block, target_keywords = c("celibe","nubile","coniug","sposat","divor","separ","vedov")) {
  out <- c()
  lines <- unlist(strsplit(text_block, "\n"))
  lines <- lines[sapply(target_keywords, function(k) grepl(k, tolower(lines))) |> apply(1, any)]
  lines <- nl_norm(paste(lines, collapse = " "))
  m <- gregexpr("(\\d{1,3})\\s+([a-zA-Z\\-/ ]{2,30})", lines)
  parts <- regmatches(lines, m)[[1]]
  if (length(parts)) {
    for (p in parts) {
      code <- as.integer(sub("^(\\d{1,3}).*$", "\\1", p))
      lab  <- sub("^\\d+\\s+", "", p)
      if (grepl("celib|nub", lab)) out["celibe_nubile"] <- code
      if (grepl("coniug|spos", lab)) out["coniugato"] <- code
      if (grepl("divor|separ", lab)) out["div_sep"] <- code
      if (grepl("vedov", lab)) out["vedovo"] <- code
    }
  }
  out
}
