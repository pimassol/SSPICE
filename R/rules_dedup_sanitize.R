# ============================================================
# FUNCTION: rules_dedup_sanitize
# ============================================================
# Purpose : Deduplicate and sanitize formal rules against declared variable domains.
# Inputs  : rules, vardom
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

rules_dedup_sanitize <- function(rules, vardom) {
  rules0 <- unique(trimws(rules))
  keep <- rep(TRUE, length(rules0))
  re_head <- "^\\s*([A-Za-z0-9_]+)\\(([-0-9]+)\\)"
  heads <- regexec(re_head, rules0)
  m <- regmatches(rules0, heads)
  for (r in seq_along(rules0)) {
    mm <- m[[r]]
    if (length(mm) >= 3) {
      var <- mm[[2]]; itm <- suppressWarnings(as.integer(mm[[3]]))
      if (is.na(itm) || !in_domain_var_item(vardom, var, itm)) keep[r] <- FALSE
    }
  }
  list(rules = rules0[keep], dropped = rules0[!keep])
}
