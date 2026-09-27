# ============================================================
# FUNCTION: V_build_patches
# ============================================================
# Purpose : Build corrective patches from routing, domains, rules, and validation results.
# Inputs  : routes, Bt, vardom, rules, res, items.map.file, items.map.path
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

V_build_patches <- function(routes, Bt, vardom, rules, res,
                            items.map.file, items.map.path) {
  vnames <- colnames(Bt)
  patches <- list(); notes <- c()

  # A) Allineamento min/max VARDOM ? ROUTES
  rout_min <- routes[,3]; rout_max <- routes[,4]
  for (i in seq_along(vnames)) {
    vmn <- suppressWarnings(as.numeric(vardom$minitem[i]))
    vmx <- suppressWarnings(as.numeric(vardom$maxitem[i]))
    rmn <- rout_min[i]; rmx <- rout_max[i]
    if (!is.na(rmn) && !is.na(rmx) && (!identical(vmn, rmn) || !identical(vmx, rmx))) {
      patches <- append(patches, list(list(
        type="fix_vardom_minmax", var=vnames[i], new_min=rmn, new_max=rmx,
        reason="Allineamento a ROUTES"
      )))
    }
  }

  # B) Igiene REGOLE: dedup + item fuori dominio
  aud <- rules_dedup_sanitize(rules, vardom)
  if (length(aud$dropped)) {
    for (line in aud$dropped) {
      patches <- append(patches, list(list(type="drop_rule_text", text=line, reason="fuori dominio/duplicato")))
    }
  }

  # C) Copertura formale: genera atteso dal DAG, aggiungi mancanti
  rules_expected <- create_rules(
    Bt, routes, items.maps = items.map.file, path.items.maps = items.map.path,
    path_output = NULL, condizione_intervallo = TRUE
  )
  exp <- rules_dedup_sanitize(rules_expected, vardom)
  cur <- rules_dedup_sanitize(rules, vardom)
  missing <- setdiff(exp$rules, cur$rules)
  if (length(missing)) {
    for (r in missing) patches <- append(patches, list(list(type="add_rule", rule=r, reason="copertura DAG")))
  }

  # D) Routing_check: segnala variabili “calde” per blank policy (nota non vincolante)
  if (!is.null(res$full_report) && nrow(res$full_report)>0) {
    freq <- aggregate(errcode ~ varname_label, data=res$full_report, FUN=function(x) sum(x %in% c(-1,1)))
    freq <- freq[order(freq$errcode, decreasing=TRUE),]
    hot  <- head(freq$varname_label, 10)
    if (length(hot)) notes <- c(notes, paste("Variabili con molti errori missing/blank:", paste(hot, collapse=", ")))
  }

  # Score: meno patch ? score più alto
  score <- max(0, 1 - length(patches)/max(1, nrow(vardom)/8))
  list(score=unname(score), patches=patches, notes=paste(notes, collapse=" | "))
}
