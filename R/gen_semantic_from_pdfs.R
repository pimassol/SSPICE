# ============================================================
# FUNCTION: gen_semantic_from_pdfs
# ============================================================
# Purpose : Generate semantic consistency rules from questionnaire PDF documents.
# Inputs  : pdf_dir, routes, Bt, vardom, items.map.file, items.map.path, output.path
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

gen_semantic_from_pdfs <- function(pdf_dir, routes, Bt, vardom,
                                   items.map.file, items.map.path,
                                   output.path) {
  vnames <- colnames(Bt)
  ETA       <- guess_var(vnames, "eta")
  SESSO     <- guess_var(vnames, "sesso")
  STATO_CIV <- guess_var(vnames, "stato_civ")
  if (is.na(ETA))   cli_alert_warning("Variabile ETA non trovata.")
  if (is.na(SESSO)) cli_alert_warning("Variabile SESSO non trovata.")
  if (is.na(STATO_CIV)) cli_alert_warning("Variabile STATO_CIV non trovata.")

  txt <- pdf_dir_to_text(pdf_dir)
  if (!nrow(txt)) { cli_alert_warning("Nessun PDF o nessun testo estratto."); return(invisible(NULL)) }

  cand <- tibble()
  for (i in seq_len(nrow(txt))) {
    row <- txt[i,]; t <- row$text

    # T1 — età < th ? vieta certi STATO_CIV
    if (!is.na(ETA) && !is.na(STATO_CIV)) {
      gates <- detect_age_gates(t)
      if (length(gates)) {
        sc_codes <- parse_option_codes(row$text_raw)
        forbid <- c()
        for (lab in c("coniugato","div_sep","vedovo")) {
          if (!is.null(sc_codes[lab])) {
            code <- as.integer(sc_codes[lab])
            if (in_domain_var_item(vardom, STATO_CIV, code)) forbid <- c(forbid, code)
          }
        }
        for (th in gates) {
          cand <- bind_rows(cand, tibble(
            id = sprintf("SEM_AGE_CIV_%s_%s_p%02d_%02d", row$pdf, STATO_CIV, row$page, th),
            if_var = ETA, op = "<", val = th, val2 = NA_integer_,
            forbid_var = STATO_CIV, forbid_vals = paste(forbid, collapse=";"),
            source_pdf = row$pdf, page = row$page,
            confidence = if (length(forbid)) 0.9 else 0.5,
            snippet = substr(t, 1, 220)
          ))
        }
      }
    }

    # T2 — scope “solo donne/uomini” ? vincoli su SESSO (nota: non inferiamo il verso 1/2)
    if (!is.na(SESSO)) {
      gsc <- detect_gender_scopes(t)
      if (isTRUE(gsc$only_female) || isTRUE(gsc$only_male)) {
        # qui proponiamo vincolo generico sul dominio (da rifinire col mapping sesso)
        dom_codes <- unique(na.omit(c(1L, 2L)))
        dom_codes <- dom_codes[vapply(dom_codes, function(c) in_domain_var_item(vardom, SESSO, c), FALSE)]
        cand <- bind_rows(cand, tibble(
          id = sprintf("SEM_GEN_SCOPE_%s_%s_p%02d", if (gsc$only_female) "F" else "M", row$pdf, row$page),
          if_var = NA_character_, op = NA_character_, val = NA_integer_, val2 = NA_integer_,
          forbid_var = SESSO, forbid_vals = paste(dom_codes, collapse=";"),
          source_pdf = row$pdf, page = row$page,
          confidence = 0.4,
          snippet = if (gsc$only_female) "[solo donne]" else "[solo uomini]"
        ))
      }
    }
  }

  if (!nrow(cand)) { cli_alert_warning("Nessun candidato semantico individuato."); return(invisible(NULL)) }
  cand <- cand |> mutate(confidence = round(confidence,2)) |> arrange(desc(confidence), source_pdf, page)
  out_csv <- file.path(output.path, "REGOLE_SEMANTICHE.csv")
  readr::write_csv(cand, out_csv)

  # Compila .dat minimo solo per righe con condizione completa e codici validi
  mk_cond <- function(if_var, op, val, val2) {
    if (is.na(if_var) || is.na(op) || is.na(val)) return(NULL)
    if (op == "between" && !is.na(val2)) return(sprintf("%s(%s-%s)", if_var, val, val2))
    if (op %in% c("<",">","<=",">=","==")) sprintf("%s(%s%s)", if_var, op, val) else sprintf("%s(%s)", if_var, val)
  }
  dat_lines <- character(0)
  for (k in seq_len(nrow(cand))) {
    r <- cand[k,]
    if (nzchar(r$forbid_var) && nzchar(r$forbid_vals)) {
      cond <- mk_cond(r$if_var, r$op, r$val, r$val2)
      if (is.null(cond)) next
      for (vv in strsplit(r$forbid_vals, ";", fixed=TRUE)[[1]]) {
        vv <- as.integer(vv); if (is.na(vv)) next
        if (in_domain_var_item(vardom, r$forbid_var, vv)) {
          dat_lines <- c(dat_lines, sprintf("%s %s(%d)< )", cond, r$forbid_var, vv))
        }
      }
    }
  }
  out_dat <- file.path(output.path, "REGOLE_SEMANTICHE.dat")
  if (length(dat_lines)) writeLines(unique(dat_lines), out_dat, useBytes=TRUE)
  list(csv = out_csv, dat = out_dat, n_candidates = nrow(cand))
}
