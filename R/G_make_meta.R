# ============================================================
# FUNCTION: G_make_meta
# ============================================================
# Purpose : Assemble metadata used by the generator stage of the audit pipeline.
# Inputs  : routes, Bt, A, vardom, rules, res, pdf_dir
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

G_make_meta <- function(routes, Bt, A, vardom, rules, res, pdf_dir = NULL) {
  vnames <- colnames(Bt)
  n_vars <- length(vnames); n_edges <- nrow(Bt)
  dag_ok <- is_DAG(A)
  spath  <- try(find_shortest_path(A, 1, nrow(A), vnames), silent = TRUE)
  lpath  <- try(find_longest_path(A, vnames), silent = TRUE)

  n_err  <- if (!is.null(res$full_report)) nrow(res$full_report) else 0
  top_var_err <- if (!is.null(res$summary_by_var) && nrow(res$summary_by_var)>0)
    head(res$summary_by_var, 5) else NULL

  rout_min <- routes[,3]; rout_max <- routes[,4]
  vard_min <- suppressWarnings(as.numeric(vardom$minitem))
  vard_max <- suppressWarnings(as.numeric(vardom$maxitem))
  domain_mismatch <- which((!is.na(rout_min) & !is.na(vard_min) & rout_min != vard_min) |
                           (!is.na(rout_max) & !is.na(vard_max) & rout_max != vard_max))
  domain_mismatch_names <- vnames[domain_mismatch]
  blanks_implicit <- sum(vardom$blank_implicit == 1, na.rm = TRUE)
  blanks_allowed  <- sum(vardom$blank_allowed  == 1, na.rm = TRUE)
  pdfs <- if (!is.null(pdf_dir) && dir.exists(pdf_dir))
    list.files(pdf_dir, pattern="\\.pdf$", ignore.case=TRUE) else character(0)

  claims <- list(
    list(id="C1", text=paste("Grafo DAG:", dag_ok), needs_citation=FALSE),
    list(id="C2", text=paste("Variabili:", n_vars), needs_citation=FALSE),
    list(id="C3", text=paste("Archi:", n_edges), needs_citation=FALSE),
    list(id="C4", text=paste("Errori routing_check:", n_err), needs_citation=FALSE),
    list(id="C5", text=paste("Mismatch domini VARDOM vs ROUTES:", length(domain_mismatch)), needs_citation=FALSE),
    list(id="C6", text=paste("Blank implicit/allowed:", blanks_implicit, "/", blanks_allowed), needs_citation=FALSE),
    list(id="C7", text=paste("Regole totali:", length(rules)), needs_citation=FALSE)
  )
  uncertainties <- if (!dag_ok) list(list(where="DAG", reason="cicli/ordine topologico non valido")) else list()
  todo <- c()
  if (length(domain_mismatch)) todo <- c(todo, "Allinea min/max VARDOM a ROUTES")
  if (n_err > 0)               todo <- c(todo, "Risolvi errori routing_check")

  A_text <- paste0(
    "Stato questionario:\n",
    "- DAG: ", dag_ok, "\n",
    "- Variabili: ", n_vars, "\n",
    "- Archi: ", n_edges, "\n",
    "- Errori (routing_check): ", n_err, "\n",
    "- Mismatch domini VARDOM: ", length(domain_mismatch),
    if (length(domain_mismatch)) paste0(" (es. ", paste(head(domain_mismatch_names,3), collapse=", "), ")") else "", "\n",
    "- Regole: ", length(rules), "\n",
    if (length(pdfs)) paste0("- PDF trovati: ", length(pdfs)) else ""
  )
  B <- list(
    assumptions = list("Bt e ROUTES coerenti con i file sorgente","Mapping items applicato"),
    claims = claims, calcs = list(), uncertainties = uncertainties,
    todo_for_critic = unname(todo),
    artifacts = list(domain_mismatch = domain_mismatch_names, pdf_list = pdfs,
                     top_var_err = if (!is.null(top_var_err)) top_var_err else data.frame())
  )
  list(A_text = A_text, B_json = toJSON(B, auto_unbox=TRUE))
}
