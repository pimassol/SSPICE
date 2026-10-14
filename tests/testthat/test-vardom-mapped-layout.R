testthat::test_that("mapped VARDOM widths and positions agree", {
  verifica <-
function (generatore = getFromNamespace("create_vardom", "SSPICE")) 
{
    cartella <- tempfile("vardom-layout-")
    dir.create(cartella)
    on.exit(unlink(cartella, recursive = TRUE), add = TRUE)
    casi <- list(BASE = list(route = 1:3, osservati = 1:3, mapping = FALSE), MAPPING_123 = list(route = 1:3, 
        osservati = c(10, 21, 99), mapping = TRUE), MAPPING_10_30 = list(route = c(10, 20, 30), osservati = c(10, 
        21, 99), mapping = TRUE), CODICI_PIU_CORTI = list(route = c(10, 20, 30), osservati = 1:3, mapping = TRUE))
    risultati <- list()
    for (nome in names(casi)) {
        z <- casi[[nome]]
        file <- file.path(cartella, paste0(nome, ".csv"))
        m <- data.frame(varname = character(), item_route = numeric(), item_dataset = numeric())
        if (z$mapping) {
            m <- data.frame(varname = "CHOICE", item_route = z$route, item_dataset = z$osservati)
        }
        write.csv(m, file, row.names = FALSE)
        routes <- data.frame(V1 = c("ID", "CHOICE", "DETAIL", "END"), V2 = c(1, 3, 1, 1), V3 = c(1, min(z$route), 
            1, 1), V4 = c(9999, max(z$route), 1, 1), V5 = 0, V6 = 0, V7 = NA, V8 = NA)
        dag <- SSPICE::build_DAG(routes, analyze = FALSE)
        risultato <- generatore(dag$routes_numeric, dag$Bt, basename(file), dirname(file))
        v <- risultato$vardom
        layout <- risultato$record_layout
        variabili <- c("ID", "CHOICE", "DETAIL", "END")
        stopifnot(identical(as.character(v$varname), variabili), identical(as.character(layout$varname), 
            variabili))
        larghezza <- if (max(z$osservati) == 99) 
            2L
        else 1L
        lunghezze_attese <- c(4L, larghezza, 1L, 1L)
        posizioni_attese <- c(1L, 5L, 5L + larghezza, 6L + larghezza)
        dominio_ok <- TRUE
        if (z$mapping) {
            intervalli_attesi <- paste0(sprintf("%04d%04d", z$osservati, z$osservati), collapse = "")
            dominio_ok <- identical(as.character(v$intervalli[v$varname == "CHOICE"]), intervalli_attesi)
        }
        ok <- dominio_ok && identical(as.integer(v$len_var), lunghezze_attese) && identical(as.integer(v$pos_i), 
            posizioni_attese) && identical(as.integer(layout$length), lunghezze_attese) && identical(as.integer(layout$position), 
            posizioni_attese)
        risultati[[nome]] <- data.frame(caso = nome, larghezza_CHOICE = v$len_var[2], larghezza_attesa = larghezza, 
            posizione_DETAIL = v$pos_i[3], posizione_attesa = posizioni_attese[3], esito = if (ok) 
                "PASS"
            else "FAIL")
    }
    tabella <- do.call(rbind, risultati)
    print(tabella, row.names = FALSE)
    invisible(all(tabella$esito == "PASS"))
}
  testthat::expect_true(verifica())
})
