# Run from the package root after applying the mapping patch.
# Synthetic records only: no application correction or publication.
local({
  if (!file.exists("DESCRIPTION") || !dir.exists("R")) stop("Run from the SSPICE package root.")
  if (!requireNamespace("pkgload", quietly = TRUE) ||
      !requireNamespace("testthat", quietly = TRUE)) stop("pkgload and testthat are required.")
  files <- unique(c(
    list.files("R", full.names = TRUE, recursive = TRUE),
    list.files("man", full.names = TRUE, recursive = TRUE),
    list.files("data-raw/metadata", full.names = TRUE, recursive = TRUE),
    list.files("inst/extdata/SSPICE/metadata", full.names = TRUE, recursive = TRUE)
  ))
  files <- files[!dir.exists(files)]
  hashes <- tools::md5sum(files)
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
  for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) invisible(parse(file = f))
  for (f in list.files("man", pattern = "[.]Rd$", full.names = TRUE)) invisible(tools::parse_Rd(f))
  fn <- function(name) getFromNamespace(name, "SSPICE")
  mapping_file <- "data-raw/metadata/items_mapping.csv"
  mapping <- fn(".sspice_read_item_mapping")(mapping_file)
  fase <- mapping[mapping$variable == "FASE",,drop = FALSE]
  fase <- fase[order(fase$new),,drop = FALSE]
  n <- nrow(fase)
  stopifnot(n > 1L)
  routes <- data.frame(V1 = c("ID","FASE","END"), V2 = c(1,n,1),
    V3 = c(1,min(fase$new),1),V4 = c(9999,max(fase$new),1),
    V5 = 0,V6 = 0,V7 = NA,V8 = NA)
  dag <- SSPICE::build_DAG(routes, analyze = FALSE)
  data <- data.frame(ID = seq_len(n),FASE = fase$old,END = 1)
  oracle <- fn(".sspice_build_routing_oracle")(dag,"ID",items_mapping_file = mapping_file)
  actual <- vapply(seq_len(n),function(i) {
    dummy <- fn(".sspice_oracle_dummy")(data[i,,drop = FALSE],oracle)
    active <- which(dummy[seq.int(2L,n+1L)] == 1L)
    if (length(active) == 1L) as.integer(active) else NA_integer_
  },integer(1))
  checker <- fn("check_all_paths")
  probe <- new.env(parent = environment(checker)); probe$positions <- integer()
  original_dummy <- fn("dummize_vector")
  probe$dummize_vector <- function(x_row,y) {
    dummy <- original_dummy(x_row,y)
    active <- which(dummy[seq.int(2L,n+1L)] == 1L)
    probe$positions <- c(probe$positions,if (length(active) == 1L) active else NA_integer_)
    dummy
  }
  environment(checker) <- probe
  invisible(checker(data,dag$routes_numeric,dag$Bt,"ID",basename(mapping_file),dirname(mapping_file)))
  print(data.frame(FASE_originale = data$FASE,codice_routing = fase$new,
                   posizione_attesa = seq_len(n),check = probe$positions,oracolo = actual),row.names = FALSE)
  stopifnot(identical(actual,seq_len(n)),identical(probe$positions,seq_len(n)))
  cat("\nFASE: entrambe le codifiche corrette.\n")
  # The full existing suite guards strings, groups, correction, caches and exports;
  # it also includes the new independent mapping regression cases.
  testthat::test_local(".", reporter = "summary", stop_on_failure = TRUE)
  source("scripts/12_verify_documentation.R", local = new.env(parent = globalenv()))
  stopifnot(identical(hashes,tools::md5sum(files)))
  cat("\nVerifica mapping e regressioni completata. Sorgenti e metadati invariati dai test.\n")
})
