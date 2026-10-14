# Run from PROJECT/GitHub. Requires installed package dependencies and pkgload.
# No questionnaire inputs, network calls or output files are needed.
local({
  if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
    stop("Run this script from the SSPICE package root.")
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Install pkgload first.")
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
  for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
    invisible(parse(file = f))
  }
  for (f in list.files("man", pattern = "[.]Rd$", full.names = TRUE)) {
    invisible(tools::parse_Rd(f))
  }
  selected <- c("standalone-workflow", "standalone-ledit", "routing-examples",
                "standalone-item-mapping")
  seen <- character()
  for (f in list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    i <- 1L
    while (i <= length(lines)) {
      if (grepl("^```\\{r[ ,]", lines[i])) {
        label <- sub("^```\\{r[ ]*([^,} ]+).*$", "\\1", lines[i])
        end <- i + 1L
        while (end <= length(lines) && !grepl("^```[ ]*$", lines[end])) end <- end + 1L
        if (end > length(lines)) stop("Unclosed chunk: ", f)
        if (label %in% selected) {
          message("Checking ", label)
          code <- paste(lines[seq.int(i + 1L, end - 1L)], collapse = "\n")
          eval(parse(text = code), envir = new.env(parent = globalenv()))
          seen <- c(seen, label)
        }
        i <- end
      }
      i <- i + 1L
    }
  }
  stopifnot(setequal(seen, selected), !anyDuplicated(seen))
  message("All four standalone documentation examples completed.")
})
