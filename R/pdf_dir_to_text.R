# ============================================================
# FUNCTION: pdf_dir_to_text
# ============================================================
# Purpose : Extract text from PDF files located in a directory.
# Inputs  : pdf_dir
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

pdf_dir_to_text <- function(pdf_dir) {
  files <- list.files(pdf_dir, pattern="\\.pdf$", ignore.case=TRUE, full.names=TRUE)
  out <- tibble()
  for (f in files) {
    ptxt <- pdftools::pdf_text(f)
    if (length(ptxt)) {
      df <- tibble(pdf=basename(f), page=seq_along(ptxt), text_raw=ptxt, text=nl_norm(ptxt))
      out <- bind_rows(out, df)
    }
  }
  out
}
