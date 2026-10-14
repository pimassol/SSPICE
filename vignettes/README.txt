SSPICE 0.1.0 - nine English R Markdown guides.

Start with panoramica-01.Rmd, then metadati-02.Rmd, routing-02b.Rmd and item-mapping-02c.Rmd.
Continue with dag-03, controlli-04, regole-05, correzione-06 and
qualita-correzione-07.

From R at the repository root:
  source("scripts/12_verify_documentation.R")

The root README explains how to render all nine guides to HTML.
R packages knitr and rmarkdown, and Pandoc, are required for rendering.
Operational chunks are deliberately disabled. Standalone examples explicitly
marked eval=TRUE run without the application questionnaire or external data.
