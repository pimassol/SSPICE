# SSPICE

**Systematic Skip-Pattern Integrity and Consistency Engine** is an R package for representing and analysing logical dependencies in structured statistical data. It builds a directed acyclic graph (DAG) from metadata, prepares record-level data, detects and localises routing and logical-edit inconsistencies, and supports record correction. It also provides tools for constructing variable domains and compatibility rules and for assessing properties of the resulting Horn theory.

This repository contains SSPICE **0.1.0**, including package source code, function documentation, tests, operational scripts, vignettes, and example inputs.

## Requirements and installation

- R **4.1.0** or later.
- Git to clone the repository.
- Repository access while it remains private.

Clone the repository:

```sh
git clone https://github.com/pimassol/SSPICE.git
```

From R, in the cloned repository:

```r
setwd("path/to/SSPICE")
install.packages("remotes")
remotes::install_local(".", dependencies = TRUE, build_vignettes = FALSE)
library(SSPICE)
```

Package dependencies are declared in `DESCRIPTION`. Building the vignettes also requires the suggested vignette packages and Pandoc.

## Repository layout

| Path | Contents |
| --- | --- |
| `R/` | Package functions |
| `man/` | Function reference files |
| `vignettes/` | Guides to the workflow, metadata, DAG, validation, correction, and correction quality |
| `scripts/` | Numbered operational scripts, from `00_config.R` through `11_correction_quality_report.R` |
| `tests/` | Automated tests |
| `inst/extdata/SSPICE/` | Bundled example data and metadata |

The example metadata include `routing_file.csv`, `vaux_rules.csv`, `logical_edit_rules.csv`, and `items_mapping.csv`. External input files are named `extfile1.csv`, `extfile2.csv`, and `extfile3.csv`.

## Example dataset

The example questionnaire contains **15,000 original records** and
**332 columns**, including `STRATUM`. It is an input dataset before
correction, not a corrected output.

The sample retains all **4,842 records classified as exact** by the
full-data extended-DAG check and selects **10,158 initially erroneous
records**, proportionally by `FPROV`, using seed 123. Original row
order and the CSV representation of selected records are retained.

External sources are subset using the same `DB030` identifiers:
`extfile1.csv` and `extfile3.csv` each contain 15,000 matching records;
`extfile2.csv` contains 14,988. The 12 missing links already exist in
the source and were not introduced by sampling.

Working inputs in `data-raw/` and bundled inputs in
`inst/extdata/SSPICE/` are identical at preparation time.

The eight English vignettes include
`vignettes/routing-02b.Rmd`, covering routing fields, variable types,
conditional and unconditional jumps, and OR/XOR groups.

## Prepare inputs for the operational scripts

Run the numbered scripts from the **repository root**. They resolve input and output paths through `scripts/00_config.R`. The bundled example files are stored under `inst/extdata/SSPICE/`, while the scripts expect a working copy under `data-raw/`. Create that working copy once from R:

```r
setwd("path/to/SSPICE")

example_dir <- file.path("inst", "extdata", "SSPICE")
stopifnot(dir.exists(example_dir))

for (folder in c("metadata", "input", "external")) {
  from <- file.path(example_dir, folder)
  to <- file.path("data-raw", folder)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(from, full.names = TRUE)
  if (length(files)) {
    stopifnot(all(file.copy(files, to, overwrite = TRUE)))
  }
}
```

This creates working copies without changing the files in `inst/extdata/SSPICE/`. Outputs are written to locations specified in the configuration, generally under `output/`. Some validation and correction scripts require outputs from earlier steps or additional inputs; inspect `scripts/00_config.R` before running the full workflow.

Start with `vignettes/panoramica-01.Rmd` for the overview and `vignettes/metadati-02.Rmd` for the metadata. The remaining guides follow the numbers in their filenames.

## Run the tests

From R, at the repository root:

```r
install.packages("testthat")
testthat::test_local(".", reporter = "summary")
```

A previous revision was cloned, tested, and installed from a fresh checkout on Windows with R 4.4.3. The updated dataset and documentation require validation for this revision; the earlier check does not establish compatibility with every operating system or R version.

## License

GPL (>= 3), as declared in `DESCRIPTION`.
