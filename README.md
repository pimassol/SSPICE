# SSPICE

**Systematic Skip-Pattern Integrity and Consistency Engine** è un pacchetto R per rappresentare e analizzare le dipendenze logiche di dati statistici strutturati. Costruisce un grafo aciclico diretto (DAG) a partire dai metadati, prepara i dati, individua e localizza incoerenze di routing e di edit logici e supporta la correzione dei record. Include strumenti per costruire domini e regole di compatibilità e per verificare proprietà della teoria di Horn risultante.

Questa è la versione **0.1.0**. Il repository contiene il codice del pacchetto, la documentazione delle funzioni, test, vignette, script di lavoro e dati di esempio con nomi dei file esterni anonimizzati.

## Requisiti e installazione

- R **4.1.0** o successivo.
- Git per clonare il repository.
- Accesso al repository finché è privato.

Da un terminale:

```text
git clone https://github.com/pimassol/SSPICE.git
```

In R, dalla cartella appena clonata:

```r
setwd("percorso/alla/cartella/SSPICE")
install.packages("remotes")
remotes::install_local(".", dependencies = TRUE, build_vignettes = FALSE)
library(SSPICE)
```

Le dipendenze del pacchetto sono dichiarate in `DESCRIPTION`. Per eseguire i test e costruire le vignette occorrono anche le dipendenze suggerite; la costruzione delle vignette richiede Pandoc.

## Struttura del repository

| Percorso | Contenuto |
| --- | --- |
| `R/` | Funzioni del pacchetto |
| `man/` | Documentazione delle funzioni |
| `vignettes/` | Guide al metodo, ai metadati, al DAG, ai controlli, alla correzione e alla qualità |
| `scripts/` | Sequenza operativa numerata da `00_config.R` a `11_correction_quality_report.R` |
| `tests/` | Test automatici |
| `inst/extdata/SSPICE/` | Dati e metadati di esempio inclusi nel pacchetto |

I metadati di esempio comprendono `routing_file.csv`, `vaux_rules.csv`, `logical_edit_rules.csv` e `items_mapping.csv`. I file esterni sono denominati `extfile1.csv`, `extfile2.csv` ed `extfile3.csv`.

## Preparare i dati per gli script

Gli script usano i percorsi relativi definiti in `scripts/00_config.R` e si eseguono dalla **radice del repository**. I file distribuiti con il pacchetto si trovano invece in `inst/extdata/SSPICE/`. Per predisporre una copia di lavoro in `data-raw/`, eseguire una volta in R:

```r
setwd("percorso/alla/cartella/SSPICE")

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

Questo passaggio copia i file di esempio; non modifica quelli in `inst/extdata/SSPICE/`. I file prodotti durante le elaborazioni vengono scritti nei percorsi `output/` indicati nella configurazione. Alcuni script di validazione o correzione richiedono risultati prodotti dagli script precedenti oppure file di input aggiuntivi: controllare `scripts/00_config.R` prima di avviarli.

Per conoscere la procedura e il significato dei metadati, iniziare da `vignettes/panoramica-01.Rmd` e `vignettes/metadati-02.Rmd`. Le altre vignette seguono la numerazione nel nome.

## Verifica

Dalla radice del repository, in R:

```r
install.packages("testthat")
testthat::test_local(".", reporter = "summary")
```

La presenza dei file nel repository non sostituisce una verifica su una nuova copia clonata: è opportuno eseguire installazione e test nell'ambiente in cui si intende usare SSPICE.

## Licenza

GPL (>= 3), come indicato in `DESCRIPTION`.
