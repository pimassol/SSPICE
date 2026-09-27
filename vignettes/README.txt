SSPICE 0.1.0 — sette vignette R Markdown.
Copiare la directory vignettes/ nella radice del pacchetto SSPICE.
DESCRIPTION include già knitr e rmarkdown in Suggests.
Dalla radice del pacchetto: devtools::build_vignettes() oppure R CMD build .
I blocchi che richiedono dati del progetto sono intenzionalmente eval=FALSE; eseguirli interattivamente nella radice del progetto dopo aver verificato la configurazione.
