#' SSPICE: Systematic Skip-Pattern Integrity and Consistency Engine
#'
#' A graph-based framework for representing, analyzing, validating,
#' and correcting structured statistical data governed by explicit
#' logical dependencies.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_h1
#' @importFrom dplyr arrange bind_rows desc mutate
#' @importFrom jsonlite toJSON
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_match_all
#' @importFrom tibble tibble
#' @keywords internal
#' @importFrom stats aggregate na.omit setNames
#' @importFrom utils head read.csv tail write.table
"_PACKAGE"
