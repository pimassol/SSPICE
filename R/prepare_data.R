#' Prepare record-level data for SSPICE validation
#'
#' Creates auxiliary variables, coarsens logical OR and XOR groups, converts
#' the resulting variables to the numeric representation used by SSPICE, and
#' verifies their alignment with a graph created by [build_DAG()].
#'
#' @details
#' \code{data_original} retains the input; \code{data_with_VAUX},
#' \code{data_with_VOR}, and \code{data_with_VOR_and_VXOR} retain intermediate
#' representations. \code{vaux_metadata} holds auxiliary rules and
#' \code{varnames} gives the aligned names. Use \code{result$data} for checking.
#' Text can become presence indicators and logical-group members can become
#' canonical nodes; the final data are not a lossless copy of the questionnaire.
#'
#' @param data Either a data frame or the path to a CSV file containing the
#'   record-level data.
#' @param dag Either an object created by [build_DAG()] or the path to an RDS
#'   file containing that object.
#' @param vaux Either `NULL`, a data frame containing VAUX rules, or the path
#'   to a delimited VAUX metadata file.
#' @param external_data_dir Directory containing external CSV files referenced
#'   by the VAUX metadata.
#' @param data_separator Field separator used when `data` is a file path.
#' @param metadata_separator Field separator used when `vaux` is a file path.
#' @param na_strings Character values interpreted as missing values.
#'
#' @return An object of class `sspice_prepared_data`. Its `data` element is
#'   the final numeric data frame aligned with the DAG. Intermediate results
#'   are retained in the remaining elements.
#'
#' @examples
#' routes <- data.frame(V1 = c("ID", "Q", "END"),
#'   V2 = c(1, 2, 1), V3 = c(1, 1, 1), V4 = c(9999, 2, 1),
#'   V5 = 0, V6 = 0, V7 = NA, V8 = NA)
#' dag <- build_DAG(routes, analyze = FALSE)
#' x <- data.frame(ID = 1:2, Q = c(1, 2), END = c(1, 1))
#' prepared <- prepare_data(x, dag)
#' prepared$data
#'
#' @export
prepare_data <- function(
  data,
  dag,
  vaux = NULL,
  external_data_dir = file.path("data-raw", "external"),
  data_separator = ",",
  metadata_separator = ";",
  na_strings = c("", ".", "NA", "NULL")
) {
  if (missing(data)) {
    stop("data must be a file path or a data frame.")
  }

  if (missing(dag)) {
    stop("dag must be an SSPICE DAG object or an RDS file path.")
  }

  if (is.character(data) && length(data) == 1L) {
    if (!file.exists(data)) {
      stop(paste0("Record-level data file not found: ", data))
    }

    data_original <- utils::read.csv(
      file = data,
      stringsAsFactors = FALSE,
      header = TRUE,
      sep = data_separator,
      na.strings = na_strings,
      check.names = FALSE
    )
  } else if (is.data.frame(data)) {
    data_original <- data
  } else {
    stop("data must be a single file path or a data frame.")
  }

  if (is.character(dag) && length(dag) == 1L) {
    if (!file.exists(dag)) {
      stop(paste0("DAG file not found: ", dag))
    }

    dag_object <- readRDS(dag)
  } else if (is.list(dag)) {
    dag_object <- dag
  } else {
    stop("dag must be an SSPICE DAG object or a single RDS file path.")
  }

  required_dag_elements <- c(
    "routes_original",
    "routes_grouped_OR",
    "varnames_original",
    "varnames"
  )

  missing_dag_elements <- setdiff(
    required_dag_elements,
    names(dag_object)
  )

  if (length(missing_dag_elements) > 0L) {
    stop(
      paste0(
        "The DAG object is missing required elements: ",
        paste(missing_dag_elements, collapse = ", "),
        "."
      )
    )
  }

  routes_original <- dag_object$routes_original
  varnames_original <- as.character(dag_object$varnames_original)
  varnames_grouped_OR <- as.character(dag_object$routes_grouped_OR$V1)
  varnames <- as.character(dag_object$varnames)

  vaux_metadata <- NULL
  data_with_VAUX <- data_original

  if (!is.null(vaux)) {
    if (is.character(vaux) && length(vaux) == 1L) {
      if (!file.exists(vaux)) {
        stop(paste0("VAUX metadata file not found: ", vaux))
      }

      vaux_na_strings <- setdiff(
        na_strings,
        "NULL"
      )

      vaux_metadata <- utils::read.csv(
        file = vaux,
        stringsAsFactors = FALSE,
        header = FALSE,
        sep = metadata_separator,
        na.strings = vaux_na_strings,
        check.names = FALSE
      )

      names(vaux_metadata) <- paste0(
        "V",
        seq_len(ncol(vaux_metadata))
      )
    } else if (is.data.frame(vaux)) {
      vaux_metadata <- vaux

      if (!all(paste0("V", 1:9) %in% names(vaux_metadata))) {
        if (ncol(vaux_metadata) < 9L) {
          stop("VAUX metadata must contain at least nine columns.")
        }

        names(vaux_metadata) <- paste0(
          "V",
          seq_len(ncol(vaux_metadata))
        )
      }
    } else {
      stop("vaux must be NULL, a single file path, or a data frame.")
    }

    if (ncol(vaux_metadata) < 9L) {
      stop("VAUX metadata must contain at least nine columns.")
    }

    data_with_VAUX <- create_vaux_variables(
      inpdata = data_original,
      vauxs_data = vaux_metadata,
      extDFpath = external_data_dir,
      routing_vnames = varnames_original
    )
  } else {
    missing_original_variables <- setdiff(
      varnames_original,
      names(data_with_VAUX)
    )

    if (length(missing_original_variables) > 0L) {
      stop(
        paste0(
          "Variables required by the ungrouped routing are missing: ",
          paste(missing_original_variables, collapse = ", "),
          "."
        )
      )
    }

    data_with_VAUX <- data_with_VAUX[
      ,
      varnames_original,
      drop = FALSE
    ]
  }

  data_with_VOR <- generate_groups_OR(
    data = data_with_VAUX,
    routing_file_ungrouped = routes_original,
    routing_file_grouped = varnames_grouped_OR
  )

  data_with_VOR_and_VXOR <- generate_groups_XOR(
    data = data_with_VOR,
    routing_file_ungrouped = routes_original,
    routing_file_grouped = varnames,
    strict = FALSE
  )

  data_clean <- clean_data(
    data_with_VOR_and_VXOR
  )

  if (!identical(names(data_clean), varnames)) {
    missing_variables <- setdiff(varnames, names(data_clean))
    unexpected_variables <- setdiff(names(data_clean), varnames)

    stop(
      paste0(
        "Prepared data columns do not match the DAG variables. ",
        "Missing: ",
        paste(missing_variables, collapse = ", "),
        "; unexpected: ",
        paste(unexpected_variables, collapse = ", "),
        "."
      )
    )
  }

  result <- list(
    data = data_clean,
    data_original = data_original,
    vaux_metadata = vaux_metadata,
    data_with_VAUX = data_with_VAUX,
    data_with_VOR = data_with_VOR,
    data_with_VOR_and_VXOR = data_with_VOR_and_VXOR,
    varnames = varnames
  )

  class(result) <- c("sspice_prepared_data", "list")
  result
}

#' Print prepared SSPICE data
#'
#' @param x An object created by [prepare_data()].
#' @param ... Additional arguments currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.sspice_prepared_data <- function(x, ...) {
  cat("SSPICE prepared data\n")
  cat("  Records:", nrow(x$data), "\n")
  cat("  Variables:", ncol(x$data), "\n")
  cat("  VAUX metadata:", !is.null(x$vaux_metadata), "\n")
  invisible(x)
}
