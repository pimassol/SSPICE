#' Localize violations detected by SSPICE
#'
#' Internal engine that converts the complete output of `routing_check()` into
#' localization information for validation and iterative repair. An existing
#' routing-check result is reused without dropping logical-edit violations.
#'
#' @param data Optional record-level data. Required when `routing_results` is
#'   `NULL`; also used to retain consistent records in the summary.
#' @param dag Optional DAG object produced by `build_DAG()`. Required when
#'   `routing_results` is `NULL`.
#' @param id_col Optional record-identifier column name.
#' @param items_mapping_file Optional path to `items_mapping.csv`.
#' @param routing_results Optional result returned by `routing_check()`.
#' @return An object of class `sspice_violation_localization`.
#' @keywords internal
localize_routing_violations <- function(
  data = NULL,
  dag = NULL,
  id_col = NULL,
  items_mapping_file = NULL,
  routing_results = NULL
) {
  if (is.null(routing_results)) {
    if (is.null(data) || !is.data.frame(data)) {
      stop(
        "data must be supplied as a data frame when routing_results is NULL.",
        call. = FALSE
      )
    }

    if (is.null(dag) || !is.list(dag)) {
      stop("dag must be supplied when routing_results is NULL.", call. = FALSE)
    }

    required_dag_elements <- c("routes_numeric", "Bt", "varnames")
    missing_dag_elements <- setdiff(required_dag_elements, names(dag))

    if (length(missing_dag_elements) > 0L) {
      stop(
        "The DAG object is missing: ",
        paste(missing_dag_elements, collapse = ", "),
        call. = FALSE
      )
    }

    if (is.null(id_col) || length(id_col) != 1L || is.na(id_col) ||
        !nzchar(id_col) || !(id_col %in% names(data))) {
      stop("A valid id_col is required.", call. = FALSE)
    }

    if (is.null(items_mapping_file) ||
        length(items_mapping_file) != 1L ||
        !file.exists(items_mapping_file)) {
      stop(
        "A valid items_mapping_file is required when routing_results is NULL.",
        call. = FALSE
      )
    }

    logical_edit_names <- if (
      "logical_edit_names" %in% names(dag)
    ) {
      dag$logical_edit_names
    } else {
      NULL
    }

    routing_results <- routing_check(
      dset_data = data,
      routes = dag$routes_numeric,
      Bt = dag$Bt,
      varkey = id_col,
      items.maps = basename(items_mapping_file),
      path.items.maps = dirname(items_mapping_file),
      logical_edit_names = logical_edit_names
    )
  }

  if (!is.list(routing_results) ||
      !("full_report" %in% names(routing_results)) ||
      !is.data.frame(routing_results$full_report)) {
    stop(
      "routing_results is not a valid result returned by routing_check().",
      call. = FALSE
    )
  }

  if (is.null(id_col) && "record_id" %in% names(routing_results)) {
    id_col <- routing_results$record_id
  }

  if (is.null(id_col) || length(id_col) != 1L || is.na(id_col) ||
      !nzchar(id_col)) {
    stop("A valid id_col could not be determined.", call. = FALSE)
  }

  full_report <- routing_results$full_report
  required_report_columns <- c(
    "errcode", "varname", "valname", "routevar", "routeval"
  )
  missing_report_columns <- setdiff(required_report_columns, names(full_report))

  if (length(missing_report_columns) > 0L) {
    stop(
      "The routing-check report is missing: ",
      paste(missing_report_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!(id_col %in% names(full_report))) {
    stop(
      "The record identifier '", id_col,
      "' is absent from the routing-check report.",
      call. = FALSE
    )
  }

  detail <- full_report
  detail$record_id <- detail[[id_col]]
  detail$localized_var <- if ("varname_label" %in% names(detail)) {
    as.character(detail$varname_label)
  } else {
    as.character(detail$varname)
  }
  detail$source_var <- if ("routevar_label" %in% names(detail)) {
    as.character(detail$routevar_label)
  } else {
    as.character(detail$routevar)
  }

  logical_rows <- if ("violation_source" %in% names(detail)) {
    detail$violation_source == "LOGICAL_EDIT"
  } else {
    rep(FALSE, nrow(detail))
  }
  logical_rows[is.na(logical_rows)] <- FALSE

  detail$violation_type <- .sspice_routing_violation_type(detail$errcode)
  detail$violation_type[logical_rows] <- "LOGICAL_EDIT_VIOLATION"
  detail$expected_status <- ifelse(
    logical_rows,
    "VALUE_1_OR_2",
    ifelse(
      detail$errcode %in% c(-1, -2),
      "FILLED",
      ifelse(detail$errcode == 1, "BLANK", NA_character_)
    )
  )
  detail$observed_status <- ifelse(
    logical_rows,
    "BLANK",
    ifelse(
      detail$errcode %in% c(-1, -2),
      "BLANK",
      ifelse(detail$errcode == 1, "FILLED", NA_character_)
    )
  )

  all_ids <- if (!is.null(data) && is.data.frame(data) &&
                 id_col %in% names(data)) {
    data[[id_col]]
  } else {
    unique(detail$record_id)
  }

  summary_by_record <- .sspice_localization_summary(all_ids, detail)

  result <- list(
    detail = detail,
    summary_by_record = summary_by_record,
    routing_results = routing_results,
    record_id = id_col
  )
  class(result) <- c("sspice_violation_localization", "list")
  result
}

.sspice_routing_violation_type <- function(errcode) {
  result <- rep(NA_character_, length(errcode))
  result[errcode == -1] <- "TARGET_EXPECTED_FILLED"
  result[errcode == -2] <- "GROUP_EXPECTED_FILLED"
  result[errcode == 1] <- "SKIP_EXPECTED_BLANK"
  result
}

.sspice_localization_summary <- function(ids, detail) {
  ids <- unique(ids)
  by_record <- if (nrow(detail) > 0L) {
    split(detail, as.character(detail$record_id))
  } else {
    list()
  }

  result <- do.call(
    rbind,
    lapply(
      ids,
      function(id) {
        one_record <- by_record[[as.character(id)]]

        if (is.null(one_record)) {
          data.frame(
            record_id = id,
            predicted_inconsistent = FALSE,
            n_localized_violations = 0L,
            localized_variables = "",
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            record_id = id,
            predicted_inconsistent = TRUE,
            n_localized_violations = nrow(one_record),
            localized_variables = paste(
              sort(unique(one_record$localized_var)),
              collapse = " | "
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    )
  )

  row.names(result) <- NULL
  result
}
