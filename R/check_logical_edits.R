#' Detect all violated logical-edit variables
#'
#' Internal helper that reports every missing logical-edit value independently
#' of the path followed through the questionnaire DAG.
#'
#' @param data Record-level data containing the logical-edit variables.
#' @param id_col Name of the record identifier column.
#' @param logical_edit_names Character vector of logical-edit variable names.
#' @return A report compatible with the full report returned by
#'   `routing_check()`.
#' @keywords internal
check_logical_edits <- function(
  data,
  id_col,
  logical_edit_names
) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }

  if (length(id_col) != 1L || is.na(id_col) || !nzchar(id_col) ||
      !(id_col %in% names(data))) {
    stop("id_col must identify one column in data.", call. = FALSE)
  }

  logical_edit_names <- unique(as.character(logical_edit_names))
  logical_edit_names <- logical_edit_names[
    !is.na(logical_edit_names) & nzchar(logical_edit_names)
  ]

  missing_edits <- setdiff(logical_edit_names, names(data))

  if (length(missing_edits) > 0L) {
    stop(
      "Logical-edit variables missing from data: ",
      paste(missing_edits, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  empty_report <- data.frame(
    id_recprd = integer(),
    record_identifier = data[[id_col]][FALSE],
    errcode = integer(),
    varname = integer(),
    valname = numeric(),
    routevar = integer(),
    routeval = numeric(),
    message = character(),
    varname_label = character(),
    routevar_label = character(),
    violation_source = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(empty_report)[2L] <- id_col

  if (length(logical_edit_names) == 0L || nrow(data) == 0L) {
    return(empty_report)
  }

  reports <- lapply(
    logical_edit_names,
    function(edit_name) {
      missing_rows <- which(is.na(data[[edit_name]]))

      if (length(missing_rows) == 0L) {
        return(NULL)
      }

      report <- data.frame(
        id_recprd = missing_rows,
        record_identifier = data[[id_col]][missing_rows],
        errcode = rep.int(-1L, length(missing_rows)),
        varname = rep.int(match(edit_name, names(data)), length(missing_rows)),
        valname = rep(NA_real_, length(missing_rows)),
        routevar = rep(NA_integer_, length(missing_rows)),
        routeval = rep(NA_real_, length(missing_rows)),
        message = rep("logical edit is violated", length(missing_rows)),
        varname_label = rep(edit_name, length(missing_rows)),
        routevar_label = rep(NA_character_, length(missing_rows)),
        violation_source = rep("LOGICAL_EDIT", length(missing_rows)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(report)[2L] <- id_col
      report
    }
  )

  reports <- Filter(Negate(is.null), reports)

  if (length(reports) == 0L) {
    return(empty_report)
  }

  result <- do.call(rbind, reports)
  row.names(result) <- NULL
  result
}
