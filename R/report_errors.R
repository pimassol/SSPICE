# ============================================================
# FUNCTION: report_errors
# ============================================================
# Purpose:
#   Convert routing errors into a readable report without assuming a fixed
#   record identifier name.
# ============================================================

report_errors <- function(route_errs, vn_routes, varkey = NULL) {
  required_columns <- c(
    "id_recprd",
    "errcode",
    "varname",
    "valname",
    "routevar",
    "routeval"
  )

  missing_columns <- setdiff(
    required_columns,
    names(route_errs)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "route_errs is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      )
    )
  }

  if (is.null(varkey) || length(varkey) != 1L || is.na(varkey)) {
    candidate_columns <- setdiff(
      names(route_errs),
      required_columns
    )

    varkey <- if (length(candidate_columns) > 0L) {
      candidate_columns[1L]
    } else {
      NULL
    }
  }

  if (!is.null(varkey) && !varkey %in% names(route_errs)) {
    stop(
      paste0(
        "Record identifier column not found in route_errs: ",
        varkey
      )
    )
  }

  df <- route_errs

  df$message <- ifelse(
    df$errcode == -1,
    "missing value where a filled value is required",
    ifelse(
      df$errcode == 1,
      "value present where a blank value is required",
      ifelse(
        df$errcode == -2,
        "all variables in a required logical group are missing",
        "unknown routing error"
      )
    )
  )

  safe_name <- function(index) {
    index <- suppressWarnings(as.integer(index))

    if (
      length(index) == 1L &&
      !is.na(index) &&
      index >= 1L &&
      index <= length(vn_routes)
    ) {
      return(vn_routes[index])
    }

    NA_character_
  }

  df$varname_label <- vapply(
    df$varname,
    safe_name,
    character(1)
  )

  df$routevar_label <- vapply(
    df$routevar,
    safe_name,
    character(1)
  )

  is_VOR <- !is.na(df$varname_label) &
    substr(df$varname_label, 1L, 3L) == "VOR"

  df$message[is_VOR & df$errcode < 0] <-
    "at least one variable in the OR group must be filled"

  output_columns <- c(
    "id_recprd",
    if (!is.null(varkey)) varkey,
    "errcode",
    "varname",
    "valname",
    "routevar",
    "routeval",
    "message",
    "varname_label",
    "routevar_label"
  )

  df[
    ,
    output_columns,
    drop = FALSE
  ]
}
