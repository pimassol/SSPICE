#' Create logical-edit auxiliary variables
#'
#' Builds LEDIT variables from nine-column metadata. Each metadata row contains
#' one base variable. Logical and arithmetic expressions are assembled across
#' consecutive rows. LEDIT variables are appended to the input data.
#'
#' @details
#' Every edit must define categories 1 and 2; both are accepted outcomes.
#' Conditions are evaluated in file order and the first assignment wins.
#' Unassigned records retain NA. Arithmetic missing operands are replaced by
#' zero; missing logical outcomes do not match. These semantics differ from
#' VAUX arithmetic. The metadata vignette provides a complete example.
#' The returned data have a \code{logical_edit_definitions} attribute containing
#' expressions, true-condition counts and newly assigned counts.
#'
#' @param inpdata Input data frame.
#' @param logical_edits_data Nine-column data frame or semicolon-separated file.
#' @param extDFpath Directory containing external CSV sources used in V4.
#' @param separator Metadata separator.
#'
#' @return The input data with the LEDIT variables appended.
#' @keywords internal
create_logical_edit_variables <- function(
  inpdata,
  logical_edits_data,
  extDFpath = NULL,
  separator = ";"
) {
  if (!is.data.frame(inpdata)) {
    stop("inpdata must be a data frame.", call. = FALSE)
  }

  if (is.character(logical_edits_data) &&
      length(logical_edits_data) == 1L) {
    if (!file.exists(logical_edits_data)) {
      stop(
        "Logical-edit metadata file not found: ",
        logical_edits_data,
        ".",
        call. = FALSE
      )
    }

    edits <- utils::read.csv(
      logical_edits_data,
      header = FALSE,
      sep = separator,
      colClasses = "character",
      na.strings = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      strip.white = TRUE
    )
  } else if (is.data.frame(logical_edits_data)) {
    edits <- logical_edits_data
    edits[] <- lapply(edits, as.character)
  } else {
    stop(
      "logical_edits_data must be a file path or a data frame.",
      call. = FALSE
    )
  }

  if (ncol(edits) != 9L) {
    stop(
      "Logical-edit metadata must contain exactly nine columns.",
      call. = FALSE
    )
  }

  names(edits) <- paste0("V", seq_len(9L))
  edits[] <- lapply(edits, trimws)

  is_unused <- function(value) {
    is.na(value) || value == "" || value == "."
  }

  is_na_token <- function(value) {
    !is.na(value) && toupper(value) %in% c("NA", "BLANK")
  }

  required_value <- function(value, field, row_number) {
    if (is_unused(value)) {
      stop(
        field,
        " is missing in logical-edit metadata row ",
        row_number,
        ".",
        call. = FALSE
      )
    }
    value
  }

  if (any(vapply(edits$V1, is_unused, logical(1))) ||
      any(vapply(edits$V2, is_unused, logical(1))) ||
      any(vapply(edits$V3, is_unused, logical(1))) ||
      any(vapply(edits$V9, is_unused, logical(1)))) {
    stop(
      "Columns V1, V2, V3 and V9 cannot be empty.",
      call. = FALSE
    )
  }

  edit_names <- unique(edits$V1)

  if (anyDuplicated(edit_names)) {
    stop("Duplicated logical-edit names were found.", call. = FALSE)
  }

  edit_values <- split(edits$V2, edits$V1)

  invalid_domains <- vapply(
    edit_values,
    function(values) {
      !setequal(unique(as.numeric(values)), c(1, 2))
    },
    logical(1)
  )

  if (any(invalid_domains)) {
    stop(
      "Every LEDIT must define exactly the accepted domain {1, 2}: ",
      paste(names(invalid_domains)[invalid_domains], collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  original_names <- names(inpdata)
  working_data <- inpdata
  imported_names <- character(0)

  external_rows <- !vapply(edits$V4, is_unused, logical(1))

  if (any(external_rows)) {
    if (is.null(extDFpath) || !dir.exists(extDFpath)) {
      stop(
        "A valid extDFpath is required by the logical-edit metadata.",
        call. = FALSE
      )
    }

    external_specs <- unique(
      edits[external_rows, c("V4", "V5"), drop = FALSE]
    )

    for (i in seq_len(nrow(external_specs))) {
      source_name <- external_specs$V4[i]
      record_key <- required_value(
        external_specs$V5[i],
        "External record key",
        i
      )
      source_file <- file.path(
        extDFpath,
        paste0(source_name, ".csv")
      )

      if (!file.exists(source_file)) {
        stop(
          "External data file not found: ",
          source_file,
          ".",
          call. = FALSE
        )
      }

      external_data <- utils::read.csv(
        source_file,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      variables_needed <- unique(
        edits$V3[
          external_rows &
            edits$V4 == source_name &
            edits$V5 == record_key
        ]
      )

      missing_columns <- setdiff(
        c(record_key, variables_needed),
        names(external_data)
      )

      if (length(missing_columns) > 0L) {
        stop(
          "Columns missing from external source ",
          source_name,
          ": ",
          paste(missing_columns, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      if (!(record_key %in% names(working_data))) {
        stop(
          "Record key missing from input data: ",
          record_key,
          ".",
          call. = FALSE
        )
      }

      variables_needed <- setdiff(
        variables_needed,
        names(working_data)
      )

      external_subset <- external_data[
        ,
        c(record_key, variables_needed),
        drop = FALSE
      ]

      if (anyDuplicated(external_subset[[record_key]])) {
        stop(
          "External record key is not unique in ",
          source_file,
          ".",
          call. = FALSE
        )
      }

      working_data$.sspice_row_order <- seq_len(nrow(working_data))
      working_data <- merge(
        working_data,
        external_subset,
        by = record_key,
        all.x = TRUE,
        sort = FALSE
      )
      working_data <- working_data[
        order(working_data$.sspice_row_order),
        ,
        drop = FALSE
      ]
      working_data$.sspice_row_order <- NULL
      row.names(working_data) <- NULL
      imported_names <- unique(c(imported_names, variables_needed))
    }
  }

  missing_base_variables <- setdiff(
    unique(edits$V3),
    names(working_data)
  )

  if (length(missing_base_variables) > 0L) {
    stop(
      "Variables required by logical edits are missing: ",
      paste(missing_base_variables, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  value_literal <- function(value) {
    numeric_value <- suppressWarnings(as.numeric(value))

    if (!is.na(numeric_value)) {
      return(format(numeric_value, scientific = FALSE, trim = TRUE))
    }

    if (toupper(value) %in% c("TRUE", "FALSE")) {
      return(toupper(value))
    }

    encodeString(value, quote = "\"")
  }

  make_predicate <- function(variable, exact, minimum, maximum) {
    variable_text <- paste0("(`", variable, "`)")

    if (!is_unused(exact)) {
      if (is_na_token(exact)) {
        return(paste0("is.na", variable_text))
      }

      return(
        paste0(variable_text, " == ", value_literal(exact))
      )
    }

    minimum_used <- !is_unused(minimum)
    maximum_used <- !is_unused(maximum)

    if (!minimum_used && !maximum_used) {
      return(NULL)
    }

    minimum_is_na <- minimum_used && is_na_token(minimum)
    maximum_is_na <- maximum_used && is_na_token(maximum)

    if (minimum_is_na && maximum_is_na) {
      return(paste0("is.na", variable_text))
    }

    comparisons <- character(0)

    if (minimum_is_na) {
      comparisons <- c(comparisons, paste0("is.na", variable_text))
    } else if (minimum_used) {
      comparisons <- c(
        comparisons,
        paste0(variable_text, " >= ", value_literal(minimum))
      )
    }

    if (maximum_is_na) {
      comparisons <- c(comparisons, paste0("is.na", variable_text))
    } else if (maximum_used) {
      comparisons <- c(
        comparisons,
        paste0(variable_text, " <= ", value_literal(maximum))
      )
    }

    paste0("(", paste(comparisons, collapse = " & "), ")")
  }

  logical_connector <- function(token) {
    switch(
      toupper(token),
      "AND" = " & ",
      "OR" = " | ",
      "XOR" = " != ",
      ") AND (" = ") & (",
      ") OR (" = ") | (",
      ") XOR (" = ") != (",
      "NOT" = " & !",
      ") NOT (" = ") & !(",
      NULL
    )
  }

  arithmetic_connector <- function(token) {
    switch(
      token,
      "+" = " + ",
      "-" = " - ",
      "*" = " * ",
      "/" = " / ",
      ") + (" = ") + (",
      ") - (" = ") - (",
      ") * (" = ") * (",
      ") / (" = ") / (",
      NULL
    )
  }

  comparison_parts <- function(token) {
    matched <- regexec(
      "^(<=|>=|==|!=|=|<|>)\\s*([-+]?[0-9]+([.][0-9]+)?)(\\) (AND|OR|XOR) \\()?$",
      token,
      ignore.case = TRUE
    )
    parts <- regmatches(token, matched)[[1L]]

    if (length(parts) == 0L) {
      return(NULL)
    }

    list(
      operator = if (parts[2L] == "=") "==" else parts[2L],
      value = parts[3L],
      connector = if (length(parts) >= 6L) toupper(parts[6L]) else ""
    )
  }

  metadata_equal <- function(x, y) {
    if (length(y) == 1L) {
      y <- rep(y, length(x))
    }

    result <- (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & x == y)
    result[is.na(result)] <- FALSE
    result
  }

  metadata_in <- function(x, ...) {
    candidates <- list(...)

    if (length(candidates) == 0L) {
      return(rep(FALSE, length(x)))
    }

    Reduce(
      `|`,
      lapply(candidates, function(y) metadata_equal(x, y))
    )
  }

  metadata_not_in <- function(x, ...) {
    !metadata_in(x, ...)
  }

  translate_set_expression <- function(token, current_variable) {
    translated <- token
    # The word boundary prevents the parser from matching the "in(" suffix
    # of the internal helper name metadata_not_in() after translation.
    pattern <- "(?i)\\b(NOT\\s+IN|IN)\\s*\\(([^()]*)\\)"

    repeat {
      matched <- regexec(pattern, translated, perl = TRUE)
      parts <- regmatches(translated, matched)[[1L]]

      if (length(parts) == 0L) {
        break
      }

      arguments <- trimws(strsplit(parts[3L], ",", fixed = TRUE)[[1L]])

      if (length(arguments) == 0L || any(arguments == "")) {
        stop("IN and NOT IN require at least one argument.", call. = FALSE)
      }

      invalid_arguments <- arguments[
        toupper(arguments) != "BLANK" &
          !grepl("^[A-Za-z.][A-Za-z0-9._]*$", arguments)
      ]

      if (length(invalid_arguments) > 0L) {
        stop(
          "Invalid IN/NOT IN arguments: ",
          paste(invalid_arguments, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      argument_text <- vapply(
        arguments,
        function(argument) {
          if (toupper(argument) == "BLANK") {
            ".sspice_blank"
          } else {
            paste0("`", argument, "`")
          }
        },
        character(1)
      )

      function_name <- if (grepl("^NOT", toupper(parts[2L]))) {
        "metadata_not_in"
      } else {
        "metadata_in"
      }

      replacement <- paste0(
        function_name,
        "(`",
        current_variable,
        "`, ",
        paste(argument_text, collapse = ", "),
        ")"
      )

      location <- attr(matched[[1L]], "capture.start")
      full_start <- matched[[1L]][1L]
      full_length <- attr(matched[[1L]], "match.length")[1L]
      translated <- paste0(
        substr(translated, 1L, full_start - 1L),
        replacement,
        substr(translated, full_start + full_length, nchar(translated))
      )
    }

    translated <- gsub("(?i)\\bAND\\b", " & ", translated, perl = TRUE)
    translated <- gsub("(?i)\\bOR\\b", " | ", translated, perl = TRUE)
    translated <- gsub("(?i)\\bXOR\\b", " != ", translated, perl = TRUE)
    translated
  }

  evaluate_condition <- function(text) {
    result <- eval(
      parse(text = text),
      envir = working_data,
      enclos = environment()
    )

    if (length(result) == 1L) {
      result <- rep(result, nrow(working_data))
    }

    if (length(result) != nrow(working_data)) {
      stop(
        "A logical edit did not return one result per record.",
        call. = FALSE
      )
    }

    result
  }

  definition_diagnostics <- list()
  diagnostic_index <- 0L
  .sspice_blank <- rep(NA, nrow(working_data))

  for (edit_name in edit_names) {
    edit_rows <- edits[edits$V1 == edit_name, , drop = FALSE]
    working_data[[edit_name]] <- NA_real_
    condition_text <- "("
    arithmetic_open <- FALSE
    statement_number <- 0L

    for (i in seq_len(nrow(edit_rows))) {
      current_row <- edit_rows[i, , drop = FALSE]
      token <- trimws(current_row$V9)
      explicit_null <- grepl("(?i)(^|\\s)NULL\\s*$", token, perl = TRUE)
      token <- trimws(
        sub("(?i)(^|\\s)NULL\\s*$", "", token, perl = TRUE)
      )

      if (grepl("(?i)\\b(NOT\\s+IN|IN)\\s*\\(", token, perl = TRUE)) {
        if (!is_unused(current_row$V6) ||
            !is_unused(current_row$V7) ||
            !is_unused(current_row$V8)) {
          stop(
            "IN/NOT IN expressions in V9 cannot be combined with V6-V8 ",
            "on the same metadata row.",
            call. = FALSE
          )
        }

        condition_text <- paste0(
          condition_text,
          translate_set_expression(token, current_row$V3)
        )

        if (!explicit_null) {
          next
        }

        condition_text <- paste0(condition_text, ")")
        condition_result <- evaluate_condition(condition_text)
        condition_result[is.na(condition_result)] <- FALSE
        assigned_value <- as.numeric(current_row$V2)
        assign_rows <- is.na(working_data[[edit_name]]) & condition_result

        statement_number <- statement_number + 1L
        diagnostic_index <- diagnostic_index + 1L
        definition_diagnostics[[diagnostic_index]] <- data.frame(
          logical_edit = edit_name,
          assigned_value = assigned_value,
          statement_number = statement_number,
          expression = condition_text,
          condition_true_records = sum(condition_result),
          newly_assigned_records = sum(assign_rows),
          stringsAsFactors = FALSE
        )

        working_data[[edit_name]][assign_rows] <- assigned_value
        condition_text <- "("
        next
      }

      predicate <- make_predicate(
        current_row$V3,
        current_row$V6,
        current_row$V7,
        current_row$V8
      )
      arithmetic_operator <- arithmetic_connector(token)
      comparison <- comparison_parts(token)
      connector <- logical_connector(token)

      if (!is.null(predicate)) {
        if (arithmetic_open) {
          stop(
            "A predicate cannot occur inside an unfinished arithmetic expression for ",
            edit_name,
            ".",
            call. = FALSE
          )
        }
        condition_text <- paste0(condition_text, predicate)
      } else {
        if (!arithmetic_open) {
          condition_text <- paste0(condition_text, "(")
          arithmetic_open <- TRUE
        }
        condition_text <- paste0(
          condition_text,
          "ifelse(is.na(`",
          current_row$V3,
          "`), 0, suppressWarnings(as.numeric(as.character(`",
          current_row$V3,
          "`))))"
        )
      }

      if (!is.null(arithmetic_operator)) {
        if (!arithmetic_open) {
          stop(
            "Arithmetic operator used after a non-arithmetic predicate for ",
            edit_name,
            ".",
            call. = FALSE
          )
        }
        condition_text <- paste0(condition_text, arithmetic_operator)
        next
      }

      if (!is.null(comparison)) {
        if (!arithmetic_open) {
          stop(
            "Arithmetic comparison used without an arithmetic expression for ",
            edit_name,
            ".",
            call. = FALSE
          )
        }

        condition_text <- paste0(
          condition_text,
          ") ",
          comparison$operator,
          " ",
          comparison$value
        )
        arithmetic_open <- FALSE

        if (comparison$connector != "") {
          condition_text <- paste0(
            condition_text,
            switch(
              comparison$connector,
              "AND" = ") & (",
              "OR" = ") | (",
              "XOR" = ") != ("
            )
          )
          next
        }

        condition_text <- paste0(condition_text, ")")
      } else if (!is.null(connector)) {
        condition_text <- paste0(condition_text, connector)
        next
      } else if (explicit_null || toupper(token) == "NULL") {
        if (arithmetic_open) {
          stop(
            "Arithmetic expression for ",
            edit_name,
            " must end with a comparison.",
            call. = FALSE
          )
        }
        condition_text <- paste0(condition_text, ")")
      } else {
        stop(
          "Unknown V9 token in logical-edit metadata: ",
          token,
          ".",
          call. = FALSE
        )
      }

      condition_result <- evaluate_condition(condition_text)
      condition_result[is.na(condition_result)] <- FALSE
      assigned_value <- as.numeric(current_row$V2)
      assign_rows <- is.na(working_data[[edit_name]]) & condition_result

      statement_number <- statement_number + 1L
      diagnostic_index <- diagnostic_index + 1L
      definition_diagnostics[[diagnostic_index]] <- data.frame(
        logical_edit = edit_name,
        assigned_value = assigned_value,
        statement_number = statement_number,
        expression = condition_text,
        condition_true_records = sum(condition_result),
        newly_assigned_records = sum(assign_rows),
        stringsAsFactors = FALSE
      )

      working_data[[edit_name]][assign_rows] <- assigned_value
      condition_text <- "("
    }

    if (condition_text != "(" || arithmetic_open) {
      stop(
        "The final definition of ",
        edit_name,
        " is incomplete.",
        call. = FALSE
      )
    }
  }

  keep_names <- unique(c(original_names, edit_names))
  working_data <- working_data[, keep_names, drop = FALSE]
  row.names(working_data) <- NULL

  logical_edit_definitions <- if (length(definition_diagnostics) > 0L) {
    do.call(rbind, definition_diagnostics)
  } else {
    data.frame(
      logical_edit = character(0),
      assigned_value = numeric(0),
      statement_number = integer(0),
      expression = character(0),
      condition_true_records = integer(0),
      newly_assigned_records = integer(0),
      stringsAsFactors = FALSE
    )
  }

  row.names(logical_edit_definitions) <- NULL
  attr(working_data, "logical_edit_definitions") <-
    logical_edit_definitions

  working_data
}
