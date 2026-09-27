# ============================================================
# FUNCTION: create_vaux_variables
# ============================================================
# Purpose : Create auxiliary questionnaire variables according
#           to logical, range, external-data, or arithmetic rules.
# Inputs  : inpdata, vauxs_data, extDFpath, routing_vnames
# Output  : Input data augmented with the VAUX variables required
#           by the routing structure.
# ============================================================

create_vaux_variables <- function(
  inpdata,
  vauxs_data,
  extDFpath,
  routing_vnames
) {

  trim_field <- function(x) {
    trimws(as.character(x))
  }

  is_empty_field <- function(x) {
    is.na(x) | trim_field(x) %in% c("", ".")
  }

  is_arithmetic_token <- function(x) {
    if (is_empty_field(x)) {
      return(FALSE)
    }

    token <- trimws(
      sub("(?i)(^|\\s)NULL\\s*$", "", trim_field(x), perl = TRUE)
    )

    token %in% c(
      "+", "-", "*", "/",
      ") + (", ") - (", ") * (", ") / ("
    ) || grepl(
      "^(<=|>=|==|!=|=|<|>)\\s*[-+]?[0-9]+([.][0-9]+)?$",
      token
    )
  }

  evaluate_arithmetic_rule <- function(data, expression, condition) {
    condition <- trimws(
      sub("(?i)(^|\\s)NULL\\s*$", "", trim_field(condition), perl = TRUE)
    )

    operator <- sub(
      "^\\s*(<=|>=|==|!=|=|<|>).*?$",
      "\\1",
      condition
    )

    comparison_value <- as.numeric(
      sub(
        "^\\s*(<=|>=|==|!=|=|<|>)\\s*",
        "",
        condition
      )
    )

    expression_value <- with(
      data,
      eval(
        parse(
          text = expression
        )
      )
    )

    if (length(expression_value) == 1L) {
      expression_value <- rep(
        expression_value,
        nrow(data)
      )
    }

    if (length(expression_value) != nrow(data)) {
      stop(
        paste0(
          "Arithmetic VAUX expression does not return one value ",
          "per record: ",
          expression,
          "."
        )
      )
    }

    switch(
      operator,
      "<" = expression_value < comparison_value,
      "<=" = expression_value <= comparison_value,
      ">" = expression_value > comparison_value,
      ">=" = expression_value >= comparison_value,
      "=" = expression_value == comparison_value,
      "==" = expression_value == comparison_value,
      "!=" = expression_value != comparison_value,
      stop(
        paste0(
          "Unknown arithmetic comparison operator: ",
          operator,
          "."
        )
      )
    )
  }

  is_blank_token <- function(x) {
    !is_empty_field(x) && toupper(trim_field(x)) %in% c("NA", "BLANK")
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
        stop("IN and NOT IN require at least one argument.")
      }

      invalid_arguments <- arguments[
        toupper(arguments) != "BLANK" &
          !grepl("^[A-Za-z.][A-Za-z0-9._]*$", arguments)
      ]

      if (length(invalid_arguments) > 0L) {
        stop(
          paste0(
            "Invalid IN/NOT IN arguments: ",
            paste(invalid_arguments, collapse = ", "),
            "."
          )
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

  # In V9, <10 AND / =100 ) OR ( continue an arithmetic predicate
  # with a logical connector. Existing pure arithmetic and logical metadata
  # keep using the original code paths below.
  mixed_comparison_pattern <- paste0(
    "^((?:<=|>=|==|!=|=|<|>)\\s*[-+]?[0-9]+(?:[.][0-9]+)?)",
    "\\s+(AND|OR|\\) AND \\(|\\) OR \\(|NULL)$"
  )
  split_mixed <- function(token) {
    m <- regexec(mixed_comparison_pattern, token, ignore.case = TRUE, perl = TRUE)
    parts <- regmatches(token, m)[[1L]]
    if (length(parts) == 0L) return(NULL)
    list(comparison = parts[2L], connector = toupper(trimws(parts[3L])))
  }
  append_connector <- function(token) {
    switch(toupper(trimws(token)),
      "AND" = " & ", "OR" = " | ",
      ") AND (" = ") & (", ") OR (" = ") | (",
      "NULL" = ")",
      stop("Unknown mixed VAUX connector: ", token)
    )
  }
  evaluate_mixed <- function(data, rows, name) {
    values <- rep(NA_real_, nrow(data))
    condition <- "("
    arithmetic <- "("
    arithmetic_pending <- FALSE
    predicate_env <- new.env(parent = baseenv())
    n_predicates <- 0L
    operators <- c("+", "-", "*", "/", ") + (", ") - (", ") * (", ") / (")
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      variable <- trim_field(row$V3)
      token <- trim_field(row$V9)
      if (!variable %in% names(data)) stop("Unknown VAUX operand: ", variable)
      mixed <- split_mixed(token)
      core <- trimws(sub("(?i)(^|\\s)NULL\\s*$", "", token, perl = TRUE))
      arithmetic_operator <- core %in% operators
      arithmetic_comparison <- !is.null(mixed) ||
        grepl("^(<=|>=|==|!=|=|<|>)\\s*[-+]?[0-9]+([.][0-9]+)?$", core)
      if (arithmetic_operator || arithmetic_comparison || arithmetic_pending) {
        if (!is_empty_field(row$V6) || !is_empty_field(row$V7) ||
            !is_empty_field(row$V8)) stop("Arithmetic row uses V6-V8: ", name)
        # The only inserted operand is an integer position from names(data).
        arithmetic <- paste0(arithmetic,
          "suppressWarnings(as.numeric(as.character(data[[",
          match(variable, names(data)), "]])))")
        if (arithmetic_operator) {
          arithmetic <- paste0(arithmetic, " ", core, " ")
          arithmetic_pending <- TRUE
          next
        }
        if (!arithmetic_comparison) stop("Missing arithmetic comparison: ", name)
        comparison <- if (is.null(mixed)) core else mixed$comparison
        predicate <- evaluate_arithmetic_rule(data, paste0(arithmetic, ")"), comparison)
        connector <- if (is.null(mixed)) "NULL" else mixed$connector
        arithmetic <- "("
        arithmetic_pending <- FALSE
      } else {
        if (grepl("(?i)\\b(NOT\\s+IN|IN)\\s*\\(", token, perl = TRUE)) {
          if (!is_empty_field(row$V6) || !is_empty_field(row$V7) ||
              !is_empty_field(row$V8)) stop("IN mixed with V6-V8: ", name)
          .sspice_blank <- rep(NA, nrow(data))
          predicate <- with(data, eval(parse(text =
            translate_set_expression(core, variable))))
          connector <- if (grepl("(?i)\\bNULL\\s*$", token, perl = TRUE)) "NULL" else ""
        } else if (!is_empty_field(row$V6)) {
          if (is_blank_token(row$V6)) {
            predicate <- is.na(data[[variable]])
          } else {
            value <- trim_field(row$V6)
            if (!grepl("^[-+]?[0-9]+([.][0-9]+)?$", value))
              stop("Mixed exact value must be numeric: ", name)
            predicate <- data[[variable]] == as.numeric(value)
          }
          connector <- toupper(token)
        } else if (!is_empty_field(row$V7) && !is_empty_field(row$V8)) {
          lo <- suppressWarnings(as.numeric(trim_field(row$V7)))
          hi <- suppressWarnings(as.numeric(trim_field(row$V8)))
          if (is.na(lo) || is.na(hi)) stop("Invalid mixed interval: ", name)
          predicate <- data[[variable]] >= lo & data[[variable]] <= hi
          connector <- toupper(token)
        } else {
          stop("Mixed VAUX row lacks a predicate: ", name)
        }
      }
      if (length(predicate) != nrow(data)) stop("Invalid predicate length: ", name)
      n_predicates <- n_predicates + 1L
      label <- paste0("p", n_predicates)
      assign(label, as.logical(predicate), envir = predicate_env)
      condition <- paste0(condition, label)
      if (connector == "") next
      condition <- paste0(condition, append_connector(connector))
      if (connector == "NULL") {
        hit <- eval(parse(text = condition), envir = predicate_env)
        mode <- suppressWarnings(as.numeric(trim_field(row$V2)))
        if (is.na(mode)) stop("Non-numeric mixed VAUX mode: ", name)
        values[!is.na(hit) & hit] <- mode
        condition <- "("
        predicate_env <- new.env(parent = baseenv())
        n_predicates <- 0L
      }
    }
    if (arithmetic_pending || condition != "(") stop("Unfinished mixed VAUX rule: ", name)
    values
  }

  required_columns <- paste0(
    "V",
    1:9
  )

  if (!all(required_columns %in% names(vauxs_data))) {
    if (ncol(vauxs_data) < 9L) {
      stop("VAUX metadata must contain at least nine columns.")
    }

    names(vauxs_data)[1:9] <- required_columns
  }

  unique_vauxs <- unique(
    trim_field(vauxs_data$V1)
  )

  original_columns <- colnames(inpdata)

  for (vaux in unique_vauxs) {
    vaux_rows <- vauxs_data[
      trim_field(vauxs_data$V1) == vaux,
      ,
      drop = FALSE
    ]

    arithmetic_rows <- vapply(
      vaux_rows$V9,
      is_arithmetic_token,
      logical(1)
    )

    mixed_mode <- (any(arithmetic_rows) && !all(arithmetic_rows)) ||
      any(vapply(vaux_rows$V9, function(x) !is.null(split_mixed(trim_field(x))),
                 logical(1)))

    if (all(arithmetic_rows) && !mixed_mode) {
      arithmetic_sources <- unique(
        vaux_rows[, c("V4", "V5", "V3"), drop = FALSE]
      )

      arithmetic_sources <- arithmetic_sources[
        !is_empty_field(arithmetic_sources$V4),
        ,
        drop = FALSE
      ]

      arithmetic_variables_to_drop <- character(0)

      if (nrow(arithmetic_sources) > 0L) {
        source_names <- unique(trim_field(arithmetic_sources$V4))

        for (source_name in source_names) {
          source_rows <- arithmetic_sources[
            trim_field(arithmetic_sources$V4) == source_name,
            ,
            drop = FALSE
          ]

          source_keys <- unique(trim_field(source_rows$V5))
          source_keys <- source_keys[!is_empty_field(source_keys)]

          if (length(source_keys) != 1L) {
            stop(
              paste0(
                "External arithmetic source ",
                source_name,
                " must have exactly one join key."
              )
            )
          }

          source_key <- source_keys[1L]
          source_file <- file.path(
            extDFpath,
            paste0(source_name, ".csv")
          )

          if (!file.exists(source_file)) {
            stop(
              paste0(
                "External arithmetic data file not found: ",
                source_file,
                "."
              )
            )
          }

          source_data <- utils::read.csv(
            source_file,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )

          source_variables <- unique(trim_field(source_rows$V3))

          missing_source_columns <- setdiff(
            c(source_key, source_variables),
            names(source_data)
          )

          if (length(missing_source_columns) > 0L) {
            stop(
              paste0(
                "Columns missing from external arithmetic source ",
                source_name,
                ": ",
                paste(missing_source_columns, collapse = ", "),
                "."
              )
            )
          }

          if (!(source_key %in% names(inpdata))) {
            stop(
              paste0(
                "Join key ",
                source_key,
                " is missing from input data."
              )
            )
          }

          source_variables <- setdiff(
            source_variables,
            names(inpdata)
          )

          source_data <- source_data[
            ,
            c(source_key, source_variables),
            drop = FALSE
          ]

          if (anyDuplicated(source_data[[source_key]])) {
            stop(
              paste0(
                "Join key ",
                source_key,
                " is duplicated in external arithmetic source ",
                source_name,
                "."
              )
            )
          }

          inpdata$.sspice_input_order <- seq_len(nrow(inpdata))
          inpdata <- merge(
            inpdata,
            source_data,
            by = source_key,
            all.x = TRUE,
            sort = FALSE
          )
          inpdata <- inpdata[
            order(inpdata$.sspice_input_order),
            ,
            drop = FALSE
          ]
          inpdata$.sspice_input_order <- NULL
          row.names(inpdata) <- NULL

          arithmetic_variables_to_drop <- c(
            arithmetic_variables_to_drop,
            source_variables
          )
        }
      }

      inpdata[[vaux]] <- NA_real_

      arithmetic_expression <- "("

      for (i in seq_len(nrow(vaux_rows))) {
        operand <- trim_field(
          vaux_rows$V3[i]
        )

        assigned_value <- suppressWarnings(
          as.numeric(
            trim_field(
              vaux_rows$V2[i]
            )
          )
        )

        if (is.na(assigned_value)) {
          stop(
            paste0(
              "VAUX ",
              vaux,
              " has a non-numeric assigned value in row ",
              i,
              "."
            )
          )
        }

        # Arithmetic operands may arrive as character or factor after a
        # record-level proposal.  Convert only the value used by the
        # arithmetic expression, leaving the input column unchanged.
        arithmetic_expression <- paste0(
          arithmetic_expression,
          " suppressWarnings(as.numeric(as.character(inpdata$",
          operand,
          ")))"
        )

        arithmetic_token <- trim_field(vaux_rows$V9[i])
        arithmetic_token_core <- trimws(
          sub(
            "(?i)(^|\\s)NULL\\s*$",
            "",
            arithmetic_token,
            perl = TRUE
          )
        )

        if (arithmetic_token_core %in% c(
          "+", "-", "*", "/",
          ") + (", ") - (", ") * (", ") / ("
        )) {
          arithmetic_expression <- paste0(
            arithmetic_expression,
            " ",
            arithmetic_token_core,
            " "
          )
        } else {
          arithmetic_expression <- paste0(
            arithmetic_expression,
            ")"
          )

          rule_result <- evaluate_arithmetic_rule(
            data = inpdata,
            expression = arithmetic_expression,
            condition = arithmetic_token_core
          )

          rows_to_assign <- !is.na(rule_result) &
            rule_result &
            is.na(inpdata[[vaux]])

          inpdata[[vaux]][rows_to_assign] <- assigned_value
          arithmetic_expression <- "("
        }
      }

      if (arithmetic_expression != "(") {
        stop(
          paste0(
            "Arithmetic definition of VAUX ",
            vaux,
            " does not end with a comparison condition."
          )
        )
      }

      inpdata <- inpdata[
        ,
        !(names(inpdata) %in% unique(arithmetic_variables_to_drop)),
        drop = FALSE
      ]

      next
    }

    external_dfs <- unique(
      vaux_rows[, c("V4", "V5", "V3"), drop = FALSE]
    )

    valid_external_rows <- !is_empty_field(
      external_dfs$V4
    )

    external_dfs <- external_dfs[
      valid_external_rows,
      ,
      drop = FALSE
    ]

    var2drop <- character(0)

    if (nrow(external_dfs) > 0L) {
      external_file_names <- unique(
        trim_field(external_dfs$V4)
      )

      for (external_file_name in external_file_names) {
        current_external_rows <- external_dfs[
          trim_field(external_dfs$V4) == external_file_name,
          ,
          drop = FALSE
        ]

        join_keys <- unique(
          trim_field(current_external_rows$V5)
        )

        join_keys <- join_keys[
          !is_empty_field(join_keys)
        ]

        if (length(join_keys) != 1L) {
          stop(
            paste0(
              "External data source ",
              external_file_name,
              " must have exactly one join key."
            )
          )
        }

        join_key <- join_keys[1L]
        external_file_path <- file.path(
          extDFpath,
          paste0(
            external_file_name,
            ".csv"
          )
        )

        if (!file.exists(external_file_path)) {
          warning(
            paste0(
              "External data file not found: ",
              external_file_path,
              "."
            )
          )
          next
        }

        external_df <- utils::read.csv(
          external_file_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        if (!(join_key %in% colnames(external_df))) {
          warning(
            paste0(
              "Join key ",
              join_key,
              " not found in ",
              external_file_name,
              "."
            )
          )
          next
        }

        if (!(join_key %in% colnames(inpdata))) {
          warning(
            paste0(
              "Join key ",
              join_key,
              " not found in input data."
            )
          )
          next
        }

        variables_to_import <- unique(
          trim_field(current_external_rows$V3)
        )

        variables_to_import <- variables_to_import[
          !is_empty_field(variables_to_import)
        ]

        missing_external_variables <- setdiff(
          variables_to_import,
          names(external_df)
        )

        if (length(missing_external_variables) > 0L) {
          stop(
            paste0(
              "Variables missing from external data source ",
              external_file_name,
              ": ",
              paste(
                missing_external_variables,
                collapse = ", "
              ),
              "."
            )
          )
        }

        variables_to_import <- setdiff(
          variables_to_import,
          names(inpdata)
        )

        external_df <- external_df[
          ,
          c(
            join_key,
            variables_to_import
          ),
          drop = FALSE
        ]

        if (anyDuplicated(external_df[[join_key]])) {
          stop(
            paste0(
              "Join key ",
              join_key,
              " is duplicated in external data source ",
              external_file_name,
              "."
            )
          )
        }

        input_order <- seq_len(nrow(inpdata))
        inpdata$.sspice_input_order <- input_order

        inpdata <- merge(
          inpdata,
          external_df,
          by = join_key,
          all.x = TRUE,
          sort = FALSE
        )

        inpdata <- inpdata[
          order(inpdata$.sspice_input_order),
          ,
          drop = FALSE
        ]

        inpdata$.sspice_input_order <- NULL
        row.names(inpdata) <- NULL

        var2drop <- c(
          var2drop,
          variables_to_import
        )
      }
    }

    if (mixed_mode) {
      inpdata[[vaux]] <- evaluate_mixed(inpdata, vaux_rows, vaux)
      inpdata <- inpdata[, !(names(inpdata) %in% unique(var2drop)), drop = FALSE]
      next
    }

    # A VAUX modality is assigned only when its complete metadata
    # condition is satisfied.  If no modality applies, the auxiliary
    # variable must remain missing: zero is not a VAUX modality and
    # would conceal the distinction between an unresolved condition
    # and an explicitly assigned value.
    inpdata[[vaux]] <- NA_real_
    condition <- "("
    nr <- nrow(vaux_rows)

    for (i in seq_len(nr)) {
      row <- vaux_rows[i, , drop = FALSE]
      varvaux <- trim_field(row$V1)
      varbase <- trim_field(row$V3)
      valvaux <- row$V2
      valbase <- row$V6
      valmin <- row$V7
      valmax <- row$V8
      logical_op_original <- trim_field(row$V9)
      explicit_null <- grepl(
        "(?i)(^|\\s)NULL\\s*$",
        logical_op_original,
        perl = TRUE
      )
      logical_op_core <- trimws(
        sub(
          "(?i)(^|\\s)NULL\\s*$",
          "",
          logical_op_original,
          perl = TRUE
        )
      )
      logical_op <- toupper(logical_op_core)

      set_expression <- grepl(
        "(?i)\\b(NOT\\s+IN|IN)\\s*\\(",
        logical_op_core,
        perl = TRUE
      )

      if (set_expression) {
        if (!is_empty_field(valbase) ||
            !is_empty_field(valmin) ||
            !is_empty_field(valmax)) {
          stop(
            paste0(
              "IN/NOT IN in V9 cannot be combined with V6-V8 ",
              "on the same VAUX metadata row."
            )
          )
        }

        condition <- paste0(
          condition,
          translate_set_expression(logical_op_core, varbase)
        )
      } else if (!is_empty_field(valbase)) {
        if (is_blank_token(valbase)) {
          condition <- paste0(
            condition,
            " is.na(inpdata$",
            varbase,
            ")"
          )
        } else {
        condition <- paste0(
          condition,
          " inpdata$",
          varbase,
          " == ",
          trim_field(valbase)
        )
        }
      } else if (
        !is_empty_field(valmin) &&
        !is_empty_field(valmax)
      ) {
        condition <- paste0(
          condition,
          " (inpdata$",
          varbase,
          " >= ",
          trim_field(valmin),
          " & inpdata$",
          varbase,
          " <= ",
          trim_field(valmax),
          ")"
        )
      }

      if (set_expression) {
        if (!explicit_null) {
          next
        }

        condition <- paste0(condition, ")")
      } else if (!is_empty_field(logical_op) && logical_op != "NULL") {
        logop <- switch(
          logical_op,
          "OR" = " | ",
          ") OR (" = ") | (",
          "AND" = " & ",
          ") AND (" = ") & (",
          "NOT" = " ! ",
          ") NOT (" = ") ! (",
          stop(
            paste0(
              "Unknown logical operator: ",
              logical_op,
              "."
            )
          )
        )

        condition <- paste0(
          condition,
          logop
        )
      } else {
        condition <- paste0(
          condition,
          ")"
        )
      }

      if (explicit_null || logical_op == "NULL") {
        .sspice_blank <- rep(NA, nrow(inpdata))
        result <- with(
          inpdata,
          eval(
            parse(
              text = condition
            )
          )
        )

        rows_to_assign <- !is.na(result) & result

        inpdata[[varvaux]][rows_to_assign] <- valvaux
        condition <- "("
      }
    }

    inpdata <- inpdata[
      ,
      !(colnames(inpdata) %in% unique(var2drop)),
      drop = FALSE
    ]
  }

  final_columns <- unique(
    c(
      original_columns,
      unique_vauxs
    )
  )

  final_columns <- final_columns[
    final_columns %in% colnames(inpdata)
  ]

  inpdata <- inpdata[
    ,
    final_columns,
    drop = FALSE
  ]

  missing_routing_variables <- setdiff(
    routing_vnames,
    names(inpdata)
  )

  if (length(missing_routing_variables) > 0L) {
    stop(
      paste0(
        "Variables required by the routing are missing after VAUX creation: ",
        paste(
          missing_routing_variables,
          collapse = ", "
        ),
        "."
      )
    )
  }

  inpdata[
    ,
    routing_vnames,
    drop = FALSE
  ]
}
