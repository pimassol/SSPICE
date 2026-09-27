# ============================================================
# FUNCTION: generate_groups_XOR
# ============================================================
# Purpose : Generate coarsened logical XOR variables in the record-level data.
# Inputs  : data, routing_file_ungrouped, routing_file_grouped, strict
# Output  : Returns the object produced by the function body.
# Notes   : The original function code is reproduced unchanged.
# ============================================================

generate_groups_XOR <- function(
  data,
  routing_file_ungrouped,
  routing_file_grouped,
  strict = TRUE
) {

  required_cols <- paste0("V", 1:5)

  if (!all(
    required_cols %in% names(routing_file_ungrouped)
  )) {
    stop(
      "Il routing non raggruppato deve contenere V1-V5."
    )
  }

  xor_groups <- unique(
    routing_file_ungrouped$V5[
      !is.na(routing_file_ungrouped$V5) &
      routing_file_ungrouped$V5 < 0
    ]
  )

  violations <- list()

  for (group_id in xor_groups) {

    group_data <- routing_file_ungrouped[
      routing_file_ungrouped$V5 == group_id,
      ,
      drop = FALSE
    ]

    if (nrow(group_data) != 2L) {
      stop(
        paste0(
          "Il gruppo XOR ", group_id,
          " contiene ", nrow(group_data),
          " variabili anziche 2."
        )
      )
    }

    nitems <- suppressWarnings(
      as.numeric(group_data$V2)
    )

    min_items <- suppressWarnings(
      as.numeric(group_data$V3)
    )

    max_items <- suppressWarnings(
      as.numeric(group_data$V4)
    )

    is_numeric <- (
      nitems == 1 &
      !is.na(min_items) &
      !is.na(max_items) &
      max_items > min_items
    )

    is_single_categorical <- (
      nitems == 1 &
      !is.na(min_items) &
      !is.na(max_items) &
      min_items == max_items
    )

    if (sum(is_numeric) != 1L) {
      stop(
        paste0(
          "Nel gruppo XOR ", group_id,
          " deve essere presente esattamente ",
          "una variabile numerica."
        )
      )
    }

    if (sum(is_single_categorical) != 1L) {
      stop(
        paste0(
          "Nel gruppo XOR ", group_id,
          " deve essere presente esattamente ",
          "una variabile categorica con un item."
        )
      )
    }

    numeric_var <- group_data$V1[is_numeric]
    categorical_var <- group_data$V1[
      is_single_categorical
    ]

    missing_vars <- setdiff(
      c(categorical_var, numeric_var),
      names(data)
    )

    if (length(missing_vars) > 0L) {
      stop(
        paste0(
          "Variabili XOR assenti nei dati: ",
          paste(missing_vars, collapse = ", "),
          "."
        )
      )
    }

    categorical_active <- !is.na(
      data[[categorical_var]]
    )

    numeric_active <- !is.na(
      data[[numeric_var]]
    )

    both_active <- (
      categorical_active &
      numeric_active
    )

    neither_active <- (
      !categorical_active &
      !numeric_active
    )

    target_var <- paste0(
      "VXOR", abs(group_id)
    )

    data[[target_var]] <- NA_integer_

    data[[target_var]][
      categorical_active & !numeric_active
    ] <- 1L

    data[[target_var]][
      numeric_active & !categorical_active
    ] <- 2L

    if (any(both_active)) {

      invalid_rows <- which(both_active)

      violations[[target_var]] <- data.frame(
        row = invalid_rows,
        xor_group = group_id,
        categorical_variable = categorical_var,
        numeric_variable = numeric_var,
        violation = "both_variables_active",
        stringsAsFactors = FALSE
      )

      if (strict) {
        stop(
          paste0(
            "Violazione del gruppo XOR ",
            group_id, ": ",
            sum(both_active),
            " record hanno entrambe le variabili ",
            "valorizzate. Righe: ",
            paste(head(invalid_rows, 20L),
                  collapse = ", "),
            if (length(invalid_rows) > 20L) {
              ", ..."
            } else {
              ""
            }
          )
        )
      }
    }

    # Le due variabili originali vengono sostituite da VXOR.
    data <- data[
      ,
      !names(data) %in%
        c(categorical_var, numeric_var),
      drop = FALSE
    ]

    # Mantiene l'informazione sui record senza
    # alcuna variabile XOR valorizzata.
    attr(data[[target_var]], "neither_active") <- which(
      neither_active
    )
  }

  # Accetta sia il dataframe di routing sia un vettore
  # contenente direttamente i nomi delle variabili.
  if (is.data.frame(routing_file_grouped)) {
    grouped_names <- routing_file_grouped$V1
  } else {
    grouped_names <- as.character(
      routing_file_grouped
    )
  }

  missing_grouped <- setdiff(
    grouped_names,
    names(data)
  )

  if (length(missing_grouped) > 0L) {
    stop(
      paste0(
        "Variabili del routing raggruppato ",
        "non presenti nei dati: ",
        paste(missing_grouped, collapse = ", "),
        "."
      )
    )
  }

  data <- data[
    ,
    grouped_names,
    drop = FALSE
  ]

  attr(data, "xor_violations") <- violations

  return(data)
}
