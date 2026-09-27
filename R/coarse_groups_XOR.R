# ============================================================
# FUNCTION: coarse_groups_XOR
# ============================================================
# Purpose:
#   Collapse each logical XOR pair into a VXOR node while preserving outgoing
#   routing. VXOR value 1 represents the categorical member and value 2 the
#   numeric member.
# ============================================================

coarse_groups_XOR <- function(routes) {
  required_cols <- paste0("V", 1:6)

  if (!all(required_cols %in% names(routes))) {
    stop("Il routing deve contenere almeno le colonne V1-V6.")
  }

  routes$cn <- seq_len(nrow(routes))

  xor_groups <- unique(
    routes$V5[!is.na(routes$V5) & routes$V5 < 0]
  )

  if (length(xor_groups) == 0L) {
    routes$cn <- NULL
    return(routes)
  }

  for (group_id in xor_groups) {
    group_rows <- which(routes$V5 == group_id)
    group_data <- routes[group_rows, , drop = FALSE]

    if (nrow(group_data) != 2L) {
      stop(
        paste0(
          "Il gruppo XOR ", group_id,
          " contiene ", nrow(group_data),
          " variabili anziche 2."
        )
      )
    }

    nitems <- suppressWarnings(as.numeric(group_data$V2))
    min_items <- suppressWarnings(as.numeric(group_data$V3))
    max_items <- suppressWarnings(as.numeric(group_data$V4))

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
          " non e stata identificata esattamente una variabile numerica."
        )
      )
    }

    if (sum(is_single_categorical) != 1L) {
      stop(
        paste0(
          "Nel gruppo XOR ", group_id,
          " non e stata identificata esattamente una variabile ",
          "categorica con un solo item."
        )
      )
    }

    first_cn <- min(group_data$cn)
    last_cn <- max(group_data$cn)
    first_row <- which(routes$cn == first_cn)
    rows_after_group <- which(routes$cn > last_cn)

    natural_target <- if (length(rows_after_group) > 0L) {
      as.character(routes$V1[rows_after_group[1L]])
    } else {
      NA_character_
    }

    get_branch_target <- function(route_row, branch_type) {
      number_of_jumps <- suppressWarnings(
        as.numeric(route_row$V6)
      )

      if (is.na(number_of_jumps) || number_of_jumps == 0) {
        return(natural_target)
      }

      if (number_of_jumps == 98) {
        if (ncol(route_row) < 8L) {
          stop("Salto assoluto XOR privo della variabile di destinazione.")
        }

        target <- as.character(route_row[[8L]])

        if (is.na(target) || trimws(target) == "") {
          stop("Salto assoluto XOR privo della variabile di destinazione.")
        }

        return(target)
      }

      number_of_jumps <- as.integer(number_of_jumps)
      target_columns <- 2L * seq_len(number_of_jumps) + 6L
      target_columns <- target_columns[target_columns <= ncol(route_row)]
      targets <- as.character(unlist(route_row[target_columns], use.names = FALSE))
      targets <- unique(targets[!is.na(targets) & trimws(targets) != ""])

      if (length(targets) == 0L) {
        return(natural_target)
      }

      if (branch_type == "numeric") {
        stop(
          paste0(
            "Il membro numerico del gruppo XOR ", group_id,
            " contiene salti condizionati. Il coarsening conserva ",
            "soltanto un salto assoluto per il ramo numerico."
          )
        )
      }

      if (length(targets) != 1L) {
        stop(
          paste0(
            "Il membro categorico del gruppo XOR ", group_id,
            " deve avere una sola destinazione."
          )
        )
      }

      targets
    }

    categorical_target <- get_branch_target(
      group_data[is_single_categorical, , drop = FALSE],
      "categorical"
    )

    numeric_target <- get_branch_target(
      group_data[is_numeric, , drop = FALSE],
      "numeric"
    )

    new_node <- paste0("VXOR", abs(group_id))
    original_group_names <- as.character(group_data$V1)

    routes[first_row, "V1"] <- new_node
    routes[first_row, "V2"] <- 2
    routes[first_row, "V3"] <- 1
    routes[first_row, "V4"] <- 2
    routes[first_row, "V5"] <- 0

    routing_column_numbers <- suppressWarnings(
      as.integer(sub("V", "", names(routes), fixed = TRUE))
    )
    jump_columns <- which(
      !is.na(routing_column_numbers) & routing_column_numbers >= 7L
    )

    if (length(jump_columns) > 0L) {
      routes[first_row, jump_columns] <- NA
    }

    branch_modes <- c(1L, 2L)
    branch_targets <- c(categorical_target, numeric_target)
    requires_jump <- !is.na(branch_targets) & (
      is.na(natural_target) | branch_targets != natural_target
    )
    branch_modes <- branch_modes[requires_jump]
    branch_targets <- branch_targets[requires_jump]

    if (length(branch_targets) == 0L) {
      routes[first_row, "V6"] <- 0
    } else if (
      length(branch_targets) == 2L &&
      identical(branch_targets[1L], branch_targets[2L])
    ) {
      routes[first_row, "V6"] <- 98
      routes[first_row, "V7"] <- NA
      routes[first_row, "V8"] <- branch_targets[1L]
    } else {
      routes[first_row, "V6"] <- length(branch_targets)

      for (jump_index in seq_along(branch_targets)) {
        item_column <- paste0("V", 2L * jump_index + 5L)
        target_column <- paste0("V", 2L * jump_index + 6L)

        if (!item_column %in% names(routes)) {
          routes[[item_column]] <- NA
        }

        if (!target_column %in% names(routes)) {
          routes[[target_column]] <- NA
        }

        routes[first_row, item_column] <- branch_modes[jump_index]
        routes[first_row, target_column] <- branch_targets[jump_index]
      }
    }

    destination_column_numbers <- seq.int(
      from = 8L,
      to = max(8L, ncol(routes) - 1L),
      by = 2L
    )
    destination_column_numbers <- destination_column_numbers[
      destination_column_numbers <= ncol(routes) &
      names(routes)[destination_column_numbers] != "cn"
    ]

    for (column_index in destination_column_numbers) {
      replace_rows <- which(
        as.character(routes[[column_index]]) %in% original_group_names
      )

      if (length(replace_rows) > 0L) {
        routes[replace_rows, column_index] <- new_node
      }
    }

    routes <- routes[
      !(routes$cn %in% setdiff(group_data$cn, first_cn)),
      ,
      drop = FALSE
    ]
  }

  routes$cn <- NULL
  rownames(routes) <- NULL
  routes
}
