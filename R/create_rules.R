# ============================================================
# FUNCTION: create_rules
# ============================================================
# Purpose:
#   Generate SSPICE formal compatibility rules from the canonical
#   routing DAG represented by the transposed incidence matrix Bt.
#
# Inputs:
#   Bt                   Transposed incidence matrix.
#   routes_df            Processed numeric routing table.
#   items.maps           Optional item-mapping CSV filename.
#   path.items.maps      Directory containing the mapping file.
#   path_output          Optional directory for RULES.dat.
#   condizione_intervallo Retained for backward compatibility.
#
# Output when path_output is supplied:
#   RULES.dat
#
# Return value:
#   Character vector containing the generated compatibility rules.
# ============================================================

create_rules <- function(Bt,
                         routes_df,
                         items.maps = NULL,
                         path.items.maps = NULL,
                         path_output = NULL,
                         condizione_intervallo = TRUE) {

  item_map <- NULL

  if (!is.null(items.maps) && !is.null(path.items.maps)) {
    mapping_file <- file.path(
      path.items.maps,
      items.maps
    )

    if (!file.exists(mapping_file)) {
      stop(
        paste0(
          "Item-mapping file not found: ",
          mapping_file,
          "."
        )
      )
    }

    item_map <- utils::read.csv(
      mapping_file,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE
    )

    if (ncol(item_map) < 3L) {
      stop(
        "The item-mapping file must contain at least three columns."
      )
    }

    names(item_map)[1:3] <- c(
      "varname",
      "route",
      "dset"
    )
  }

  tmp <- routes_df[, 1:4]
  tmp$V2 <- ifelse(
    is.na(tmp$V2) | tmp$V2 < 1,
    1,
    tmp$V2
  )

  tmp$V3 <- ifelse(
    is.na(tmp$V3),
    1,
    tmp$V3
  )

  items_list <- tmp

  items_exp <- items_list[
    rep(
      seq_len(nrow(items_list)),
      items_list$V2
    ),
  ]

  items_exp$item <- unlist(
    mapply(
      function(start, end, n) {
        seq(
          start,
          end,
          length.out = n
        )
      },
      items_list$V3,
      items_list$V4,
      items_list$V2,
      SIMPLIFY = FALSE
    ),
    use.names = FALSE
  )

  rownames(items_exp) <- NULL
  items_exp$row <- seq_len(nrow(items_exp))
  vnames <- colnames(Bt)

  items_exp$varname <- vnames[
    items_list$V1[
      rep(
        seq_len(nrow(items_list)),
        items_list$V2
      )
    ]
  ]

  items_exp <- items_exp[, c(
    "row",
    "varname",
    "item"
  )]

  if (is.null(item_map)) {
    mapped_items_exp <- items_exp
    mapped_items_exp$dset <- NA_real_
  } else {
    mapped_items_exp <- merge(
      x = items_exp,
      y = item_map,
      by.x = c(
        "varname",
        "item"
      ),
      by.y = c(
        "varname",
        "route"
      ),
      all.x = TRUE
    )

    mapped_items_exp <- mapped_items_exp[
      order(mapped_items_exp$row),
    ]
  }

  mapped_rows <- !is.na(
    mapped_items_exp$dset
  )

  mapped_items_exp$item[mapped_rows] <-
    mapped_items_exp$dset[mapped_rows]

  rules <- character(0)
  vn <- colnames(Bt)

  for (i in seq_len(nrow(Bt))) {
    row_vals <- Bt[i, ]
    var_from <- mapped_items_exp$varname[i]
    item_val <- mapped_items_exp$item[i]
    to_idx <- which(row_vals == -1)
    from_idx <- which(row_vals == 1)

    if (length(from_idx) != 1L || length(to_idx) == 0L) {
      next
    }

    condizione <- paste0(
      var_from,
      "(",
      item_val,
      ")"
    )

    for (k in to_idx) {
      var_to <- vn[k]

      if (k > from_idx + 1L) {
        intermediate_indices <- seq.int(
          from_idx + 1L,
          k - 1L
        )

        mid_vars <- vn[
          intermediate_indices[
            row_vals[intermediate_indices] == 0
          ]
        ]
      } else {
        mid_vars <- character(0)
      }

      negazioni <- c(
        if (length(mid_vars) > 0L) {
          paste0(mid_vars, "()")
        } else {
          NULL
        },
        paste0(var_to, "< )")
      )

      regola <- paste(
        c(
          condizione,
          negazioni
        ),
        collapse = " "
      )

      rules <- unique(
        c(
          rules,
          regola
        )
      )
    }
  }

  if (!is.null(path_output)) {
    dir.create(
      path_output,
      recursive = TRUE,
      showWarnings = FALSE
    )

    rules_file <- file.path(
      path_output,
      "RULES.dat"
    )

    writeLines(
      rules,
      con = rules_file,
      useBytes = TRUE
    )

    message(
      "SSPICE rules written to: ",
      rules_file
    )
  }

  rules
}
