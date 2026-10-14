# ============================================================
# FUNCTION: create_vardom
# ============================================================
# Purpose:
#   Create CONCORD/SCIA variable-domain definitions and the
#   layout required to write fixed-width ASCII records.
#
# Outputs when output.path is supplied:
#   VARDOM.dat
#   record_layout.csv
#
# Return value:
#   A list containing vardom and record_layout.
#
# STRUTT.dat and LISTE.dat are not generated because SSPICE
# represents logical groups as canonical VOR and VXOR variables.
# ============================================================

create_vardom <- function(routes_df,
                          Bt,
                          items.maps,
                          path.items.maps,
                          output.path = NULL) {

  mapping_file <- file.path(path.items.maps, items.maps)

  if (!file.exists(mapping_file)) {
    stop(paste0("Item-mapping file not found: ", mapping_file, "."))
  }

  item_map <- utils::read.csv(
    mapping_file,
    stringsAsFactors = FALSE,
    header = TRUE
  )

  graph <- generate_stree(Bt, routes_df)
  flow_df <- calculate_flows(graph$stree, graph$list, routes_df, Bt)

  nitems <- abs(routes_df[, 2])
  vtypes <- routes_df[, 5]
  vnames <- colnames(Bt)
  minmax <- routes_df[, c(1, 3:4)]
  minitem <- minmax[, 2]
  maxitem <- minmax[, 3]
  maxitem <- ifelse(maxitem == 0, 1, maxitem)
  minitem <- ifelse(is.na(minitem), maxitem, minitem)

  vardom <- cbind(vnames[minmax[, 1]], minitem, maxitem)
  vardom <- as.data.frame(vardom, stringsAsFactors = FALSE)
  names(vardom) <- c("varname", "minitem", "maxitem")
  vardom$node <- seq_len(nrow(routes_df))
  vardom <- merge(vardom, flow_df, by = "node", all.x = TRUE)

  vardom$blank_allowed <- ifelse(round(vardom$flow, 3) < 1, 1, 0)
  vardom$blank_implicit <- vardom$blank_allowed
  vardom$blank_allowed[vtypes < 0] <- 1
  vardom$blank_implicit[vtypes < 0] <- 1

  vardom$minitem <- as.numeric(vardom$minitem)
  vardom$maxitem <- as.numeric(vardom$maxitem)
  vardom$intervalli <- NA_character_

  varnames_mapping <- unique(item_map[, 1])

  for (i in seq_len(nrow(vardom))) {
    var <- vardom$varname[i]

    if (var %in% item_map[, 1]) {
      codici <- item_map[
        item_map[, 1] == var,
        2:3,
        drop = FALSE
      ]

      names(codici) <- c("route", "dset")
      codici <- codici[order(codici$route), , drop = FALSE]
      intervalli <- lapply(codici$dset, function(x) c(x, x))

      intervalli_chr <- paste0(
        sprintf("%04d", sapply(intervalli, `[`, 1)),
        sprintf("%04d", sapply(intervalli, `[`, 2))
      )

      vardom$minitem[i] <- 0
      vardom$maxitem[i] <- 0
      vardom$intervalli[i] <- paste0(intervalli_chr, collapse = "")
    }
  }

  for (i in seq_len(nrow(vardom))) {
    var <- vardom$varname[i]

    vardom$blank_allowed[i] <- ifelse(
      is.na(routes_df[i, 3]),
      1,
      vardom$blank_allowed[i]
    )

    if (!(var %in% varnames_mapping)) {
      if (nitems[i] == 0) {
        step <- 0
      } else if (nitems[i] == 1 && minitem[i] < maxitem[i]) {
        step <- -1
      } else if (nitems[i] > 1) {
        step <- (maxitem[i] - minitem[i]) / (nitems[i] - 1)
      } else if (nitems[i] == 1 && minitem[i] == maxitem[i]) {
        step <- -2
      }

      step_is_integer <- step %% 1 == 0
      intervalli <- list()

      if (step_is_integer && step > 1) {
        start <- minitem[i]
        intervalli[[1]] <- c(start, start)

        for (k in seq_len(nitems[i] - 1)) {
          start <- start + step
          intervalli[[length(intervalli) + 1]] <- c(start, start)
        }
      } else if (step == 0) {
        intervalli[[1]] <- c(9999, 9999)
      } else if (step == -1 || step == -2) {
        intervalli[[1]] <- c(minitem[i], maxitem[i])
      }

      intervalli_chr <- paste0(
        sprintf("%04d", sapply(intervalli, `[`, 1)),
        sprintf("%04d", sapply(intervalli, `[`, 2))
      )

      vardom$intervalli[i] <- paste0(intervalli_chr, collapse = "")
    }
  }

  layout_maximum <- ifelse(routes_df[, 4] == 0, 1, routes_df[, 4])
  len_var <- floor(log10(layout_maximum)) + 1
  # SSPICE-MAPPED-LAYOUT-FIX
  # Fixed-width fields must accommodate observed dataset codes.
  for (i in seq_len(nrow(vardom))) {
    rows <- trimws(as.character(item_map[[1L]])) ==
      as.character(vardom$varname[i])
    if (!any(rows)) next
    codes <- suppressWarnings(as.numeric(
      as.character(item_map[[3L]][rows])
    ))
    if (anyNA(codes) || any(!is.finite(codes)) ||
        any(codes != trunc(codes))) {
      stop('VARDOM mapped codes must be finite integers.',
           call. = FALSE)
    }
    len_var[i] <- max(nchar(format(
      codes, scientific = FALSE, trim = TRUE
    )))
  }
  pos_i <- cumsum(c(1, utils::head(len_var, -1L)))

  vardom$pos_i <- pos_i
  vardom$len_var <- len_var

  vardom$record <- paste0(
    sprintf(
      "%-10s%04d%04d",
      substr(vardom$varname, 1, 10),
      ifelse(is.na(vardom$minitem), 0, vardom$minitem),
      ifelse(is.na(vardom$maxitem), 0, vardom$maxitem)
    ),
    substr(vardom$intervalli, 1, 80),
    ifelse(vardom$blank_allowed == 1, "B   B", "")
  )

  vardom_df <- vardom[, c(
    "varname",
    "minitem",
    "maxitem",
    "intervalli",
    "blank_allowed",
    "blank_implicit",
    "pos_i",
    "len_var",
    "record"
  )]

  record_layout <- data.frame(
    varname = as.character(vardom$varname),
    position = as.integer(vardom$pos_i),
    length = as.integer(vardom$len_var),
    stringsAsFactors = FALSE
  )

  if (!is.null(output.path)) {
    dir.create(
      output.path,
      recursive = TRUE,
      showWarnings = FALSE
    )

    writeLines(
      vardom$record,
      con = file.path(output.path, "VARDOM.dat"),
      useBytes = TRUE
    )

    utils::write.csv(
      record_layout,
      file = file.path(output.path, "record_layout.csv"),
      row.names = FALSE,
      na = ""
    )
  }

  list(
    vardom = vardom_df,
    record_layout = record_layout
  )
}
