#' Check record conformity with an SSPICE routing structure
#'
#' Runs the path-based routing check and optionally adds an exhaustive scan of
#' logical-edit variables. The latter reports every LEDIT equal to `NA`, even
#' when an earlier path violation prevents that node from being reached.
#'
#' @details
#' \code{full_report} has one row per reported violation, with the record key,
#' variable labels, error code and message. \code{summary_by_rec} groups by
#' identifier and message, so multiple rows can represent one record.
#' \code{summary_by_var} groups by variable and message. Consistent records
#' are absent from these summaries. \code{violation_source} is added to the
#' full report only when logical-edit names are supplied.
#' Item mapping translates dataset codes to structural routing codes without
#' modifying the input records. See the routing guide for the mapping contract.
#'
#' @param dset_data Record-level data aligned with the routing DAG.
#' @param routes Processed numeric routing table.
#' @param Bt Transposed incidence matrix.
#' @param varkey Name of the record identifier column.
#' @param items.maps File name of the mapping CSV. Use a header-only file when
#'   no recoding is needed. Columns are variable, routing code, dataset code.
#' @param path.items.maps Directory containing the item-mapping file.
#' @param logical_edit_names Optional character vector naming logical-edit
#'   variables. `NULL` preserves the original routing-only behaviour.
#' @return A list containing `full_report`, `summary_by_rec`,
#'   `summary_by_var`, `record_id`, and `logical_edit_names`.
#' @examples
#' routes <- data.frame(V1 = c("ID", "Q", "END"),
#'   V2 = c(1, 2, 1), V3 = c(1, 1, 1), V4 = c(9999, 2, 1),
#'   V5 = 0, V6 = 0, V7 = NA, V8 = NA)
#' dag <- build_DAG(routes, analyze = FALSE)
#' x <- data.frame(ID = 1:2, Q = c(1, 2), END = c(1, 1))
#' mapping <- tempfile(fileext = ".csv")
#' write.csv(data.frame(varname = character(), item_route = numeric(),
#'                     item_dataset = numeric()), mapping, row.names = FALSE)
#' checked <- routing_check(x, dag$routes_numeric, dag$Bt, "ID",
#'                          basename(mapping), dirname(mapping))
#' checked$full_report
#' unlink(mapping)
#'
#' @export
routing_check <- function(
  dset_data,
  routes,
  Bt,
  varkey,
  items.maps,
  path.items.maps,
  logical_edit_names = NULL
) {
  if (!is.data.frame(dset_data)) {
    stop("dset_data must be a data frame.", call. = FALSE)
  }

  if (length(varkey) != 1L || is.na(varkey) || !nzchar(varkey) ||
      !(varkey %in% names(dset_data))) {
    stop("varkey must identify one column in dset_data.", call. = FALSE)
  }

  route_errors <- check_all_paths(
    dset_data = dset_data,
    routes = routes,
    Bt = Bt,
    varkey = varkey,
    items.maps = items.maps,
    path.items.maps = path.items.maps
  )

  variable_names <- colnames(Bt)
  path_report <- report_errors(route_errors, variable_names)

  if (!(varkey %in% names(path_report))) {
    path_report[[varkey]] <- route_errors[[varkey]]
  }

  logical_edit_names <- unique(as.character(logical_edit_names))
  logical_edit_names <- logical_edit_names[
    !is.na(logical_edit_names) & nzchar(logical_edit_names)
  ]

  if (length(logical_edit_names) == 0L) {
    full_report <- path_report
  } else {
    logical_report <- check_logical_edits(
      data = dset_data,
      id_col = varkey,
      logical_edit_names = logical_edit_names
    )

    path_report$violation_source <- rep.int(
      "ROUTING",
      nrow(path_report)
    )

    if (nrow(logical_report) > 0L && nrow(path_report) > 0L) {
      logical_keys <- paste(
        as.character(logical_report[[varkey]]),
        logical_report$varname_label,
        sep = "\r"
      )
      path_keys <- paste(
        as.character(path_report[[varkey]]),
        path_report$varname_label,
        sep = "\r"
      )

      # Prefer the explicit logical-edit report when the path checker has
      # already reported the same edit for the same record.
      path_report <- path_report[
        !(path_keys %in% logical_keys),
        ,
        drop = FALSE
      ]
    }

    all_columns <- union(names(path_report), names(logical_report))

    add_missing_columns <- function(data, columns) {
      missing_columns <- setdiff(columns, names(data))
      for (column in missing_columns) {
        data[[column]] <- NA
      }
      data[, columns, drop = FALSE]
    }

    path_report <- add_missing_columns(path_report, all_columns)
    logical_report <- add_missing_columns(logical_report, all_columns)
    full_report <- rbind(path_report, logical_report)

    if (nrow(full_report) > 0L) {
      full_report <- full_report[
        order(full_report$id_recprd, full_report$varname),
        ,
        drop = FALSE
      ]
    }
    row.names(full_report) <- NULL
  }

  if (nrow(full_report) == 0L) {
    summary_by_rec <- data.frame(
      id_record = full_report[[varkey]],
      message = character(),
      count = integer(),
      stringsAsFactors = FALSE
    )
    summary_by_var <- data.frame(
      varname_label = character(),
      message = character(),
      count = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    summary_by_rec <- stats::aggregate(
      x = rep.int(1L, nrow(full_report)),
      by = list(
        id_record = full_report[[varkey]],
        message = full_report$message
      ),
      FUN = sum
    )
    names(summary_by_rec)[3L] <- "count"
    summary_by_rec <- summary_by_rec[
      order(summary_by_rec$count, decreasing = TRUE),
      ,
      drop = FALSE
    ]

    summary_by_var <- stats::aggregate(
      x = rep.int(1L, nrow(full_report)),
      by = list(
        varname_label = full_report$varname_label,
        message = full_report$message
      ),
      FUN = sum
    )
    names(summary_by_var)[3L] <- "count"
    summary_by_var <- summary_by_var[
      order(summary_by_var$count, decreasing = TRUE),
      ,
      drop = FALSE
    ]
  }

  row.names(summary_by_rec) <- NULL
  row.names(summary_by_var) <- NULL

  result <- list(
    full_report = full_report,
    summary_by_rec = summary_by_rec,
    summary_by_var = summary_by_var,
    record_id = varkey,
    logical_edit_names = logical_edit_names
  )
  class(result) <- c("sspice_routing_check", "list")
  result
}
