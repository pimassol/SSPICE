#' Generate records admissible under an SSPICE routing DAG
#'
#' Internal generator used to create controlled data for routing and
#' localization tests.
#'
#' @param dag A DAG object produced by `build_DAG()`.
#' @param n_records Number of records to generate.
#' @param id_col Optional record identifier represented in the DAG.
#' @param items_mapping_file Optional path to `items_mapping.csv`.
#' @param seed Random seed.
#' @param optional_fill_probability Probability of filling an optional target.
#' @param max_attempts Maximum attempts allowed for each record.
#' @return A data frame whose columns follow `dag$varnames`.
#' @keywords internal
generate_admissible_records <- function(
  dag,
  n_records = 5000L,
  id_col = NULL,
  items_mapping_file = NULL,
  seed = 123L,
  optional_fill_probability = 0.5,
  max_attempts = 1000L
) {
  required <- c("routes_numeric", "varnames", "Bt", "A")
  missing_elements <- setdiff(required, names(dag))
  if (length(missing_elements)) {
    stop("The DAG object is missing: ", paste(missing_elements, collapse = ", "),
         call. = FALSE)
  }

  n_records <- as.integer(n_records)
  if (is.na(n_records) || n_records < 1L) {
    stop("n_records must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(optional_fill_probability) ||
      optional_fill_probability < 0 || optional_fill_probability > 1) {
    stop("optional_fill_probability must be between zero and one.", call. = FALSE)
  }

  set.seed(seed)
  routes <- dag$routes_numeric
  varnames <- as.character(dag$varnames)
  Bt <- as.matrix(dag$Bt)
  domains <- .sspice_synthetic_domains(routes, varnames, items_mapping_file)
  rows_by_source <- .sspice_bt_rows_by_source(domains, nrow(Bt))
  source_nodes <- which(colSums(dag$A != 0) == 0L)
  source_nodes <- source_nodes[rowSums(dag$A[source_nodes, , drop = FALSE] != 0) > 0L]
  if (!length(source_nodes)) {
    stop("No source node was found in the routing DAG.", call. = FALSE)
  }

  id_values <- NULL
  if (!is.null(id_col) && id_col %in% varnames) {
    id_index <- match(id_col, varnames)
    id_domain <- domains[[id_index]]
    lower <- suppressWarnings(as.integer(routes[id_index, 3L]))
    upper <- suppressWarnings(as.integer(routes[id_index, 4L]))
    if (length(id_domain$route) == 1L && !is.na(lower) && !is.na(upper) &&
        upper >= lower && (upper - lower + 1) >= n_records) {
      id_values <- sample(seq.int(lower, upper), n_records, replace = FALSE)
    } else {
      stop("The routing domain cannot provide unique values for id_col.",
           call. = FALSE)
    }
  }

  records <- vector("list", n_records)
  for (record_index in seq_len(n_records)) {
    generated <- NULL
    for (attempt in seq_len(max_attempts)) {
      generated <- .sspice_generate_one_record(
        routes = routes,
        varnames = varnames,
        Bt = Bt,
        domains = domains,
        rows_by_source = rows_by_source,
        source_nodes = source_nodes,
        optional_fill_probability = optional_fill_probability,
        id_col = id_col,
        id_value = if (is.null(id_values)) NULL else id_values[record_index]
      )
      if (!is.null(generated)) break
    }
    if (is.null(generated)) {
      stop("Unable to generate record ", record_index,
           " after ", max_attempts, " attempts.", call. = FALSE)
    }
    records[[record_index]] <- generated
  }

  result <- as.data.frame(do.call(rbind, records), stringsAsFactors = FALSE)
  names(result) <- varnames
  for (j in seq_along(result)) {
    result[[j]] <- suppressWarnings(as.numeric(result[[j]]))
  }
  result
}

.sspice_synthetic_domains <- function(routes, varnames, mapping_file = NULL) {
  item_map <- NULL
  if (!is.null(mapping_file)) {
    if (!file.exists(mapping_file)) {
      stop("Item-mapping file not found: ", mapping_file, call. = FALSE)
    }
    item_map <- utils::read.csv(mapping_file, stringsAsFactors = FALSE,
                                check.names = FALSE)
    if (ncol(item_map) < 3L) {
      stop("The item-mapping file must contain at least three columns.",
           call. = FALSE)
    }
    names(item_map)[1:3] <- c("varname", "route", "dset")
  }

  lapply(seq_len(nrow(routes)), function(i) {
    number <- abs(suppressWarnings(as.integer(routes[i, 2L])))
    if (is.na(number) || number <= 0L) number <- 1L
    lower <- suppressWarnings(as.numeric(routes[i, 3L]))
    upper <- suppressWarnings(as.numeric(routes[i, 4L]))
    if (is.na(lower)) lower <- if (is.na(upper)) 1 else upper
    if (is.na(upper)) upper <- lower
    route_values <- if (number == 1L) lower else
      seq(lower, upper, length.out = number)
    data_values <- route_values

    if (!is.null(item_map)) {
      for (k in seq_along(route_values)) {
        index <- which(
          as.character(item_map$varname) == varnames[i] &
            as.character(item_map$route) == as.character(route_values[k])
        )
        if (length(index) == 1L && !is.na(item_map$dset[index])) {
          data_values[k] <- item_map$dset[index]
        }
      }
    }
    list(route = route_values, data = data_values)
  })
}

.sspice_bt_rows_by_source <- function(domains, number_of_bt_rows) {
  counts <- vapply(domains, function(x) length(x$route), integer(1L))
  last_source <- which(cumsum(counts) <= number_of_bt_rows)
  rows <- vector("list", length(domains))
  cursor <- 1L
  for (i in last_source) {
    rows[[i]] <- seq.int(cursor, length.out = counts[i])
    cursor <- cursor + counts[i]
  }
  rows
}

.sspice_generate_one_record <- function(
  routes, varnames, Bt, domains, rows_by_source, source_nodes,
  optional_fill_probability, id_col, id_value
) {
  nvars <- length(varnames)
  state <- integer(nvars)
  values <- rep(NA_real_, nvars)
  state[source_nodes] <- 1L
  queue <- source_nodes
  processed <- logical(nvars)

  while (length(queue)) {
    node <- queue[1L]
    queue <- queue[-1L]
    if (processed[node] || state[node] != 1L) next
    modal_rows <- rows_by_source[[node]]

    if (!length(modal_rows)) {
      if (!is.null(id_col) && varnames[node] == id_col && !is.null(id_value)) {
        values[node] <- id_value
      } else {
        values[node] <- sample(domains[[node]]$data, 1L)
      }
      processed[node] <- TRUE
      next
    }

    position <- sample(seq_along(modal_rows), 1L)
    if (!is.null(id_col) && varnames[node] == id_col && !is.null(id_value)) {
      values[node] <- id_value
      position <- 1L
    } else {
      values[node] <- domains[[node]]$data[position]
    }
    edge <- Bt[modal_rows[position], ]
    targets <- which(edge == -1L)
    from <- which(edge == 1L)
    if (length(from) != 1L || !length(targets)) return(NULL)

    for (target in targets) {
      between <- if (target > from + 1L) seq.int(from + 1L, target - 1L) else integer()
      blank_nodes <- between[edge[between] == 0L]
      if (any(state[blank_nodes] == 1L)) return(NULL)
      state[blank_nodes] <- -1L

      required <- !is.na(routes[target, 3L])
      fill_target <- required || stats::runif(1L) < optional_fill_probability
      if (fill_target) {
        if (state[target] == -1L) return(NULL)
        state[target] <- 1L
        queue <- unique(c(queue, target))
      } else if (state[target] == 0L) {
        state[target] <- -1L
      }
    }
    processed[node] <- TRUE
  }

  values[state != 1L] <- NA_real_
  values
}
