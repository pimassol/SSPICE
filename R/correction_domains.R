.sspice_normalize_fixed_variables <- function(fixed_variables, names_data) {
  if (is.null(fixed_variables)) return(character())
  if (!is.null(names(fixed_variables))) {
    keep <- !is.na(fixed_variables) & as.numeric(fixed_variables) == 1
    fixed_variables <- names(fixed_variables)[keep]
  }
  fixed_variables <- unique(as.character(fixed_variables))
  unknown <- setdiff(fixed_variables, names_data)
  if (length(unknown)) {
    stop("Unknown fixed variables: ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  fixed_variables
}

.sspice_observed_domain <- function(variable, data = NULL, donor_data = NULL) {
  sources <- Filter(
    Negate(is.null),
    list(data, donor_data)
  )
  values <- unlist(lapply(sources, function(source) {
    if (!variable %in% names(source)) return(NULL)
    as.character(source[[variable]])
  }), use.names = FALSE)
  unique(values[!is.na(values) & nzchar(values)])
}

.sspice_domains_from_routes <- function(
    routes, variable_names, data = NULL, donor_data = NULL, item_mapping = NULL) {
  if (is.null(routes) || ncol(routes) < 4L) {
    stop("Routing metadata must contain at least four columns.", call. = FALSE)
  }
  route_names <- as.character(routes[, 1L])
  if (all(suppressWarnings(!is.na(as.integer(route_names))))) {
    route_names <- variable_names[suppressWarnings(as.integer(route_names))]
  }
  result <- setNames(vector("list", length(variable_names)), variable_names)
  for (variable in variable_names) {
    index <- which(route_names == variable)[1L]
    if (is.na(index)) next
    raw_n_items <- suppressWarnings(as.integer(routes[index, 2L]))
    n_items <- abs(raw_n_items)
    minimum <- suppressWarnings(as.numeric(routes[index, 3L]))
    maximum <- suppressWarnings(as.numeric(routes[index, 4L]))
    if (is.finite(raw_n_items) && raw_n_items > 1L &&
        is.finite(minimum) && is.finite(maximum) && maximum >= minimum) {
      result[[variable]] <- seq(
        from = minimum,
        to = maximum,
        length.out = raw_n_items
      )
    } else if (identical(raw_n_items, 1L) &&
               is.finite(minimum) && is.finite(maximum) &&
               maximum > minimum) {
      result[[variable]] <- seq.int(
        from = ceiling(minimum),
        to = floor(maximum)
      )
    } else if (identical(raw_n_items, 1L) &&
               is.finite(minimum) && is.finite(maximum) &&
               maximum == minimum) {
      result[[variable]] <- minimum
    } else if (is.finite(raw_n_items) && raw_n_items > 0L &&
               is.finite(n_items) && n_items > 0L) {
      result[[variable]] <- seq_len(n_items)
    } else {
      result[[variable]] <- .sspice_observed_domain(
        variable = variable,
        data = data,
        donor_data = donor_data
      )
    }
  }
  if (!is.null(item_mapping)) {
    for (variable in intersect(variable_names, item_mapping$variable)) {
      pairs <- item_mapping[item_mapping$variable == variable, , drop = FALSE]
      result[[variable]] <- pairs$old[order(pairs$new)]
    }
  }
  result
}

.sspice_validate_priors <- function(priors, domains) {
  if (is.null(priors)) return(list())
  if (!is.list(priors) || is.null(names(priors)) || any(!nzchar(names(priors)))) {
    stop("imputation_priors must be a named list.", call. = FALSE)
  }
  lapply(names(priors), function(variable) {
    probability <- priors[[variable]]
    if (is.null(names(probability))) {
      stop("Prior probabilities for ", variable, " must have mode names.",
           call. = FALSE)
    }
    probability <- as.numeric(probability)
    names(probability) <- names(priors[[variable]])
    if (any(!is.finite(probability)) || any(probability < 0) ||
        sum(probability) <= 0) {
      stop("Invalid prior probabilities for ", variable, ".", call. = FALSE)
    }
    allowed <- as.character(domains[[variable]])
    if (length(allowed) && any(!names(probability) %in% allowed)) {
      stop("Prior for ", variable, " contains values outside its domain.",
           call. = FALSE)
    }
    probability / sum(probability)
  }) |>
    stats::setNames(names(priors))
}
