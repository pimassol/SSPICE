# Item mapping boundary: dataset codes -> routing codes -> dummy positions.
# Records and routing metadata are never recoded in place.
.sspice_read_item_mapping <- function(file = NULL) {
  if (is.null(file) || !length(file)) return(NULL)
  if (length(file) != 1L || is.na(file) || !file.exists(file)) {
    stop("Item-mapping file not found: ", paste(file, collapse = ", "),
         call. = FALSE)
  }
  mapping <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(mapping) < 3L) {
    stop("Item mapping requires variable, routing code and dataset code columns.",
         call. = FALSE)
  }
  mapping <- mapping[, 1:3, drop = FALSE]
  names(mapping) <- c("variable", "new", "old")
  if (!nrow(mapping)) return(NULL)
  mapping$variable <- trimws(as.character(mapping$variable))
  mapping$new <- suppressWarnings(as.numeric(as.character(mapping$new)))
  mapping$old <- suppressWarnings(as.numeric(as.character(mapping$old)))
  if (anyNA(mapping) || any(!nzchar(mapping$variable)) ||
      any(!is.finite(mapping$new)) || any(!is.finite(mapping$old))) {
    stop("Item mapping contains missing or invalid numeric codes.", call. = FALSE)
  }
  for (variable in unique(mapping$variable)) {
    rows <- mapping$variable == variable
    if (anyDuplicated(mapping$new[rows]) || anyDuplicated(mapping$old[rows])) {
      stop("Item mapping must be one-to-one for ", variable, ".", call. = FALSE)
    }
  }
  mapping
}

.sspice_validate_item_mapping <- function(mapping, variables, nitems, minimum, maximum) {
  if (is.null(mapping)) return(invisible(NULL))
  for (variable in intersect(mapping$variable, variables)) {
    index <- match(variable, variables)
    codes <- sort(mapping$new[mapping$variable == variable])
    n <- nitems[index]
    lo <- minimum[index]
    hi <- maximum[index]
    if (!is.finite(n) || n < 1L || !is.finite(lo) || !is.finite(hi) ||
        (n > 1L && hi <= lo) || (n == 1L && hi != lo) ||
        length(codes) != n ||
        !isTRUE(all.equal(codes, seq(lo, hi, length.out = n),
                          check.attributes = FALSE))) {
      stop("Item-mapping routing codes disagree with the routing domain for ",
           variable, ".", call. = FALSE)
    }
  }
  invisible(NULL)
}

.sspice_map_route_values <- function(value, variable, mapping = NULL) {
  if (is.null(mapping)) return(value)
  rows <- mapping$variable == variable
  if (!any(rows)) return(value)
  pairs <- mapping[rows, , drop = FALSE]
  numeric_value <- suppressWarnings(as.numeric(as.character(value)))
  matched <- match(numeric_value, pairs$old)
  if (any(!is.na(value) & is.na(matched))) {
    stop("Dataset code absent from item mapping for ", variable, ".",
         call. = FALSE)
  }
  # One lookup against the original vector; overlapping codes cannot cascade.
  pairs$new[matched]
}

.sspice_routing_positions <- function(values, nitems, minimum, maximum,
                                      mapping = NULL) {
  for (index in seq_along(values)) {
    if (nitems[index] <= 1L) next
    value <- .sspice_map_route_values(values[[index]], names(values)[index], mapping)
    value <- suppressWarnings(as.numeric(as.character(value)))
    lo <- minimum[index]
    hi <- maximum[index]
    if (is.finite(lo) && is.finite(hi) && hi > lo) {
      # Same scaling and out-of-range convention as the existing dummize().
      value <- pmax(1, pmin(nitems[index],
        round((value - lo) / ((hi - lo) / (nitems[index] - 1L))) + 1))
    }
    values[[index]] <- value
  }
  values
}
