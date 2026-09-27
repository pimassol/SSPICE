#' Count the admissible data patterns induced by a routing DAG
#'
#' Uses dynamic programming and arbitrary-precision integers to count all
#' record patterns compatible with the modal transitions represented by `Bt`.
#'
#' @param dag A DAG object produced by the SSPICE DAG-building pipeline.
#' @param root_variables Optional root-variable names or indices. By default,
#'   roots are inferred from `Bt`.
#' @param max_memo_states Maximum number of memoized states.
#' @param verbose Whether to print a short execution summary.
#'
#' @return An object of class `sspice_admissible_pattern_count`.
#' @export
count_admissible_patterns <- function(
  dag,
  root_variables = NULL,
  max_memo_states = 5000000L,
  verbose = TRUE
) {
  if (!requireNamespace("gmp", quietly = TRUE)) {
    stop("Package 'gmp' is required for exact pattern counting.", call. = FALSE)
  }
  required <- c("routes_numeric", "Bt")
  missing <- setdiff(required, names(dag))
  if (length(missing)) {
    stop("The DAG object is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  Bt <- unname(as.matrix(dag$Bt))
  storage.mode(Bt) <- "integer"
  routes <- dag$routes_numeric
  varnames <- colnames(dag$Bt)
  if (is.null(varnames) && !is.null(dag$varnames)) varnames <- dag$varnames
  if (!nrow(Bt) || !ncol(Bt) || any(!Bt %in% c(-1L, 0L, 1L))) {
    stop("Bt must be a non-empty matrix containing only -1, 0 and 1.", call. = FALSE)
  }
  if (length(varnames) != ncol(Bt) || anyDuplicated(varnames)) {
    stop("Bt must have unique and valid variable names.", call. = FALSE)
  }

  p <- ncol(Bt)
  nitems <- abs(suppressWarnings(as.integer(routes[, 2L])))
  nitems[is.na(nitems) | nitems <= 0L] <- 1L
  if (length(nitems) != p) {
    stop("The routing metadata are not coherent with Bt.", call. = FALSE)
  }

  source_index <- apply(Bt, 1L, function(row) {
    index <- which(row == 1L)
    if (length(index) == 1L) index else NA_integer_
  })
  if (anyNA(source_index)) {
    stop("Each Bt row must contain exactly one source node.", call. = FALSE)
  }
  target_list <- lapply(seq_len(nrow(Bt)), function(row) which(Bt[row, ] == -1L))
  if (any(lengths(target_list) == 0L)) {
    stop("Each Bt row must contain at least one destination node.", call. = FALSE)
  }
  if (any(vapply(seq_len(nrow(Bt)), function(row) {
    any(target_list[[row]] <= source_index[row])
  }, logical(1L)))) {
    stop("Bt columns are not in topological order.", call. = FALSE)
  }

  rows_by_source <- split(
    seq_len(nrow(Bt)),
    factor(source_index, levels = seq_len(p))
  )
  incoming <- colSums(Bt == -1L) > 0L
  inferred_roots <- which(!incoming)
  root_index <- if (is.null(root_variables)) {
    inferred_roots
  } else if (is.character(root_variables)) {
    match(root_variables, varnames)
  } else {
    as.integer(root_variables)
  }
  if (!length(root_index) || anyNA(root_index) ||
      any(root_index < 1L | root_index > p)) {
    stop("Invalid root variables.", call. = FALSE)
  }
  root_index <- sort(unique(root_index))

  consequence <- lapply(seq_len(nrow(Bt)), function(row) {
    from <- source_index[row]
    targets <- target_list[[row]]
    status <- rep.int(-1L, p)
    status[targets] <- 1L
    for (target in targets) {
      if (target > from + 1L) {
        between <- seq.int(from + 1L, target - 1L)
        blanks <- between[Bt[row, between] == 0L]
        if (length(blanks)) status[blanks] <- 0L
      }
    }
    status
  })

  memo <- new.env(hash = TRUE, parent = emptyenv())
  n_states <- 0L
  make_key <- function(position, required) {
    paste0(position, "|", paste0(required[position:p] + 1L, collapse = ""))
  }
  merge_requirements <- function(required, imposed, start) {
    index <- which(imposed != -1L & seq_len(p) >= start)
    if (!length(index)) return(required)
    if (any(required[index] != -1L & required[index] != imposed[index])) return(NULL)
    required[index] <- imposed[index]
    required
  }
  count_from <- function(position, required) {
    if (position > p) return(gmp::as.bigz(1L))
    key <- make_key(position, required)
    if (exists(key, envir = memo, inherits = FALSE)) {
      return(get(key, envir = memo, inherits = FALSE))
    }
    n_states <<- n_states + 1L
    if (n_states > max_memo_states) {
      stop("Maximum number of memoized states exceeded.", call. = FALSE)
    }
    if (required[position] != 1L) {
      answer <- count_from(position + 1L, required)
      assign(key, answer, envir = memo)
      return(answer)
    }
    modal_rows <- rows_by_source[[position]]
    if (!length(modal_rows)) {
      answer <- gmp::as.bigz(nitems[position]) *
        count_from(position + 1L, required)
      assign(key, answer, envir = memo)
      return(answer)
    }
    answer <- gmp::as.bigz(0L)
    for (row in modal_rows) {
      next_required <- merge_requirements(
        required, consequence[[row]], position + 1L
      )
      if (!is.null(next_required)) {
        answer <- answer + count_from(position + 1L, next_required)
      }
    }
    assign(key, answer, envir = memo)
    answer
  }

  initial <- rep.int(-1L, p)
  initial[root_index] <- 1L
  count <- count_from(1L, initial)
  unrestricted <- gmp::as.bigz(1L)
  for (number in nitems) unrestricted <- unrestricted * (number + 1L)

  result <- list(
    n_admissible_patterns = count,
    n_all_unrestricted_patterns = unrestricted,
    n_variables = p,
    n_modal_rows = nrow(Bt),
    root_variables = varnames[root_index],
    n_memo_states = n_states,
    exact_arbitrary_precision = TRUE
  )
  class(result) <- "sspice_admissible_pattern_count"
  if (isTRUE(verbose)) {
    print(data.frame(
      measure = names(result)[1:6],
      value = vapply(result[1:6], function(x) paste(x, collapse = " | "), character(1)),
      stringsAsFactors = FALSE
    ))
  }
  result
}
