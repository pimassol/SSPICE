.sspice_partition_correction_records <- function(
    data,
    full_report,
    record_id) {
  if (!is.data.frame(data) || !record_id %in% names(data)) {
    stop("data must contain record_id.", call. = FALSE)
  }
  invalid_ids <- character()
  if (is.data.frame(full_report) && nrow(full_report)) {
    if (!record_id %in% names(full_report)) {
      stop("The routing report does not contain record_id.", call. = FALSE)
    }
    invalid_ids <- unique(as.character(full_report[[record_id]]))
    invalid_ids <- invalid_ids[!is.na(invalid_ids)]
  }
  erroneous <- as.character(data[[record_id]]) %in% invalid_ids
  list(
    exact_indices = which(!erroneous),
    erroneous_indices = which(erroneous),
    exact_records = data[!erroneous, , drop = FALSE],
    erroneous_records = data[erroneous, , drop = FALSE]
  )
}

.sspice_cache_value <- function(value) {
  result <- as.character(value)
  result[is.na(value)] <- "<NA>"
  result
}

.sspice_condition_key <- function(data, variables) {
  if (!length(variables)) return(rep("<MARGINAL>", nrow(data)))
  encoded <- lapply(data[variables], .sspice_cache_value)
  do.call(paste, c(encoded, sep = "\r"))
}

.sspice_previous_variable <- function(variable, dag, record_id = NULL) {
  position <- match(variable, dag$varnames)
  if (is.na(position) || position <= 1L) return(character())
  previous <- dag$varnames[position - 1L]
  if (!is.null(record_id) && identical(previous, record_id)) {
    return(character())
  }
  previous
}

.sspice_probability_matrix <- function(data, variable, conditioning) {
  if (!nrow(data) || !variable %in% names(data) ||
      length(setdiff(conditioning, names(data)))) {
    return(list(
      counts = matrix(integer(), 0L, 0L),
      probabilities = matrix(numeric(), 0L, 0L),
      donor_count = integer(),
      conditioning = conditioning
    ))
  }
  values <- .sspice_cache_value(data[[variable]])
  usable <- values != "<NA>"
  if (!any(usable)) {
    return(list(
      counts = matrix(integer(), 0L, 0L),
      probabilities = matrix(numeric(), 0L, 0L),
      donor_count = integer(),
      conditioning = conditioning
    ))
  }
  keys <- .sspice_condition_key(data, conditioning)
  counts <- table(keys[usable], values[usable])
  probabilities <- counts / rowSums(counts)
  list(
    counts = unclass(counts),
    probabilities = unclass(probabilities),
    donor_count = stats::setNames(
      as.integer(rowSums(counts)),
      rownames(counts)
    ),
    conditioning = conditioning
  )
}

.sspice_build_donor_cache <- function(
    donor_data,
    dag,
    record_id = NULL,
    strata = character(),
    immutable = TRUE) {
  if (is.null(donor_data)) return(NULL)
  if (!is.data.frame(donor_data)) {
    stop("donor_data must be a data frame.", call. = FALSE)
  }
  if (is.null(dag$varnames) ||
      length(setdiff(dag$varnames, names(donor_data)))) {
    stop("donor_data and DAG+ variables are not aligned.", call. = FALSE)
  }
  strata <- unique(as.character(strata))
  strata <- strata[!is.na(strata) & nzchar(strata)]
  unknown_strata <- setdiff(strata, names(donor_data))
  if (length(unknown_strata)) {
    stop("Unknown cache strata: ", paste(unknown_strata, collapse = ", "),
         ".", call. = FALSE)
  }
  record_id <- record_id %||% dag$record_id
  entries <- stats::setNames(
    lapply(dag$varnames, function(variable) {
      predecessor <- .sspice_previous_variable(variable, dag, record_id)
      full_conditioning <- unique(c(predecessor, strata))
      list(
        predecessor = predecessor,
        strata = strata,
        predecessor_strata = .sspice_probability_matrix(
          donor_data,
          variable,
          full_conditioning
        ),
        predecessor_only = .sspice_probability_matrix(
          donor_data,
          variable,
          predecessor
        ),
        marginal = .sspice_probability_matrix(
          donor_data,
          variable,
          character()
        )
      )
    }),
    dag$varnames
  )
  structure(
    list(
      matrices = entries,
      number_donors = nrow(donor_data),
      record_id = record_id,
      strata = strata,
      immutable = isTRUE(immutable)
    ),
    class = "sspice_donor_cache"
  )
}

.sspice_matrix_distribution <- function(matrix_entry, record) {
  probabilities <- matrix_entry$probabilities
  if (!nrow(probabilities) || !ncol(probabilities)) return(NULL)
  key <- .sspice_condition_key(record, matrix_entry$conditioning)[1L]
  position <- match(key, rownames(probabilities))
  if (is.na(position)) return(NULL)
  probability <- as.numeric(probabilities[position, , drop = TRUE])
  counts <- as.integer(
    matrix_entry$counts[position, , drop = TRUE]
  )
  keep <- is.finite(probability) & probability > 0
  if (!any(keep)) return(NULL)
  list(
    values = colnames(probabilities)[keep],
    probabilities = probability[keep] / sum(probability[keep]),
    counts = counts[keep],
    donor_count = sum(counts[keep])
  )
}

.sspice_cached_node_distribution <- function(
    cache,
    variable,
    record,
    domain,
    minimum_donors,
    allow_unconditional_donor = FALSE) {
  if (is.null(cache) || !inherits(cache, "sspice_donor_cache")) return(NULL)
  entry <- cache$matrices[[variable]]
  if (is.null(entry)) return(NULL)
  tiers <- list()
  if (length(entry$predecessor) && length(entry$strata)) {
    tiers[[length(tiers) + 1L]] <- list(
      matrix = entry$predecessor_strata,
      strata_used = entry$strata
    )
  }
  if (length(entry$predecessor)) {
    tiers[[length(tiers) + 1L]] <- list(
      matrix = entry$predecessor_only,
      strata_used = character()
    )
  }
  if (isTRUE(allow_unconditional_donor)) {
    tiers[[length(tiers) + 1L]] <- list(
      matrix = entry$marginal,
      strata_used = character()
    )
  }
  for (tier in tiers) {
    observed <- .sspice_matrix_distribution(tier$matrix, record)
    if (is.null(observed)) next
    if (length(domain)) {
      keep <- observed$values %in% as.character(domain)
      observed$values <- observed$values[keep]
      observed$probabilities <- observed$probabilities[keep]
      observed$counts <- observed$counts[keep]
      if (!length(observed$values)) next
      observed$donor_count <- sum(observed$counts)
      observed$probabilities <- observed$probabilities /
        sum(observed$probabilities)
    }
    if (observed$donor_count < minimum_donors) next
    observed$counts <- NULL
    observed$source <- if (length(tier$matrix$conditioning)) {
      "conditional_donor"
    } else {
      "marginal_donor"
    }
    observed$predecessors_used <- paste(entry$predecessor, collapse = " | ")
    observed$strata_used <- paste(tier$strata_used, collapse = " | ")
    return(observed)
  }
  NULL
}

.sspice_recombine_correction_data <- function(
    original_data,
    corrected_records,
    corrected_indices) {
  result <- original_data
  if (length(corrected_indices)) {
    if (nrow(corrected_records) != length(corrected_indices)) {
      stop("corrected_records and corrected_indices are not aligned.",
           call. = FALSE)
    }
    result[corrected_indices, ] <- corrected_records
  }
  result
}

.sspice_conditioning_hierarchy <- function(
    predecessors,
    strata = NULL) {

  predecessors <- unique(
    as.character(
      predecessors %||% character()
    )
  )

  strata <- setdiff(
    unique(
      as.character(
        strata %||% character()
      )
    ),
    predecessors
  )

  hierarchy <- lapply(
    length(strata):0L,
    function(number_strata) {
      c(
        predecessors,
        if (number_strata > 0L) {
          strata[seq_len(number_strata)]
        } else {
          character()
        }
      )
    }
  )

  unique(
    c(
      hierarchy,
      list(character())
    )
  )
}
