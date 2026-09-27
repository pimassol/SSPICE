#' Validate an extended canonical representation with ELOG variables
#'
#' Selects empirical semantic edits, constructs their ELOG variables with
#' accepted domain `1 and 2`, and verifies localization against the induced
#' semantic ground truth.
#'
#' @param data A routing-consistent record-level data frame.
#' @param routing Original routing metadata.
#' @param id_col Name of the record identifier column.
#' @param seed Random seed.
#' @param min_outcome_count Minimum required frequency for every ELOG outcome.
#' @param max_search Maximum number of candidate searches per logical form.
#' @param min_non_missing_rate Minimum eligible-variable completeness.
#' @param max_levels Maximum number of levels for an eligible variable.
#'
#' @return An object of class `sspice_elog_validation`.
#' @export
validate_extended_elog <- function(
  data,
  routing,
  id_col = "ID_RECORD",
  seed = 123L,
  min_outcome_count = 5L,
  max_search = 50000L,
  min_non_missing_rate = 0.25,
  max_levels = 12L
) {
  if (!id_col %in% names(data)) data[[id_col]] <- seq_len(nrow(data))
  if (ncol(routing) < 6L) stop("Routing must contain at least six columns.")
  if (!all(paste0("V", 1:6) %in% names(routing))) {
    names(routing) <- paste0("V", seq_len(ncol(routing)))
  }
  set.seed(seed)
  pool <- .sspice_elog_eligible(
    data, routing, min_non_missing_rate, max_levels
  )
  types <- c("AND", "OR", "XOR", "NOT", "IN")
  edits <- lapply(types, function(type) {
    .sspice_find_elog_edit(
      type, data, pool, min_outcome_count, max_search
    )
  })
  elog_names <- paste0("ELOG", seq_along(edits))
  elog_data <- as.data.frame(
    setNames(lapply(edits, .sspice_evaluate_elog, data = data), elog_names),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data_with_elog <- cbind(data, elog_data)
  basis <- do.call(rbind, lapply(seq_along(edits), function(i) {
    edit <- edits[[i]]
    data.frame(
      rule_id = paste0("SEM_ELOG", i),
      logical_form = edit$type,
      auxiliary_node = elog_names[i],
      base_variables = paste(edit$variables, collapse = ","),
      consequent = edit$consequent,
      accepted_values = paste(edit$allowed, collapse = ","),
      accepted_elog_domain = "1,2",
      expression = edit$expression,
      stringsAsFactors = FALSE
    )
  }))
  truth <- do.call(rbind, lapply(seq_along(edits), function(i) {
    index <- which(is.na(elog_data[[i]]))
    if (!length(index)) return(NULL)
    data.frame(
      record_id = data[[id_col]][index],
      expected_localized_var = elog_names[i],
      stringsAsFactors = FALSE
    )
  }))
  localization <- truth
  names(localization)[names(localization) == "expected_localized_var"] <-
    "localized_var"
  truth_key <- unique(paste(truth$record_id, truth$expected_localized_var, sep = "\u001F"))
  localized_key <- unique(paste(
    localization$record_id, localization$localized_var, sep = "\u001F"
  ))
  tp <- sum(localized_key %in% truth_key)
  fp <- sum(!localized_key %in% truth_key)
  fn <- sum(!truth_key %in% localized_key)
  metrics <- data.frame(
    n_expected = length(truth_key),
    n_localized = length(localized_key),
    true_positive = tp,
    false_positive = fp,
    false_negative = fn,
    precision = if (tp + fp) tp / (tp + fp) else NA_real_,
    recall = if (tp + fn) tp / (tp + fn) else NA_real_,
    exact_match = identical(sort(truth_key), sort(localized_key)),
    stringsAsFactors = FALSE
  )
  result <- list(
    edits = edits,
    extended_data = data_with_elog,
    semantic_basis = basis,
    ground_truth = truth,
    localization = localization,
    metrics = metrics,
    accepted_elog_domains = setNames(rep(list(c("1", "2")), 5L), elog_names)
  )
  class(result) <- "sspice_elog_validation"
  result
}

.sspice_elog_eligible <- function(data, routing, minimum_rate, max_levels) {
  jumps <- suppressWarnings(as.integer(routing$V6))
  candidates <- intersect(
    routing$V1[!is.na(jumps) & jumps == 0L],
    names(data)
  )
  keep <- vapply(candidates, function(variable) {
    values <- data[[variable]]
    rate <- mean(!is.na(values))
    levels <- length(unique(values[!is.na(values)]))
    rate >= minimum_rate && levels >= 2L && levels <= max_levels
  }, logical(1L))
  candidates <- candidates[keep]
  if (length(candidates) < 3L) stop("At least three eligible variables are required.")
  candidates
}

.sspice_elog_antecedent <- function(data, specification) {
  variables <- specification$variables
  equal <- function(x, value) {
    answer <- x == value
    answer[is.na(x)] <- NA
    answer
  }
  switch(
    specification$type,
    AND = equal(data[[variables[1]]], specification$value_1) &
      equal(data[[variables[2]]], specification$value_2),
    OR = equal(data[[variables[1]]], specification$value_1) |
      equal(data[[variables[2]]], specification$value_2),
    XOR = xor(equal(data[[variables[1]]], specification$value_1),
              equal(data[[variables[2]]], specification$value_2)),
    NOT = !equal(data[[variables[1]]], specification$value_1),
    IN = data[[variables[1]]] %in% specification$group_1,
    stop("Unknown logical form: ", specification$type)
  )
}

.sspice_find_elog_edit <- function(type, data, pool, minimum, max_search) {
  best <- NULL
  for (iteration in seq_len(max_search)) {
    number <- if (type %in% c("AND", "OR", "XOR")) 3L else 2L
    variables <- sample(pool, number, replace = FALSE)
    specification <- list(
      type = type,
      variables = variables,
      consequent = tail(variables, 1L)
    )
    observed <- function(variable) {
      values <- data[[variable]][!is.na(data[[variable]])]
      if (length(values)) sample(values, 1L) else NA
    }
    specification$value_1 <- observed(variables[1])
    if (type %in% c("AND", "OR", "XOR")) {
      specification$value_2 <- observed(variables[2])
    }
    if (type == "IN") {
      domain <- unique(data[[variables[1]]][!is.na(data[[variables[1]]])])
      if (length(domain) < 2L) next
      specification$group_1 <- sample(domain, min(2L, length(domain) - 1L))
    }
    active <- .sspice_elog_antecedent(data, specification)
    outcome <- data[[specification$consequent]]
    values <- outcome[active %in% TRUE & !is.na(outcome)]
    if (!length(values)) next
    counts <- sort(table(values), decreasing = TRUE)
    total <- sum(counts)
    valid <- counts >= minimum & (total - counts) >= minimum
    counts <- counts[valid]
    if (!length(counts)) next
    specification$allowed <- names(counts)[which.min(abs(as.numeric(counts) - total / 2))]
    elog <- .sspice_evaluate_elog(data, specification)
    frequencies <- c(
      sum(elog == "1", na.rm = TRUE),
      sum(elog == "2", na.rm = TRUE),
      sum(is.na(elog))
    )
    if (min(frequencies) < minimum) next
    specification$score <- min(frequencies)
    specification$expression <- paste0(
      type, "(", paste(variables[-length(variables)], collapse = ","),
      ") => ", specification$consequent, " IN {",
      paste(specification$allowed, collapse = ","), "}"
    )
    if (is.null(best) || specification$score > best$score) best <- specification
  }
  if (is.null(best)) stop("No suitable ELOG edit found for ", type, ".")
  best
}

.sspice_evaluate_elog <- function(specification, data) {
  active <- .sspice_elog_antecedent(data, specification)
  outcome <- data[[specification$consequent]]
  accepted <- outcome %in% specification$allowed
  accepted[is.na(outcome)] <- NA
  result <- rep(NA_character_, nrow(data))
  result[active %in% FALSE | is.na(active)] <- "2"
  result[active %in% TRUE & is.na(outcome)] <- "2"
  result[active %in% TRUE & accepted %in% TRUE] <- "1"
  result
}
