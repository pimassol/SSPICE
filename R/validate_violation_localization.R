#' Validate violation localization against ground truth
#'
#' Internal SSPICE function used to evaluate record-level localization when a
#' validated ground-truth report is available. With `report = NULL`, it returns
#' empty validation tables without interrupting the workflow.
#'
#' @param localization Result returned by `localize_routing_violations()`.
#' @param report Optional data frame containing record-level ground truth.
#' @param id_col Name of the record identifier column. It is required only
#'   when a ground-truth report is supplied.
#' @return A list containing comparisons, metrics, and normalized truth values.
#' @keywords internal
validate_violation_localization <- function(
  localization,
  report = NULL,
  id_col = NULL
) {
  if (!is.list(localization) || !"summary_by_record" %in% names(localization)) {
    stop("localization is not a valid SSPICE localization result.", call. = FALSE)
  }

  if (is.null(report)) {
    return(list(
      ground_truth_available = FALSE,
      comparison_by_record = data.frame(),
      metrics = data.frame(),
      truth = data.frame()
    ))
  }

  if (!is.data.frame(report)) {
    stop("report must be NULL or a data frame.", call. = FALSE)
  }

  if (is.null(id_col) || length(id_col) != 1L || !nzchar(id_col)) {
    stop("id_col is required when a ground-truth report is supplied.",
         call. = FALSE)
  }

  if (!id_col %in% names(report)) {
    candidates <- names(report)[
      toupper(names(report)) %in% c("ID_RECORD", "RECORD_ID", "ID")
    ]
    if (!length(candidates)) {
      stop("No record identifier was found in the report.", call. = FALSE)
    }
    names(report)[names(report) == candidates[1L]] <- id_col
  }

  truth <- .sspice_extract_record_truth(report, id_col)
  predicted <- localization$summary_by_record
  index <- match(as.character(predicted$record_id), as.character(truth$record_id))

  if (anyNA(index)) {
    stop("Some analyzed records are absent from the ground-truth report.", call. = FALSE)
  }

  comparison <- predicted
  comparison$truth_inconsistent <- truth$truth_inconsistent[index]
  comparison$agreement <- comparison$predicted_inconsistent ==
    comparison$truth_inconsistent

  observed <- comparison$truth_inconsistent
  estimated <- comparison$predicted_inconsistent
  tp <- sum(observed & estimated)
  tn <- sum(!observed & !estimated)
  fp <- sum(!observed & estimated)
  fn <- sum(observed & !estimated)
  ratio <- function(a, b) if (b == 0L) NA_real_ else a / b

  metrics <- data.frame(
    n_records = nrow(comparison),
    true_positive = tp,
    true_negative = tn,
    false_positive = fp,
    false_negative = fn,
    sensitivity = ratio(tp, tp + fn),
    specificity = ratio(tn, tn + fp),
    precision = ratio(tp, tp + fp),
    accuracy = ratio(tp + tn, tp + tn + fp + fn),
    stringsAsFactors = FALSE
  )

  list(
    ground_truth_available = TRUE,
    comparison_by_record = comparison,
    metrics = metrics,
    truth = truth
  )
}

.sspice_as_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(!is.na(x) & x > 0)
  value <- toupper(trimws(as.character(x)))
  result <- rep(NA, length(value))
  result[value %in% c("TRUE", "T", "YES", "Y", "SI", "S", "1")] <- TRUE
  result[value %in% c("FALSE", "F", "NO", "N", "0")] <- FALSE
  result
}

.sspice_extract_record_truth <- function(report, id_col) {
  logical_columns <- intersect(
    c("is_violated", "IS_VIOLATED", "violated", "VIOLATED",
      "has_violation", "HAS_VIOLATION"),
    names(report)
  )
  count_columns <- intersect(
    c("n_violations", "N_VIOLATIONS", "n_violations_introduced",
      "N_VIOLATIONS_INTRODUCED", "n_errors", "N_ERRORS"),
    names(report)
  )

  if (length(logical_columns)) {
    truth_value <- .sspice_as_logical(report[[logical_columns[1L]]])
    truth_source <- logical_columns[1L]
  } else if (length(count_columns)) {
    value <- suppressWarnings(as.numeric(report[[count_columns[1L]]]))
    truth_value <- !is.na(value) & value > 0
    truth_source <- count_columns[1L]
  } else {
    stop("No recognizable ground-truth column was found.", call. = FALSE)
  }

  if (anyNA(truth_value)) {
    stop("Ground truth contains uninterpretable values.", call. = FALSE)
  }

  truth_by_record <- split(
    truth_value,
    as.character(report[[id_col]])
  )

  data.frame(
    record_id = names(truth_by_record),
    truth_inconsistent = vapply(truth_by_record, any, logical(1L)),
    truth_source = truth_source,
    stringsAsFactors = FALSE
  )
}
