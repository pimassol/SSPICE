make_test_localization_summary <- function(
  record_id,
  predicted_inconsistent
) {
  structure(
    list(
      summary_by_record = data.frame(
        record_id = record_id,
        predicted_inconsistent = predicted_inconsistent,
        stringsAsFactors = FALSE
      )
    ),
    class = "sspice_violation_localization"
  )
}

test_that("record-level metrics represent all confusion-matrix cells", {
  localization <- make_test_localization_summary(
    record_id = c("K1", "K2", "K3", "K4"),
    predicted_inconsistent = c(TRUE, FALSE, TRUE, FALSE)
  )

  ground_truth <- data.frame(
    SURVEY_KEY = c("K1", "K2", "K3", "K4"),
    is_violated = c(TRUE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = ground_truth,
    id_col = "SURVEY_KEY"
  )

  expect_true(result$ground_truth_available)
  expect_equal(nrow(result$comparison_by_record), 4L)
  expect_equal(nrow(result$truth), 4L)
  expect_equal(result$metrics$n_records, 4L)
  expect_equal(result$metrics$true_positive, 1L)
  expect_equal(result$metrics$true_negative, 1L)
  expect_equal(result$metrics$false_positive, 1L)
  expect_equal(result$metrics$false_negative, 1L)
  expect_equal(result$metrics$sensitivity, 0.5)
  expect_equal(result$metrics$specificity, 0.5)
  expect_equal(result$metrics$precision, 0.5)
  expect_equal(result$metrics$accuracy, 0.5)
  expect_equal(
    result$comparison_by_record$agreement,
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("validation works without a ground-truth report", {
  localization <- make_test_localization_summary(
    record_id = 1:2,
    predicted_inconsistent = c(FALSE, TRUE)
  )

  result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = NULL
  )

  expect_false(result$ground_truth_available)
  expect_s3_class(result$comparison_by_record, "data.frame")
  expect_equal(nrow(result$comparison_by_record), 0L)
  expect_equal(nrow(result$metrics), 0L)
  expect_equal(nrow(result$truth), 0L)
})

test_that("duplicated ground-truth rows are combined by record", {
  localization <- make_test_localization_summary(
    record_id = c("A", "B", "C"),
    predicted_inconsistent = c(TRUE, FALSE, TRUE)
  )

  ground_truth <- data.frame(
    CASE_CODE = c("A", "A", "B", "C", "C"),
    is_violated = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = ground_truth,
    id_col = "CASE_CODE"
  )

  expect_equal(nrow(result$truth), 3L)
  expect_identical(
    result$truth$record_id,
    c("A", "B", "C")
  )
  expect_identical(
    result$truth$truth_inconsistent,
    c(TRUE, FALSE, TRUE)
  )
  expect_true(all(result$comparison_by_record$agreement))
  expect_equal(result$metrics$accuracy, 1)
})

test_that("count and textual truth encodings are interpreted correctly", {
  localization <- make_test_localization_summary(
    record_id = c("1", "2", "3"),
    predicted_inconsistent = c(FALSE, TRUE, TRUE)
  )

  count_report <- data.frame(
    UNIT_KEY = c("1", "2", "3"),
    n_violations = c(0L, 1L, 3L),
    stringsAsFactors = FALSE
  )

  count_result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = count_report,
    id_col = "UNIT_KEY"
  )

  expect_identical(
    count_result$truth$truth_inconsistent,
    c(FALSE, TRUE, TRUE)
  )
  expect_equal(count_result$metrics$accuracy, 1)

  textual_report <- data.frame(
    UNIT_KEY = c("1", "2", "3"),
    violated = c("NO", "YES", "SI"),
    stringsAsFactors = FALSE
  )

  textual_result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = textual_report,
    id_col = "UNIT_KEY"
  )

  expect_identical(
    textual_result$truth$truth_inconsistent,
    c(FALSE, TRUE, TRUE)
  )
  expect_equal(textual_result$metrics$accuracy, 1)
})

test_that("a conventional identifier can be normalized to id_col", {
  localization <- make_test_localization_summary(
    record_id = c("10", "20"),
    predicted_inconsistent = c(FALSE, TRUE)
  )

  ground_truth <- data.frame(
    ID_RECORD = c("10", "20"),
    is_violated = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = ground_truth,
    id_col = "CUSTOM_KEY"
  )

  expect_true(result$ground_truth_available)
  expect_equal(result$metrics$accuracy, 1)
  expect_identical(
    result$truth$truth_inconsistent,
    c(FALSE, TRUE)
  )
})

test_that("undefined metric denominators return NA", {
  localization <- make_test_localization_summary(
    record_id = 1:3,
    predicted_inconsistent = rep(FALSE, 3L)
  )

  ground_truth <- data.frame(
    CASE_KEY = 1:3,
    is_violated = rep(FALSE, 3L)
  )

  result <- SSPICE:::validate_violation_localization(
    localization = localization,
    report = ground_truth,
    id_col = "CASE_KEY"
  )

  expect_true(is.na(result$metrics$sensitivity))
  expect_true(is.na(result$metrics$precision))
  expect_equal(result$metrics$specificity, 1)
  expect_equal(result$metrics$accuracy, 1)
})

test_that("invalid validation inputs are rejected clearly", {
  valid_localization <- make_test_localization_summary(
    record_id = c("A", "B"),
    predicted_inconsistent = c(FALSE, TRUE)
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = data.frame(record_id = 1),
      report = NULL
    ),
    "not a valid SSPICE localization result"
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = valid_localization,
      report = c(TRUE, FALSE),
      id_col = "CASE_KEY"
    ),
    "report must be NULL or a data frame"
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = valid_localization,
      report = data.frame(CASE_KEY = c("A", "B")),
      id_col = NULL
    ),
    "id_col is required"
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = valid_localization,
      report = data.frame(
        CASE_KEY = c("A", "B"),
        unknown_truth = c(0, 1)
      ),
      id_col = "CASE_KEY"
    ),
    "No recognizable ground-truth column"
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = valid_localization,
      report = data.frame(
        CASE_KEY = c("A", "B"),
        is_violated = c("MAYBE", "NO")
      ),
      id_col = "CASE_KEY"
    ),
    "uninterpretable values"
  )

  expect_error(
    SSPICE:::validate_violation_localization(
      localization = valid_localization,
      report = data.frame(
        CASE_KEY = "A",
        is_violated = FALSE
      ),
      id_col = "CASE_KEY"
    ),
    "analyzed records are absent"
  )
})
