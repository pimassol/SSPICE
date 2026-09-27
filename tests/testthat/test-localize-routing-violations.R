test_that("localization retains every logical-edit violation", {
  test_data <- data.frame(
    CASE_ID = c("A", "B", "C"),
    X = c(1, 1, 1),
    LEDIT_1 = c(NA_real_, 1, 2),
    LEDIT_2 = c(NA_real_, NA_real_, 2),
    stringsAsFactors = FALSE
  )

  report <- SSPICE:::check_logical_edits(
    data = test_data,
    id_col = "CASE_ID",
    logical_edit_names = c("LEDIT_1", "LEDIT_2")
  )

  routing_results <- list(
    full_report = report,
    record_id = "CASE_ID"
  )

  localization <- SSPICE:::localize_routing_violations(
    data = test_data,
    id_col = "CASE_ID",
    routing_results = routing_results
  )

  expect_s3_class(localization, "sspice_violation_localization")
  expect_equal(nrow(localization$detail), 3L)
  expect_equal(localization$detail$localized_var,
               c("LEDIT_1", "LEDIT_2", "LEDIT_2"))
  expect_true(all(
    localization$detail$violation_type == "LOGICAL_EDIT_VIOLATION"
  ))
  expect_true(all(localization$detail$expected_status == "VALUE_1_OR_2"))
  expect_true(all(localization$detail$observed_status == "BLANK"))

  summary <- localization$summary_by_record
  expect_equal(summary$record_id, c("A", "B", "C"))
  expect_equal(summary$n_localized_violations, c(2L, 1L, 0L))
  expect_equal(summary$predicted_inconsistent, c(TRUE, TRUE, FALSE))
})

test_that("localization is an exact extension of the supplied report", {
  test_data <- data.frame(
    SAMPLE_KEY = c(101, 102),
    LEDIT_1 = c(NA_real_, 1)
  )

  report <- SSPICE:::check_logical_edits(
    data = test_data,
    id_col = "SAMPLE_KEY",
    logical_edit_names = "LEDIT_1"
  )

  localization <- SSPICE:::localize_routing_violations(
    data = test_data,
    id_col = "SAMPLE_KEY",
    routing_results = list(
      full_report = report,
      record_id = "SAMPLE_KEY"
    )
  )

  expect_identical(
    localization$detail[, names(report), drop = FALSE],
    report
  )
  expect_equal(localization$detail$record_id, 101)
  expect_equal(localization$detail$localized_var, "LEDIT_1")
})

test_that("localization also handles a report without violations", {
  test_data <- data.frame(
    ID = 1:2,
    LEDIT_1 = c(1, 2)
  )

  report <- SSPICE:::check_logical_edits(
    data = test_data,
    id_col = "ID",
    logical_edit_names = "LEDIT_1"
  )

  localization <- SSPICE:::localize_routing_violations(
    data = test_data,
    id_col = "ID",
    routing_results = list(
      full_report = report,
      record_id = "ID"
    )
  )

  expect_equal(nrow(localization$detail), 0L)
  expect_equal(nrow(localization$summary_by_record), 2L)
  expect_true(all(!localization$summary_by_record$predicted_inconsistent))
  expect_true(all(localization$summary_by_record$n_localized_violations == 0L))
})
