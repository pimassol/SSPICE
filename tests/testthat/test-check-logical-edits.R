test_that("all missing logical edits are reported independently", {
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

  expect_equal(nrow(report), 3L)
  expect_named(
    report,
    c(
      "id_recprd", "CASE_ID", "errcode", "varname", "valname",
      "routevar", "routeval", "message", "varname_label",
      "routevar_label", "violation_source"
    )
  )
  expect_equal(report$CASE_ID, c("A", "A", "B"))
  expect_equal(report$varname_label, c("LEDIT_1", "LEDIT_2", "LEDIT_2"))
  expect_true(all(report$errcode == -1L))
  expect_true(all(report$violation_source == "LOGICAL_EDIT"))
  expect_identical(
    anyDuplicated(
      paste(report$CASE_ID, report$varname_label)
    ),
    0L
  )
})

test_that("values 1 and 2 are valid logical-edit outcomes", {
  test_data <- data.frame(
    RECORD_KEY = 1:3,
    LEDIT_1 = c(1, 2, 1),
    LEDIT_2 = c(2, 1, 2)
  )

  report <- SSPICE:::check_logical_edits(
    data = test_data,
    id_col = "RECORD_KEY",
    logical_edit_names = c("LEDIT_1", "LEDIT_2")
  )

  expect_s3_class(report, "data.frame")
  expect_equal(nrow(report), 0L)
  expect_true("RECORD_KEY" %in% names(report))
})

test_that("absence of declared logical edits preserves ordinary workflows", {
  test_data <- data.frame(
    GENERIC_ID = 1:2,
    X = c(1, 2)
  )

  report <- SSPICE:::check_logical_edits(
    data = test_data,
    id_col = "GENERIC_ID",
    logical_edit_names = NULL
  )

  expect_equal(nrow(report), 0L)
  expect_true("GENERIC_ID" %in% names(report))
})

test_that("invalid identifiers and missing edit columns are rejected", {
  test_data <- data.frame(
    ID = 1:2,
    LEDIT_1 = c(1, NA_real_)
  )

  expect_error(
    SSPICE:::check_logical_edits(
      data = test_data,
      id_col = "UNKNOWN_ID",
      logical_edit_names = "LEDIT_1"
    ),
    "id_col must identify"
  )

  expect_error(
    SSPICE:::check_logical_edits(
      data = test_data,
      id_col = "ID",
      logical_edit_names = c("LEDIT_1", "LEDIT_2")
    ),
    "Logical-edit variables missing from data: LEDIT_2"
  )
})
