make_mock_path_report <- function(
  record_id = "CASE_ID",
  record_values = numeric(),
  variable_numbers = integer(),
  variable_labels = character(),
  error_codes = integer(),
  messages = character()
) {
  n <- length(record_values)

  report <- data.frame(
    id_recprd = seq_len(n),
    errcode = as.integer(error_codes),
    varname = as.integer(variable_numbers),
    valname = rep(NA_real_, n),
    routevar = rep(NA_integer_, n),
    routeval = rep(NA_real_, n),
    message = as.character(messages),
    varname_label = as.character(variable_labels),
    routevar_label = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  report[[record_id]] <- record_values
  report <- report[, c(
    "id_recprd", record_id, "errcode", "varname", "valname",
    "routevar", "routeval", "message", "varname_label",
    "routevar_label"
  )]
  report
}

mock_routing_dependencies <- function(path_report, local_environment) {
  testthat::local_mocked_bindings(
    check_all_paths = function(
      dset_data,
      routes,
      Bt,
      varkey,
      items.maps,
      path.items.maps
    ) {
      result <- path_report[, c(
        "id_recprd", varkey, "errcode", "varname", "valname",
        "routevar", "routeval"
      ), drop = FALSE]
      result
    },
    report_errors = function(errors, variable_names) {
      path_report
    },
    .package = "SSPICE",
    .env = local_environment
  )
}

test_that("routing_check preserves the ordinary routing-only workflow", {
  path_report <- make_mock_path_report(
    record_values = 102,
    variable_numbers = 2L,
    variable_labels = "QUESTION_A",
    error_codes = -1L,
    messages = "missing value where a filled value is required"
  )
  mock_routing_dependencies(path_report, environment())

  test_data <- data.frame(
    CASE_ID = c(101, 102),
    QUESTION_A = c(1, NA_real_)
  )

  result <- routing_check(
    dset_data = test_data,
    routes = matrix(0, nrow = 2, ncol = 6),
    Bt = matrix(
      0,
      nrow = 1,
      ncol = 2,
      dimnames = list(NULL, names(test_data))
    ),
    varkey = "CASE_ID",
    items.maps = "items_mapping.csv",
    path.items.maps = tempdir()
  )

  expect_s3_class(result, "sspice_routing_check")
  expect_identical(result$record_id, "CASE_ID")
  expect_length(result$logical_edit_names, 0L)
  expect_equal(result$full_report, path_report)
  expect_equal(nrow(result$summary_by_rec), 1L)
  expect_equal(result$summary_by_rec$count, 1L)
  expect_equal(result$summary_by_var$varname_label, "QUESTION_A")
})

test_that("routing_check reports every missing LEDIT independently", {
  path_report <- make_mock_path_report(
    record_values = c(11, 11),
    variable_numbers = c(2L, 3L),
    variable_labels = c("QUESTION_A", "LEDIT_A"),
    error_codes = c(-1L, -1L),
    messages = c(
      "missing value where a filled value is required",
      "missing value where a filled value is required"
    )
  )
  mock_routing_dependencies(path_report, environment())

  test_data <- data.frame(
    CASE_ID = c(11, 12, 13),
    QUESTION_A = c(NA, 1, 1),
    LEDIT_A = c(NA, 1, 2),
    LEDIT_B = c(NA, NA, 2)
  )

  result <- routing_check(
    dset_data = test_data,
    routes = matrix(0, nrow = 4, ncol = 6),
    Bt = matrix(
      0,
      nrow = 1,
      ncol = 4,
      dimnames = list(NULL, names(test_data))
    ),
    varkey = "CASE_ID",
    items.maps = "items_mapping.csv",
    path.items.maps = tempdir(),
    logical_edit_names = c("LEDIT_A", "LEDIT_B")
  )

  logical_report <- result$full_report[
    result$full_report$violation_source == "LOGICAL_EDIT",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(result$full_report), 4L)
  expect_equal(nrow(logical_report), 3L)
  expect_equal(
    paste(logical_report$CASE_ID, logical_report$varname_label),
    c("11 LEDIT_A", "11 LEDIT_B", "12 LEDIT_B")
  )
  expect_identical(
    anyDuplicated(
      paste(
        result$full_report$CASE_ID,
        result$full_report$varname_label
      )
    ),
    0L
  )
  expect_setequal(
    result$full_report$violation_source,
    c("ROUTING", "LOGICAL_EDIT")
  )
})

test_that("valid LEDIT outcomes do not alter an empty routing report", {
  path_report <- make_mock_path_report(
    record_id = "UNIT_KEY"
  )
  mock_routing_dependencies(path_report, environment())

  test_data <- data.frame(
    UNIT_KEY = c("A", "B"),
    QUESTION_A = c(1, 1),
    LEDIT_A = c(1, 2)
  )

  result <- routing_check(
    dset_data = test_data,
    routes = matrix(0, nrow = 3, ncol = 6),
    Bt = matrix(
      0,
      nrow = 1,
      ncol = 3,
      dimnames = list(NULL, names(test_data))
    ),
    varkey = "UNIT_KEY",
    items.maps = "items_mapping.csv",
    path.items.maps = tempdir(),
    logical_edit_names = "LEDIT_A"
  )

  expect_equal(nrow(result$full_report), 0L)
  expect_equal(nrow(result$summary_by_rec), 0L)
  expect_equal(nrow(result$summary_by_var), 0L)
  expect_identical(result$record_id, "UNIT_KEY")
})

test_that("routing_check rejects an invalid record identifier", {
  test_data <- data.frame(
    CASE_ID = 1:2,
    QUESTION_A = c(1, 2)
  )

  expect_error(
    routing_check(
      dset_data = test_data,
      routes = matrix(0, nrow = 2, ncol = 6),
      Bt = matrix(0, nrow = 1, ncol = 2),
      varkey = "UNKNOWN_KEY",
      items.maps = "items_mapping.csv",
      path.items.maps = tempdir()
    ),
    "varkey must identify one column"
  )
})
