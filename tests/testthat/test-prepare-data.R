make_prepare_test_dag <- function(
  varnames_original = c("UNIT_KEY", "QUESTION_A"),
  varnames = varnames_original
) {
  routes_original <- data.frame(
    V1 = varnames_original,
    stringsAsFactors = FALSE
  )

  routes_grouped_or <- data.frame(
    V1 = varnames,
    stringsAsFactors = FALSE
  )

  list(
    routes_original = routes_original,
    routes_grouped_OR = routes_grouped_or,
    varnames_original = varnames_original,
    varnames = varnames
  )
}

mock_identity_grouping <- function(local_environment) {
  testthat::local_mocked_bindings(
    generate_groups_OR = function(
      data,
      routing_file_ungrouped,
      routing_file_grouped
    ) {
      data
    },
    generate_groups_XOR = function(
      data,
      routing_file_ungrouped,
      routing_file_grouped,
      strict
    ) {
      data
    },
    clean_data = function(data) {
      data
    },
    .package = "SSPICE",
    .env = local_environment
  )
}

make_prepare_vaux_metadata <- function() {
  rbind(
    data.frame(
      V1 = "VAUX_TEST",
      V2 = 1,
      V3 = "QUESTION_A",
      V4 = "",
      V5 = "",
      V6 = 1,
      V7 = "",
      V8 = "",
      V9 = "NULL",
      stringsAsFactors = FALSE
    ),
    data.frame(
      V1 = "VAUX_TEST",
      V2 = 2,
      V3 = "QUESTION_A",
      V4 = "",
      V5 = "",
      V6 = 2,
      V7 = "",
      V8 = "",
      V9 = "NULL",
      stringsAsFactors = FALSE
    )
  )
}

test_that("prepare_data aligns ordinary input data with the DAG", {
  mock_identity_grouping(environment())

  test_data <- data.frame(
    UNIT_KEY = c("A", "B", "C"),
    QUESTION_A = c(1, 2, NA_real_),
    stringsAsFactors = FALSE
  )

  result <- prepare_data(
    data = test_data,
    dag = make_prepare_test_dag()
  )

  expect_s3_class(result, "sspice_prepared_data")
  expect_identical(result$data, test_data)
  expect_identical(names(result$data), c("UNIT_KEY", "QUESTION_A"))
  expect_equal(nrow(result$data), nrow(test_data))
  expect_identical(result$data$UNIT_KEY, test_data$UNIT_KEY)
  expect_null(result$vaux_metadata)
})

test_that("prepare_data constructs VAUX variables before DAG alignment", {
  mock_identity_grouping(environment())

  test_data <- data.frame(
    UNIT_KEY = 101:104,
    QUESTION_A = c(1, 2, NA, 3)
  )

  dag <- make_prepare_test_dag(
    varnames_original = c(
      "UNIT_KEY",
      "QUESTION_A",
      "VAUX_TEST"
    )
  )

  metadata <- make_prepare_vaux_metadata()

  result <- prepare_data(
    data = test_data,
    dag = dag,
    vaux = metadata,
    external_data_dir = tempdir()
  )

  expect_identical(
    names(result$data),
    c("UNIT_KEY", "QUESTION_A", "VAUX_TEST")
  )
  expect_equal(result$data$VAUX_TEST, c(1, 2, NA, NA))
  expect_equal(nrow(result$data), 4L)
  expect_identical(result$data$UNIT_KEY, test_data$UNIT_KEY)
  expect_identical(result$vaux_metadata, metadata)
})

test_that("prepare_data accepts CSV data and RDS DAG paths", {
  mock_identity_grouping(environment())

  test_directory <- tempfile(
    pattern = "sspice-prepare-files-"
  )
  dir.create(test_directory)
  on.exit(
    unlink(test_directory, recursive = TRUE),
    add = TRUE
  )

  data_file <- file.path(test_directory, "records.csv")
  dag_file <- file.path(test_directory, "dag.rds")

  test_data <- data.frame(
    UNIT_KEY = 1:2,
    QUESTION_A = c(1, NA)
  )

  utils::write.csv(
    test_data,
    data_file,
    row.names = FALSE,
    na = ""
  )

  saveRDS(
    make_prepare_test_dag(),
    dag_file
  )

  result <- prepare_data(
    data = data_file,
    dag = dag_file
  )

  expect_equal(result$data, test_data)
  expect_identical(result$varnames, c("UNIT_KEY", "QUESTION_A"))
})

test_that("prepare_data rejects incomplete DAG objects", {
  test_data <- data.frame(
    UNIT_KEY = 1,
    QUESTION_A = 1
  )

  incomplete_dag <- list(
    varnames = c("UNIT_KEY", "QUESTION_A")
  )

  expect_error(
    prepare_data(
      data = test_data,
      dag = incomplete_dag
    ),
    "DAG object is missing required elements"
  )
})

test_that("prepare_data rejects missing ungrouped routing variables", {
  mock_identity_grouping(environment())

  test_data <- data.frame(
    UNIT_KEY = 1:2
  )

  expect_error(
    prepare_data(
      data = test_data,
      dag = make_prepare_test_dag()
    ),
    "Variables required by the ungrouped routing are missing"
  )
})

test_that("prepare_data detects final data and DAG misalignment", {
  testthat::local_mocked_bindings(
    generate_groups_OR = function(
      data,
      routing_file_ungrouped,
      routing_file_grouped
    ) {
      data
    },
    generate_groups_XOR = function(
      data,
      routing_file_ungrouped,
      routing_file_grouped,
      strict
    ) {
      data$UNEXPECTED <- 1
      data
    },
    clean_data = function(data) {
      data
    },
    .package = "SSPICE"
  )

  test_data <- data.frame(
    UNIT_KEY = 1:2,
    QUESTION_A = c(1, 2)
  )

  expect_error(
    prepare_data(
      data = test_data,
      dag = make_prepare_test_dag()
    ),
    "Prepared data columns do not match the DAG variables"
  )
})
