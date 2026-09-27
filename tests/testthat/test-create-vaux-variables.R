make_vaux_row <- function(
  vaux,
  value,
  variable,
  source = "",
  key = "",
  exact = "",
  minimum = "",
  maximum = "",
  token = "NULL"
) {
  data.frame(
    V1 = vaux,
    V2 = value,
    V3 = variable,
    V4 = source,
    V5 = key,
    V6 = exact,
    V7 = minimum,
    V8 = maximum,
    V9 = token,
    stringsAsFactors = FALSE
  )
}

test_that("legacy logical VAUX rules retain their semantics", {
  test_data <- data.frame(
    UNIT_ID = 1:6,
    X = c(1, 1, 2, 2, NA, 1),
    Y = c(2, 4, 2, NA, 2, 3)
  )

  metadata <- rbind(
    make_vaux_row(
      "VAUX_TEST", 1, "X", exact = 1, token = "AND"
    ),
    make_vaux_row(
      "VAUX_TEST", 1, "Y", minimum = 2, maximum = 3
    ),
    make_vaux_row(
      "VAUX_TEST", 2, "X", exact = 2
    )
  )

  result <- SSPICE:::create_vaux_variables(
    inpdata = test_data,
    vauxs_data = metadata,
    extDFpath = tempdir(),
    routing_vnames = c(names(test_data), "VAUX_TEST")
  )

  expect_equal(
    result$VAUX_TEST,
    c(1, NA, 2, 2, NA, 1)
  )
  expect_equal(nrow(result), nrow(test_data))
  expect_identical(
    result[, names(test_data), drop = FALSE],
    test_data
  )
})

test_that("external VAUX rules preserve input rows and their order", {
  external_directory <- tempfile(
    pattern = "sspice-vaux-external-"
  )
  dir.create(external_directory)
  on.exit(
    unlink(external_directory, recursive = TRUE),
    add = TRUE
  )

  external_data <- data.frame(
    UNIT_ID = c(30, 10, 20),
    AGE = c(70, 40, 65)
  )

  utils::write.csv(
    external_data,
    file.path(external_directory, "HOUSEHOLD.csv"),
    row.names = FALSE,
    na = ""
  )

  test_data <- data.frame(
    UNIT_ID = c(20, 10, 30, 40),
    X = c(1, 2, 3, 4)
  )

  metadata <- rbind(
    make_vaux_row(
      "VAUX_AGE", 1, "AGE",
      source = "HOUSEHOLD", key = "UNIT_ID",
      minimum = 65, maximum = 110
    ),
    make_vaux_row(
      "VAUX_AGE", 2, "AGE",
      source = "HOUSEHOLD", key = "UNIT_ID",
      minimum = -2, maximum = 64
    )
  )

  result <- SSPICE:::create_vaux_variables(
    inpdata = test_data,
    vauxs_data = metadata,
    extDFpath = external_directory,
    routing_vnames = c(names(test_data), "VAUX_AGE")
  )

  expect_equal(result$UNIT_ID, test_data$UNIT_ID)
  expect_equal(result$X, test_data$X)
  expect_equal(result$VAUX_AGE, c(1, 2, 1, NA))
  expect_false("AGE" %in% names(result))
  expect_equal(nrow(result), 4L)
})

test_that("arithmetic VAUX rules classify cumulative values correctly", {
  test_data <- data.frame(
    UNIT_ID = 1:6,
    QPROP1 = c(100, 40, 40, NA, 70, 60),
    QPROP2 = c(NA, 60, 50, NA, 40, 40)
  )

  metadata <- rbind(
    make_vaux_row(
      "VAUX_PROP", 1, "QPROP1", token = "+"
    ),
    make_vaux_row(
      "VAUX_PROP", 1, "QPROP2", token = "<100"
    ),
    make_vaux_row(
      "VAUX_PROP", 2, "QPROP1", token = "+"
    ),
    make_vaux_row(
      "VAUX_PROP", 2, "QPROP2", token = "=100"
    )
  )

  result <- SSPICE:::create_vaux_variables(
    inpdata = test_data,
    vauxs_data = metadata,
    extDFpath = tempdir(),
    routing_vnames = c(names(test_data), "VAUX_PROP")
  )

  expect_equal(
    result$VAUX_PROP,
    c(NA, 2, 1, NA, NA, 2)
  )
  expect_equal(nrow(result), nrow(test_data))
})

test_that("IN and NOT IN expressions treat BLANK as an NA value", {
  test_data <- data.frame(
    UNIT_ID = 1:5,
    A = c(1, 1, 2, NA, 3),
    B = c(NA, 2, 2, NA, 1)
  )

  metadata <- rbind(
    make_vaux_row(
      "VAUX_SET", 1, "B",
      token = "NOT IN(BLANK,A) NULL"
    ),
    make_vaux_row(
      "VAUX_SET", 2, "B",
      token = "IN(BLANK) NULL"
    )
  )

  result <- SSPICE:::create_vaux_variables(
    inpdata = test_data,
    vauxs_data = metadata,
    extDFpath = tempdir(),
    routing_vnames = c(names(test_data), "VAUX_SET")
  )

  expect_equal(
    result$VAUX_SET,
    c(2, 1, NA, 2, 1)
  )
})

test_that("invalid VAUX metadata and missing routing variables are rejected", {
  test_data <- data.frame(
    UNIT_ID = 1,
    X = 1
  )

  incomplete_metadata <- data.frame(
    V1 = "VAUX_TEST",
    V2 = 1,
    V3 = "X"
  )

  expect_error(
    SSPICE:::create_vaux_variables(
      inpdata = test_data,
      vauxs_data = incomplete_metadata,
      extDFpath = tempdir(),
      routing_vnames = c("UNIT_ID", "X", "VAUX_TEST")
    ),
    "at least nine columns"
  )

  valid_metadata <- make_vaux_row(
    "VAUX_TEST", 1, "X", exact = 1
  )

  expect_error(
    SSPICE:::create_vaux_variables(
      inpdata = test_data,
      vauxs_data = valid_metadata,
      extDFpath = tempdir(),
      routing_vnames = c(
        "UNIT_ID", "X", "VAUX_TEST", "MISSING_VARIABLE"
      )
    ),
    "Variables required by the routing are missing"
  )
})
