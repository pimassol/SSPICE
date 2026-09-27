make_test_logical_edit_metadata <- function() {
  metadata_row <- function(edit, value, variable, exact = "", token) {
    data.frame(
      V1 = edit,
      V2 = as.character(value),
      V3 = variable,
      V4 = "",
      V5 = "",
      V6 = exact,
      V7 = "",
      V8 = "",
      V9 = token,
      stringsAsFactors = FALSE
    )
  }

  propr <- paste0("PROPR", 1:6)
  qprop <- paste0("QPROP", 1:6)

  ledit1_value2 <- do.call(
    rbind,
    Map(
      function(variable, token) {
        metadata_row("LEDIT_1", 2, variable, "blank", token)
      },
      propr,
      c(rep("AND", 5L), "NULL")
    )
  )

  ledit1_tokens <- c(
    "NOT IN(blank) AND",
    "(IN(blank) OR NOT IN(PROPR1)) AND",
    "(IN(blank) OR NOT IN(PROPR1,PROPR2)) AND",
    "(IN(blank) OR NOT IN(PROPR1,PROPR2,PROPR3)) AND",
    "(IN(blank) OR NOT IN(PROPR1,PROPR2,PROPR3,PROPR4)) AND",
    "(IN(blank) OR NOT IN(PROPR1,PROPR2,PROPR3,PROPR4,PROPR5)) NULL"
  )

  ledit1_value1 <- do.call(
    rbind,
    Map(
      function(variable, token) {
        metadata_row("LEDIT_1", 1, variable, "", token)
      },
      propr,
      ledit1_tokens
    )
  )

  ledit2_value2 <- do.call(
    rbind,
    Map(
      function(variable, token) {
        metadata_row("LEDIT_2", 2, variable, "blank", token)
      },
      propr,
      c(rep("AND", 5L), "NULL")
    )
  )

  ledit2_value1 <- do.call(
    rbind,
    Map(
      function(variable, token) {
        metadata_row("LEDIT_2", 1, variable, "", token)
      },
      qprop,
      c(rep("+", 5L), "=100 NULL")
    )
  )

  row.names(ledit1_value2) <- NULL
  row.names(ledit1_value1) <- NULL
  row.names(ledit2_value2) <- NULL
  row.names(ledit2_value1) <- NULL

  rbind(
    ledit1_value2,
    ledit1_value1,
    ledit2_value2,
    ledit2_value1
  )
}

test_that("LEDIT variables follow their declared semantics", {
  test_data <- data.frame(
    RECORD_KEY = 1:8,
    PROPR1 = c(NA, 1, 1, 1, 1, 1, 1, NA),
    PROPR2 = c(NA, NA, 2, 1, 2, NA, 2, 2),
    PROPR3 = c(NA, NA, NA, NA, NA, NA, NA, NA),
    PROPR4 = c(NA, NA, NA, NA, NA, NA, 4, NA),
    PROPR5 = rep(NA_real_, 8L),
    PROPR6 = rep(NA_real_, 8L),
    QPROP1 = c(NA, 100, 40, 50, 40, 100, 25, 100),
    QPROP2 = c(NA, NA, 60, 50, 50, NA, 25, NA),
    QPROP3 = rep(NA_real_, 8L),
    QPROP4 = c(NA, NA, NA, NA, NA, NA, 50, NA),
    QPROP5 = rep(NA_real_, 8L),
    QPROP6 = rep(NA_real_, 8L)
  )

  result <- SSPICE:::create_logical_edit_variables(
    inpdata = test_data,
    logical_edits_data = make_test_logical_edit_metadata()
  )

  expect_equal(
    result$LEDIT_1,
    c(2, 1, 1, NA, 1, 1, 1, NA)
  )
  expect_equal(
    result$LEDIT_2,
    c(2, 1, 1, 1, NA, 1, 1, 1)
  )
})

test_that("logical edits are appended without changing input variables", {
  test_data <- data.frame(
    ID = 1:2,
    PROPR1 = c(NA, 1),
    PROPR2 = c(NA, NA),
    PROPR3 = c(NA, NA),
    PROPR4 = c(NA, NA),
    PROPR5 = c(NA, NA),
    PROPR6 = c(NA, NA),
    QPROP1 = c(NA, 100),
    QPROP2 = c(NA, NA),
    QPROP3 = c(NA, NA),
    QPROP4 = c(NA, NA),
    QPROP5 = c(NA, NA),
    QPROP6 = c(NA, NA)
  )

  result <- SSPICE:::create_logical_edit_variables(
    inpdata = test_data,
    logical_edits_data = make_test_logical_edit_metadata()
  )

  expect_equal(
    names(result),
    c(names(test_data), "LEDIT_1", "LEDIT_2")
  )
  expect_identical(
    result[, names(test_data), drop = FALSE],
    test_data
  )
  expect_equal(nrow(result), nrow(test_data))
})

test_that("logical-edit definition diagnostics are retained", {
  test_data <- data.frame(
    ID = 1,
    PROPR1 = 1,
    PROPR2 = NA_real_,
    PROPR3 = NA_real_,
    PROPR4 = NA_real_,
    PROPR5 = NA_real_,
    PROPR6 = NA_real_,
    QPROP1 = 100,
    QPROP2 = NA_real_,
    QPROP3 = NA_real_,
    QPROP4 = NA_real_,
    QPROP5 = NA_real_,
    QPROP6 = NA_real_
  )

  result <- SSPICE:::create_logical_edit_variables(
    inpdata = test_data,
    logical_edits_data = make_test_logical_edit_metadata()
  )

  diagnostics <- attr(result, "logical_edit_definitions")

  expect_s3_class(diagnostics, "data.frame")
  expect_equal(nrow(diagnostics), 4L)
  expect_setequal(
    diagnostics$logical_edit,
    c("LEDIT_1", "LEDIT_2")
  )
  expect_setequal(
    diagnostics$assigned_value,
    c(1, 2)
  )
})

test_that("invalid metadata are rejected clearly", {
  metadata <- make_test_logical_edit_metadata()
  incomplete_metadata <- metadata[, 1:8, drop = FALSE]

  expect_error(
    SSPICE:::create_logical_edit_variables(
      inpdata = data.frame(ID = 1),
      logical_edits_data = incomplete_metadata
    ),
    "exactly nine columns"
  )

  missing_variable_data <- data.frame(
    ID = 1,
    PROPR1 = 1
  )

  expect_error(
    SSPICE:::create_logical_edit_variables(
      inpdata = missing_variable_data,
      logical_edits_data = metadata
    ),
    "Variables required by logical edits are missing"
  )
})
