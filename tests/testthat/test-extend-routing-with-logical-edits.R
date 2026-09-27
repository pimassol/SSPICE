make_test_base_routing <- function() {
  data.frame(
    V1 = c("ROOT", "MIDDLE", "END"),
    V2 = c(2, 2, 2),
    V3 = c(1, 1, 1),
    V4 = c(2, 2, 2),
    V5 = c(0, 0, 0),
    V6 = c(0, 0, 0),
    V7 = c(NA, NA, NA),
    V8 = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

make_test_edit_domains <- function() {
  data.frame(
    V1 = c("LEDIT_A", "LEDIT_A", "LEDIT_B", "LEDIT_B"),
    V2 = c("1", "2", "1", "2"),
    V3 = c("X", "X", "X", "X"),
    V4 = c("", "", "", ""),
    V5 = c("", "", "", ""),
    V6 = c("1", "2", "1", "2"),
    V7 = c("", "", "", ""),
    V8 = c("", "", "", ""),
    V9 = c("NULL", "NULL", "NULL", "NULL"),
    stringsAsFactors = FALSE
  )
}

test_that("logical-edit nodes are appended in declared order", {
  base_routing <- make_test_base_routing()

  extended <- SSPICE:::extend_routing_with_logical_edits(
    routing = base_routing,
    logical_edits = make_test_edit_domains()
  )

  expect_equal(nrow(extended), nrow(base_routing) + 2L)
  expect_equal(
    extended$V1,
    c(base_routing$V1, "LEDIT_A", "LEDIT_B")
  )
  expect_equal(
    tail(extended$V1, 2L),
    c("LEDIT_A", "LEDIT_B")
  )
  expect_equal(as.numeric(tail(extended$V2, 2L)), c(2, 2))
  expect_equal(as.numeric(tail(extended$V3, 2L)), c(1, 1))
  expect_equal(as.numeric(tail(extended$V4, 2L)), c(2, 2))
  expect_equal(as.numeric(tail(extended$V5, 2L)), c(0, 0))
  expect_equal(as.numeric(tail(extended$V6, 2L)), c(0, 0))
  expect_identical(
    attr(extended, "logical_edit_names"),
    c("LEDIT_A", "LEDIT_B")
  )
})

test_that("the original routing rows are not changed", {
  base_routing <- make_test_base_routing()

  extended <- SSPICE:::extend_routing_with_logical_edits(
    routing = base_routing,
    logical_edits = make_test_edit_domains()
  )

  retained <- extended[
    seq_len(nrow(base_routing)),
    names(base_routing),
    drop = FALSE
  ]

  retained_as_character <- as.data.frame(
    lapply(retained, as.character),
    stringsAsFactors = FALSE
  )
  base_as_character <- as.data.frame(
    lapply(base_routing, as.character),
    stringsAsFactors = FALSE
  )

  expect_identical(retained_as_character, base_as_character)
})

test_that("the extended routing produces a canonical acyclic DAG", {
  extended <- SSPICE:::extend_routing_with_logical_edits(
    routing = make_test_base_routing(),
    logical_edits = make_test_edit_domains()
  )

  dag <- build_DAG(
    routing = extended,
    analyze = FALSE,
    stop_on_cycle = TRUE
  )

  dag$logical_edit_names <- attr(
    extended,
    "logical_edit_names"
  )

  expect_s3_class(dag, "sspice_dag")
  expect_true(dag$graph_diagnostics$is_DAG)
  expect_equal(nrow(dag$A), 5L)
  expect_equal(ncol(dag$A), 5L)
  expect_equal(ncol(dag$Bt), 5L)
  expect_equal(nrow(dag$Bt), 8L)
  expect_equal(
    tail(dag$varnames, 2L),
    c("LEDIT_A", "LEDIT_B")
  )
  expect_identical(
    dag$logical_edit_names,
    c("LEDIT_A", "LEDIT_B")
  )
})

test_that("extended routing can be written and read again", {
  output_file <- tempfile(
    pattern = "sspice-extended-routing-",
    fileext = ".csv"
  )
  on.exit(unlink(output_file), add = TRUE)

  extended <- SSPICE:::extend_routing_with_logical_edits(
    routing = make_test_base_routing(),
    logical_edits = make_test_edit_domains(),
    output_file = output_file
  )

  expect_true(file.exists(output_file))

  restored <- utils::read.csv(
    output_file,
    header = FALSE,
    sep = ";",
    na.strings = ".",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(nrow(restored), nrow(extended))
  expect_equal(as.character(restored[[1L]]), as.character(extended$V1))
  expect_equal(
    tail(as.character(restored[[1L]]), 2L),
    c("LEDIT_A", "LEDIT_B")
  )
})

test_that("invalid or duplicated logical-edit domains are rejected", {
  duplicated_name <- make_test_edit_domains()[1:2, , drop = FALSE]
  duplicated_name$V1 <- "MIDDLE"

  expect_error(
    SSPICE:::extend_routing_with_logical_edits(
      routing = make_test_base_routing(),
      logical_edits = duplicated_name
    ),
    "already present in routing metadata"
  )

  nonconsecutive_domain <- make_test_edit_domains()[1:2, , drop = FALSE]
  nonconsecutive_domain$V2 <- c("1", "3")

  expect_error(
    SSPICE:::extend_routing_with_logical_edits(
      routing = make_test_base_routing(),
      logical_edits = nonconsecutive_domain
    ),
    "domain|accepted values"
  )
})
