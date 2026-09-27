make_linear_test_routing <- function() {
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

make_cyclic_test_routing <- function() {
  routing <- make_linear_test_routing()
  routing$V6[2L] <- 1
  routing$V7[2L] <- 1
  routing$V8[2L] <- "ROOT"
  routing
}

test_that("build_DAG constructs the expected linear graph", {
  dag <- build_DAG(
    routing = make_linear_test_routing(),
    analyze = FALSE
  )

  expect_s3_class(dag, "sspice_dag")
  expect_identical(dag$varnames, c("ROOT", "MIDDLE", "END"))
  expect_equal(dim(dag$A), c(3L, 3L))
  expect_equal(dim(dag$Bt), c(4L, 3L))
  expect_true(dag$graph_diagnostics$is_DAG)
  expect_equal(sum(dag$A != 0), 2L)
  expect_equal(unname(dag$A["ROOT", "MIDDLE"]), 1)
  expect_equal(unname(dag$A["MIDDLE", "END"]), 1)
  expect_equal(unname(dag$A["ROOT", "END"]), 0)
})

test_that("incidence rows have one source and one destination", {
  dag <- build_DAG(
    routing = make_linear_test_routing(),
    analyze = FALSE
  )

  expect_true(all(rowSums(dag$Bt == 1) == 1L))
  expect_true(all(rowSums(dag$Bt == -1) == 1L))
  expect_true(all(rowSums(dag$Bt) == 0))
})

test_that("source and path functions agree with the linear DAG", {
  dag <- build_DAG(
    routing = make_linear_test_routing(),
    analyze = FALSE
  )

  source_nodes <- SSPICE:::find_source_nodes(dag$A)
  shortest <- SSPICE:::find_shortest_path(
    A = dag$A,
    start_node = 1L,
    end_node = 3L,
    vnames = dag$varnames
  )
  longest <- SSPICE:::find_longest_path(
    A = dag$A,
    vnames = dag$varnames,
    start_node = 1L,
    end_node = 3L
  )

  longest_variables <- if (!is.null(longest$path)) {
    longest$path
  } else {
    longest$vars
  }

  expect_identical(unname(source_nodes), 1L)
  expect_equal(shortest$exists_path, 1)
  expect_equal(shortest$vars, c("ROOT", "MIDDLE", "END"))
  expect_equal(shortest$length, 3L)
  expect_equal(longest_variables, c("ROOT", "MIDDLE", "END"))
  expect_equal(longest$length, 3L)
})

test_that("build_DAG accepts a semicolon-separated routing file", {
  routing_file <- tempfile(
    pattern = "sspice-routing-",
    fileext = ".csv"
  )
  on.exit(unlink(routing_file), add = TRUE)

  utils::write.table(
    make_linear_test_routing(),
    file = routing_file,
    sep = ";",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    na = "."
  )

  dag <- build_DAG(
    routing = routing_file,
    separator = ";",
    analyze = FALSE
  )

  expect_s3_class(dag, "sspice_dag")
  expect_identical(dag$varnames, c("ROOT", "MIDDLE", "END"))
  expect_true(dag$graph_diagnostics$is_DAG)
})

test_that("cycles are detected and optionally rejected", {
  cyclic_routing <- make_cyclic_test_routing()

  expect_error(
    build_DAG(
      routing = cyclic_routing,
      analyze = FALSE,
      stop_on_cycle = TRUE
    ),
    "not a DAG"
  )

  cyclic_graph <- build_DAG(
    routing = cyclic_routing,
    analyze = FALSE,
    stop_on_cycle = FALSE
  )

  expect_false(cyclic_graph$graph_diagnostics$is_DAG)
  expect_false(SSPICE:::is_DAG(cyclic_graph$A))
})

test_that("invalid routing metadata are rejected", {
  duplicated_names <- make_linear_test_routing()
  duplicated_names$V1[2L] <- "ROOT"

  expect_error(
    build_DAG(
      routing = duplicated_names,
      analyze = FALSE
    ),
    "duplicated variable names"
  )

  too_few_columns <- make_linear_test_routing()[, 1:5]

  expect_error(
    build_DAG(
      routing = too_few_columns,
      analyze = FALSE
    ),
    "at least six columns"
  )
})
