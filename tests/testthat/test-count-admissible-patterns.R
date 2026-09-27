make_pattern_count_dag <- function(routes, Bt) {
  routes <- as.data.frame(
    routes,
    stringsAsFactors = FALSE
  )

  names(routes)[seq_len(ncol(routes))] <- paste0(
    "V",
    seq_len(ncol(routes))
  )

  list(
    routes_numeric = routes,
    Bt = Bt,
    varnames = colnames(Bt)
  )
}

test_that("a linear routing has the expected exact count", {
  Bt <- matrix(
    c(
      1L, -1L,
      1L, -1L
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )

  routes <- data.frame(
    V1 = 1:2,
    V2 = c(2L, 3L)
  )

  result <- count_admissible_patterns(
    dag = make_pattern_count_dag(routes, Bt),
    verbose = FALSE
  )

  expect_s3_class(
    result,
    "sspice_admissible_pattern_count"
  )
  expect_identical(
    as.character(result$n_admissible_patterns),
    "6"
  )
  expect_identical(
    as.character(result$n_all_unrestricted_patterns),
    "12"
  )
  expect_identical(result$root_variables, "A")
  expect_equal(result$n_variables, 2L)
  expect_equal(result$n_modal_rows, 2L)
  expect_true(result$exact_arbitrary_precision)
})

test_that("branching routes count required and skipped variables correctly", {
  Bt <- matrix(
    c(
      1L, -1L,  0L,
      1L,  0L, -1L,
      0L,  1L, -1L,
      0L,  1L, -1L
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  routes <- data.frame(
    V1 = 1:3,
    V2 = c(2L, 2L, 2L)
  )

  result <- count_admissible_patterns(
    dag = make_pattern_count_dag(routes, Bt),
    verbose = FALSE
  )

  expect_identical(
    as.character(result$n_admissible_patterns),
    "6"
  )
  expect_identical(
    as.character(result$n_all_unrestricted_patterns),
    "27"
  )
})

test_that("multiple inferred roots are counted jointly", {
  Bt <- matrix(
    c(
      1L, -1L,  0L,  0L,
      1L, -1L,  0L,  0L,
      0L,  0L,  1L, -1L,
      0L,  0L,  1L, -1L,
      0L,  0L,  1L, -1L
    ),
    nrow = 5L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C", "D"))
  )

  routes <- data.frame(
    V1 = 1:4,
    V2 = c(2L, 4L, 3L, 5L)
  )

  result <- count_admissible_patterns(
    dag = make_pattern_count_dag(routes, Bt),
    verbose = FALSE
  )

  expect_identical(result$root_variables, c("A", "C"))
  expect_identical(
    as.character(result$n_admissible_patterns),
    "120"
  )

  result_by_name <- count_admissible_patterns(
    dag = make_pattern_count_dag(routes, Bt),
    root_variables = c("C", "A"),
    verbose = FALSE
  )

  expect_identical(
    as.character(result_by_name$n_admissible_patterns),
    "120"
  )
})

test_that("large counts use exact arbitrary-precision integers", {
  variable_names <- LETTERS[1:9]
  Bt <- matrix(
    c(1L, rep(-1L, 8L)),
    nrow = 1L,
    dimnames = list(NULL, variable_names)
  )

  routes <- data.frame(
    V1 = seq_along(variable_names),
    V2 = c(1L, rep(1000000L, 8L))
  )

  result <- count_admissible_patterns(
    dag = make_pattern_count_dag(routes, Bt),
    verbose = FALSE
  )

  expected <- gmp::as.bigz(1000000L) ^ 8L

  expect_identical(
    as.character(result$n_admissible_patterns),
    as.character(expected)
  )
  expect_true(
    result$n_all_unrestricted_patterns >=
      result$n_admissible_patterns
  )
})

test_that("the memoization limit is enforced", {
  Bt <- matrix(
    c(
      1L, -1L,
      1L, -1L
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )

  routes <- data.frame(
    V1 = 1:2,
    V2 = c(2L, 2L)
  )

  expect_error(
    count_admissible_patterns(
      dag = make_pattern_count_dag(routes, Bt),
      max_memo_states = 1L,
      verbose = FALSE
    ),
    "Maximum number of memoized states exceeded"
  )
})

test_that("invalid DAG inputs are rejected clearly", {
  valid_Bt <- matrix(
    c(1L, -1L),
    nrow = 1L,
    dimnames = list(NULL, c("A", "B"))
  )

  valid_routes <- data.frame(
    V1 = 1:2,
    V2 = c(1L, 1L)
  )

  expect_error(
    count_admissible_patterns(
      dag = list(Bt = valid_Bt),
      verbose = FALSE
    ),
    "missing"
  )

  invalid_values <- valid_Bt
  invalid_values[1L, 2L] <- 2L

  expect_error(
    count_admissible_patterns(
      dag = make_pattern_count_dag(
        valid_routes,
        invalid_values
      ),
      verbose = FALSE
    ),
    "only -1, 0 and 1"
  )

  two_sources <- rbind(
    c(1L, 1L, -1L)
  )
  colnames(two_sources) <- c("A", "B", "C")

  expect_error(
    count_admissible_patterns(
      dag = make_pattern_count_dag(
        data.frame(V1 = 1:3, V2 = 1L),
        two_sources
      ),
      verbose = FALSE
    ),
    "exactly one source node"
  )

  backward_Bt <- matrix(
    c(-1L, 1L),
    nrow = 1L,
    dimnames = list(NULL, c("A", "B"))
  )

  expect_error(
    count_admissible_patterns(
      dag = make_pattern_count_dag(
        valid_routes,
        backward_Bt
      ),
      verbose = FALSE
    ),
    "not in topological order"
  )

  expect_error(
    count_admissible_patterns(
      dag = make_pattern_count_dag(
        valid_routes,
        valid_Bt
      ),
      root_variables = "UNKNOWN",
      verbose = FALSE
    ),
    "Invalid root variables"
  )
})
