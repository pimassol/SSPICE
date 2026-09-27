make_matrix_cache_dag <- function() {
  list(
    varnames = c("PREVIOUS", "X"),
    record_id = "ID"
  )
}

test_that("records are partitioned into original EXACT and ERRONEOUS sets", {
  data <- data.frame(ID = 1:4, X = c(10L, 20L, 30L, 40L))
  report <- data.frame(
    ID = c(2L, 4L),
    varname_label = c("X", "X")
  )
  observed <- SSPICE:::.sspice_partition_correction_records(
    data = data,
    full_report = report,
    record_id = "ID"
  )
  expect_identical(observed$exact_indices, c(1L, 3L))
  expect_identical(observed$erroneous_indices, c(2L, 4L))
  expect_identical(observed$exact_records$ID, c(1L, 3L))
  expect_identical(observed$erroneous_records$ID, c(2L, 4L))
})

test_that("each X(i) uses only the immediately preceding X(i-1)", {
  dag <- list(
    varnames = c("A", "B", "X"),
    record_id = "ID"
  )
  expect_identical(
    SSPICE:::.sspice_previous_variable("X", dag, "ID"),
    "B"
  )
  expect_identical(
    SSPICE:::.sspice_previous_variable("B", dag, "ID"),
    "A"
  )
  expect_length(
    SSPICE:::.sspice_previous_variable("A", dag, "ID"),
    0L
  )
})

test_that("donor lookup ignores other Bt incoming arcs", {
  dag <- list(
    varnames = c("A", "B", "X"),
    record_id = "ID",
    Bt = matrix(
      c(
        1, 0, -1,
        0, 1, -1
      ),
      nrow = 2L,
      byrow = TRUE
    )
  )
  donors <- data.frame(
    ID = 1:4,
    A = c(1L, 1L, 1L, 1L),
    B = c(5L, 5L, 6L, 6L),
    X = c(10L, 10L, 20L, 20L)
  )
  cache <- SSPICE:::.sspice_build_donor_cache(
    donor_data = donors,
    dag = dag,
    record_id = "ID"
  )
  record <- data.frame(A = 1L, B = 6L, X = NA_integer_)
  observed <- SSPICE:::.sspice_cached_node_distribution(
    cache = cache,
    variable = "X",
    record = record,
    domain = c(10L, 20L),
    minimum_donors = 1L,
    allow_unconditional_donor = TRUE
  )
  expect_identical(observed$predecessors_used, "B")
  expect_identical(observed$values, "20")
  expect_equal(observed$probabilities, 1)
})

test_that("the cache is a precomputed immutable list of matrices", {
  donors <- data.frame(
    ID = 1:4,
    STRATUM = c("A", "A", "B", "A"),
    PREVIOUS = c(1L, 1L, 1L, 2L),
    X = c(10L, 20L, 30L, 40L)
  )
  cache <- SSPICE:::.sspice_build_donor_cache(
    donor_data = donors,
    dag = make_matrix_cache_dag(),
    record_id = "ID",
    strata = "STRATUM"
  )
  expect_s3_class(cache, "sspice_donor_cache")
  expect_true(cache$immutable)
  expect_identical(names(cache$matrices), c("PREVIOUS", "X"))
  expect_true(is.matrix(cache$matrices$X$predecessor_strata$probabilities))
  expect_true(is.matrix(cache$matrices$X$predecessor_only$probabilities))
  expect_true(is.matrix(cache$matrices$X$marginal$probabilities))
  expect_identical(cache$matrices$X$predecessor, "PREVIOUS")
})

test_that("X(i-1) equal to NA is retained as a conditioning mode", {
  donors <- data.frame(
    ID = 1:4,
    PREVIOUS = c(NA_integer_, NA_integer_, 1L, 1L),
    X = c(10L, 20L, 30L, 30L)
  )
  cache <- SSPICE:::.sspice_build_donor_cache(
    donor_data = donors,
    dag = make_matrix_cache_dag(),
    record_id = "ID",
    strata = character()
  )
  record <- data.frame(PREVIOUS = NA_integer_, X = NA_integer_)
  observed <- SSPICE:::.sspice_cached_node_distribution(
    cache = cache,
    variable = "X",
    record = record,
    domain = c(10L, 20L, 30L),
    minimum_donors = 1L,
    allow_unconditional_donor = TRUE
  )
  expect_identical(observed$predecessors_used, "PREVIOUS")
  expect_identical(observed$strata_used, "")
  expect_setequal(observed$values, c("10", "20"))
  expect_equal(observed$probabilities, c(0.5, 0.5))
  expect_equal(observed$donor_count, 2L)
})

test_that("optional multiple strata are used jointly then relaxed", {
  donors <- data.frame(
    ID = 1:5,
    S1 = c("A", "A", "A", "B", "B"),
    S2 = c("K", "K", "L", "K", "L"),
    PREVIOUS = c(1L, 1L, 1L, 1L, 2L),
    X = c(10L, 20L, 30L, 40L, 50L)
  )
  cache <- SSPICE:::.sspice_build_donor_cache(
    donor_data = donors,
    dag = make_matrix_cache_dag(),
    record_id = "ID",
    strata = c("S1", "S2")
  )
  record <- data.frame(S1 = "A", S2 = "K", PREVIOUS = 1L, X = NA)
  observed <- SSPICE:::.sspice_cached_node_distribution(
    cache = cache,
    variable = "X",
    record = record,
    domain = 10:50,
    minimum_donors = 2L,
    allow_unconditional_donor = TRUE
  )
  expect_identical(observed$predecessors_used, "PREVIOUS")
  expect_identical(observed$strata_used, "S1 | S2")
  expect_setequal(observed$values, c("10", "20"))
})

test_that("corrected records cannot update precomputed matrices", {
  donors <- data.frame(
    ID = 1:3,
    PREVIOUS = c(1L, 1L, 2L),
    X = c(10L, 20L, 30L)
  )
  cache <- SSPICE:::.sspice_build_donor_cache(
    donor_data = donors,
    dag = make_matrix_cache_dag(),
    record_id = "ID"
  )
  before <- cache$matrices$X$predecessor_only$probabilities
  donors$X[] <- 99L
  after <- cache$matrices$X$predecessor_only$probabilities
  expect_identical(after, before)
  expect_false("99" %in% colnames(after))
})

test_that("recombination preserves the original row order", {
  original <- data.frame(ID = 1:5, X = 11:15)
  corrected <- data.frame(ID = c(2L, 5L), X = c(200L, 500L))
  observed <- SSPICE:::.sspice_recombine_correction_data(
    original_data = original,
    corrected_records = corrected,
    corrected_indices = c(2L, 5L)
  )
  expect_identical(observed$ID, 1:5)
  expect_identical(observed$X, c(11L, 200L, 13L, 14L, 500L))
})
