testthat::test_that("routing cache is reused only while inputs are unchanged", {
  directory <- tempfile("sspice-cache-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  input_file <- file.path(directory, "input.txt")
  writeLines("version-one", input_file)
  cache_file <- file.path(directory, "routing-check.rds")
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  fake_check <- function(
      dset_data, routes, Bt, varkey, items.maps,
      path.items.maps, logical_edit_names) {
    calls$n <- calls$n + 1L
    list(
      full_report = data.frame(
        ID = character(),
        stringsAsFactors = FALSE
      ),
      marker = calls$n
    )
  }
  data <- data.frame(ID = c("A", "B"), X = c(1L, 2L))
  dag <- list(routes_numeric = matrix(1), Bt = matrix(1))

  refreshed <- cached_routing_check(
    data = data,
    dag = dag,
    record_id = "ID",
    cache_file = cache_file,
    input_files = c(source = input_file),
    cache_policy = "refresh",
    check_function = fake_check
  )
  testthat::expect_equal(calls$n, 1L)
  testthat::expect_false(refreshed$cache$used)
  testthat::expect_true(refreshed$cache$refreshed)
  testthat::expect_true(file.exists(cache_file))
  testthat::expect_true(file.exists(sub(
    "[.]rds$", ".manifest.rds", cache_file
  )))

  reused <- cached_routing_check(
    data = data,
    dag = dag,
    record_id = "ID",
    cache_file = cache_file,
    input_files = c(source = input_file),
    cache_policy = "auto",
    check_function = fake_check
  )
  testthat::expect_equal(calls$n, 1L)
  testthat::expect_true(reused$cache$used)
  testthat::expect_false(reused$cache$refreshed)
  testthat::expect_identical(reused$result$marker, 1L)

  writeLines("version-two", input_file)
  rebuilt <- cached_routing_check(
    data = data,
    dag = dag,
    record_id = "ID",
    cache_file = cache_file,
    input_files = c(source = input_file),
    cache_policy = "auto",
    check_function = fake_check
  )
  testthat::expect_equal(calls$n, 2L)
  testthat::expect_false(rebuilt$cache$used)
  testthat::expect_true(rebuilt$cache$refreshed)
  testthat::expect_match(
    rebuilt$cache$reason,
    "input_fingerprint_mismatch",
    fixed = TRUE
  )
})

testthat::test_that("readonly rejects stale cache and off does not write", {
  directory <- tempfile("sspice-cache-policy-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  input_file <- file.path(directory, "input.txt")
  writeLines("one", input_file)
  cache_file <- file.path(directory, "routing-check.rds")
  off_cache_file <- file.path(directory, "off-routing-check.rds")
  fake_check <- function(
      dset_data, routes, Bt, varkey, items.maps,
      path.items.maps, logical_edit_names) {
    list(full_report = data.frame(ID = character()))
  }
  data <- data.frame(ID = "A", X = 1L)
  dag <- list(routes_numeric = matrix(1), Bt = matrix(1))

  cached_routing_check(
    data = data,
    dag = dag,
    record_id = "ID",
    cache_file = cache_file,
    input_files = c(source = input_file),
    cache_policy = "refresh",
    check_function = fake_check
  )
  writeLines("two", input_file)
  testthat::expect_error(
    cached_routing_check(
      data = data,
      dag = dag,
      record_id = "ID",
      cache_file = cache_file,
      input_files = c(source = input_file),
      cache_policy = "readonly",
      check_function = fake_check
    ),
    "input_fingerprint_mismatch",
    fixed = TRUE
  )

  off <- cached_routing_check(
    data = data,
    dag = dag,
    record_id = "ID",
    cache_file = off_cache_file,
    input_files = c(source = input_file),
    cache_policy = "off",
    check_function = fake_check
  )
  testthat::expect_false(off$cache$used)
  testthat::expect_false(off$cache$refreshed)
  testthat::expect_false(file.exists(off_cache_file))
})
