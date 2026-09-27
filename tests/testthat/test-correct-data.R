make_correct_data_test_dag <- function() {
  variable_names <- c(
    "RECORD_ID",
    "FPROV",
    "X",
    "VAUX_EXT",
    "LEDIT_1"
  )

  routes <- matrix(
    c(
      1, 2, 1, 2, 0,
      2, 2, 1, 2, 0,
      3, 3, 1, 3, 0,
      4, 2, 1, 2, 0,
      5, 2, 1, 2, 0
    ),
    nrow = 5L,
    byrow = TRUE
  )

  list(
    routes_numeric = routes,
    Bt = matrix(
      0,
      nrow = 9L,
      ncol = length(variable_names),
      dimnames = list(NULL, variable_names)
    ),
    varnames = variable_names,
    logical_edit_names = "LEDIT_1",
    record_id = "RECORD_ID"
  )
}

make_correct_data_test_records <- function() {
  data.frame(
    RECORD_ID = 1:2,
    FPROV = c(10L, 20L),
    X = c(1L, 2L),
    VAUX_EXT = c(1L, 2L),
    LEDIT_1 = c(NA_integer_, 1L)
  )
}

make_external_vaux_metadata <- function() {
  data.frame(
    V1 = "VAUX_EXT",
    V2 = "1",
    V3 = "SOURCE_X",
    V4 = "external_source.csv",
    V5 = "RECORD_ID",
    V6 = "1",
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )
}

make_mock_record_result <- function(
    record,
    resolved = TRUE,
    new_value = record$X) {
  corrected <- record
  corrected$X <- new_value

  list(
    corrected_record = corrected,
    final_routing_violations = if (resolved) 0L else 1L,
    final_logical_edit_violations = 0L,
    final_vaux_violations = 0L,
    final_energy = if (resolved) 0 else 1,
    resolved = resolved
  )
}

test_that("missing donor data produce empty donor diagnostics", {
  result <- SSPICE:::.sspice_validated_donors(
    donor_data = NULL,
    dag = make_correct_data_test_dag(),
    record_id = "RECORD_ID",
    items_mapping_file = NULL
  )

  expect_null(result$data)
  expect_equal(result$supplied, 0L)
  expect_equal(result$retained, 0L)
})

test_that("only routing-valid donor records are retained", {
  testthat::local_mocked_bindings(
    routing_check = function(...) {
      list(
        full_report = data.frame(
          RECORD_ID = 2L,
          violation_source = "ROUTING"
        )
      )
    },
    .package = "SSPICE"
  )

  donors <- make_correct_data_test_records()

  result <- SSPICE:::.sspice_validated_donors(
    donor_data = donors,
    dag = make_correct_data_test_dag(),
    record_id = "RECORD_ID",
    items_mapping_file = NULL
  )

  expect_equal(result$supplied, 2L)
  expect_equal(result$retained, 1L)
  expect_identical(result$data$RECORD_ID, 1L)
})

test_that("prevalidated donors bypass the expensive routing check", {
  testthat::local_mocked_bindings(
    routing_check = function(...) stop("routing_check must not run"),
    .package = "SSPICE"
  )
  donors <- make_correct_data_test_records()
  result <- SSPICE:::.sspice_validated_donors(
    donor_data = donors,
    dag = make_correct_data_test_dag(),
    record_id = "RECORD_ID",
    items_mapping_file = NULL,
    already_validated = TRUE
  )
  expect_identical(result$data, donors)
  expect_equal(result$supplied, 2L)
  expect_equal(result$retained, 2L)
})

test_that("donor data must be aligned exactly with DAG variables", {
  donors <- make_correct_data_test_records()[
    ,
    rev(names(make_correct_data_test_records())),
    drop = FALSE
  ]

  expect_error(
    SSPICE:::.sspice_validated_donors(
      donor_data = donors,
      dag = make_correct_data_test_dag(),
      record_id = "RECORD_ID",
      items_mapping_file = NULL
    ),
    "columns must match DAG variables and order"
  )
})

test_that("correct_data orchestrates records and aggregates diagnostics", {
  captured_fixed <- list()
  captured_strata <- list()
  captured_frozen <- list()
  captured_mode_locked <- list()
  canonical_checks <- 0L

  testthat::local_mocked_bindings(
    routing_check = function(...) {
      canonical_checks <<- canonical_checks + 1L
      list(full_report = data.frame())
    },
    .sspice_validated_donors = function(
      donor_data,
      dag,
      record_id,
      items_mapping_file,
      already_validated = FALSE
    ) {
      list(
        data = donor_data,
        supplied = if (is.null(donor_data)) 0L else nrow(donor_data),
        retained = if (is.null(donor_data)) 0L else nrow(donor_data)
      )
    },
    .sspice_correct_record = function(
      record,
      original,
      dag,
      record_id,
      domains,
      donor_data,
      donor_cache,
      strata,
      imputation_method,
      priors,
      fixed_variables,
      control,
      vaux_metadata,
      logical_edit_metadata,
      external_data_dir,
      endogenous_vaux,
      frozen_values,
      mode_locked_values = list(),
      items_mapping_file = NULL,
      routing_oracle = NULL
    ) {
      identifier <- as.character(record[[record_id]][1L])
      captured_fixed[[identifier]] <<- fixed_variables
      captured_strata[[identifier]] <<- strata
      captured_frozen[[identifier]] <<- frozen_values
      captured_mode_locked[[identifier]] <<- mode_locked_values
      make_mock_record_result(
        record = record,
        resolved = TRUE,
        new_value = record$X + 1L
      )
    },
    .package = "SSPICE"
  )

  records <- make_correct_data_test_records()

  result <- correct_data(
    data = records,
    dag = make_correct_data_test_dag(),
    record_id = "RECORD_ID",
    donor_data = records,
    vaux_metadata = make_external_vaux_metadata(),
    immutable_variables = c(X = 0, FPROV = 1),
    strata = "FPROV",
    imputation_method = "uniform",
    control = correction_control(seed = 77L)
  )

  expect_s3_class(result, "sspice_correction_result")
  expect_equal(result$corrected_data$X, records$X + 1L)
  expect_identical(result$corrected_data$RECORD_ID, records$RECORD_ID)
  expect_identical(result$corrected_data$FPROV, records$FPROV)
  expect_identical(result$corrected_data$VAUX_EXT, records$VAUX_EXT)
  expect_true(all(result$record_summary$resolved))
  expect_equal(nrow(result$unresolved_records), 0L)
  expect_false("corrections" %in% names(result))
  expect_false("energy_trace" %in% names(result))
  expect_setequal(
    names(result$record_summary),
    c(
      "RECORD_ID",
      "final_routing_violations",
      "final_logical_edit_violations",
      "final_vaux_violations",
      "final_energy",
      "resolved"
    )
  )
  expect_equal(result$donor_diagnostics$supplied, 2L)
  expect_equal(result$donor_diagnostics$retained, 2L)
  expect_false(result$parallel$requested)
  expect_equal(result$parallel$workers_used, 1L)
  expect_identical(canonical_checks, 1L)

  expect_setequal(
    result$immutable_variables,
    c("RECORD_ID", "FPROV")
  )
  expect_identical(result$recalculable_variables, "LEDIT_1")
  expect_identical(result$mode_locked_vaux, "VAUX_EXT")
  expect_length(intersect(
    result$immutable_variables,
    result$recalculable_variables
  ), 0L)
  expect_length(intersect(
    result$immutable_variables,
    result$mode_locked_vaux
  ), 0L)
  expect_length(intersect(
    result$recalculable_variables,
    result$mode_locked_vaux
  ), 0L)
  expect_null(result$fixed_variables)

  required_fixed <- c(
    "RECORD_ID",
    "FPROV",
    "LEDIT_1"
  )

  expect_true(all(vapply(
    captured_fixed,
    function(x) all(required_fixed %in% x),
    logical(1)
  )))
  expect_true(all(vapply(
    captured_fixed,
    function(x) "VAUX_EXT" %in% x,
    logical(1)
  )))
  expect_false(any(vapply(
    captured_fixed,
    function(x) "X" %in% x,
    logical(1)
  )))
  expect_true(all(vapply(
    captured_frozen,
    function(x) {
      identical(names(x), "VAUX_EXT") &&
        !is.na(x$VAUX_EXT)
    },
    logical(1)
  )))
  expect_identical(result$mode_locked_vaux, "VAUX_EXT")
  expect_identical(result$frozen_external_vaux, "VAUX_EXT")
  expect_true(all(vapply(
    captured_mode_locked,
    function(x) {
      identical(names(x), "VAUX_EXT") &&
        !is.na(x$VAUX_EXT)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    captured_strata,
    identical,
    logical(1),
    "FPROV"
  )))
})

test_that("parallel correction controls are validated", {
  sequential <- correction_control()
  expect_false(sequential$use_parallel)
  expect_null(sequential$parallel_workers)

  parallel <- correction_control(
    use_parallel = TRUE,
    parallel_workers = 2L
  )
  expect_true(parallel$use_parallel)
  expect_equal(parallel$parallel_workers, 2L)

  expect_error(
    correction_control(use_parallel = NA),
    "use_parallel must be TRUE or FALSE"
  )
  expect_error(
    correction_control(parallel_workers = 0L),
    "parallel_workers must be NULL or a positive integer"
  )
  expect_error(
    correction_control(parallel_workers = 1.5),
    "parallel_workers must be NULL or a positive integer"
  )
})

test_that("parallel worker counts are bounded safely", {
  expect_equal(
    SSPICE:::.sspice_parallel_worker_count(10L, requested = 2L),
    2L
  )
  expect_equal(
    SSPICE:::.sspice_parallel_worker_count(2L, requested = 8L),
    2L
  )
  expect_gte(
    SSPICE:::.sspice_parallel_worker_count(10L),
    1L
  )
  expect_lte(
    SSPICE:::.sspice_parallel_worker_count(10L),
    10L
  )
})

test_that("parallel record RNG streams are deterministic and distinct", {
  first <- SSPICE:::.sspice_record_rng_streams(987L, 3L)
  second <- SSPICE:::.sspice_record_rng_streams(987L, 3L)

  expect_identical(first, second)
  expect_length(first, 3L)
  expect_false(identical(first[[1L]], first[[2L]]))
  expect_false(identical(first[[2L]], first[[3L]]))
})

test_that("the PSOCK backend preserves task order and closes cleanly", {
  testthat::skip_if_not_installed("doParallel")
  testthat::skip_if_not_installed("foreach")

  cluster <- parallel::makePSOCKcluster(2L)
  cluster_open <- TRUE
  on.exit({
    if (isTRUE(cluster_open)) {
      try(parallel::stopCluster(cluster), silent = TRUE)
    }
    foreach::registerDoSEQ()
  }, add = TRUE)
  doParallel::registerDoParallel(cluster)

  iterator <- foreach::foreach(
    index = 1:4,
    .inorder = TRUE
  )
  result <- foreach::`%dopar%`(
    iterator,
    index
  )

  expect_identical(unlist(result), 1:4)

  parallel::stopCluster(cluster)
  cluster_open <- FALSE
  foreach::registerDoSEQ()
  expect_equal(foreach::getDoParWorkers(), 1L)
})

test_that("unresolved records are reported explicitly", {
  testthat::local_mocked_bindings(
    routing_check = function(...) {
      data <- list(...)$dset_data
      list(full_report = data.frame(
        RECORD_ID = data$RECORD_ID[data$RECORD_ID == 2L],
        varname_label = "X",
        violation_source = "ROUTING",
        stringsAsFactors = FALSE
      ))
    },
    .sspice_validated_donors = function(...) {
      list(data = NULL, supplied = 0L, retained = 0L)
    },
    .sspice_correct_record = function(record, ...) {
      make_mock_record_result(
        record = record,
        resolved = record$RECORD_ID != 2L,
        new_value = record$X
      )
    },
    .package = "SSPICE"
  )

  result <- correct_data(
    data = make_correct_data_test_records(),
    dag = make_correct_data_test_dag(),
    record_id = "RECORD_ID",
    imputation_method = "uniform"
  )

  expect_equal(nrow(result$unresolved_records), 1L)
  expect_identical(result$unresolved_records$RECORD_ID, 2L)
  expect_false(result$unresolved_records$resolved)
})

test_that("the correction seed makes orchestration reproducible", {
  testthat::local_mocked_bindings(
    routing_check = function(...) list(full_report = data.frame()),
    .sspice_validated_donors = function(...) {
      list(data = NULL, supplied = 0L, retained = 0L)
    },
    .sspice_correct_record = function(record, ...) {
      make_mock_record_result(
        record = record,
        resolved = TRUE,
        new_value = sample(1:3, 1L)
      )
    },
    .package = "SSPICE"
  )

  control <- correction_control(seed = 987L)
  records <- make_correct_data_test_records()
  dag <- make_correct_data_test_dag()

  first <- correct_data(
    data = records,
    dag = dag,
    record_id = "RECORD_ID",
    imputation_method = "uniform",
    control = control
  )

  second <- correct_data(
    data = records,
    dag = dag,
    record_id = "RECORD_ID",
    imputation_method = "uniform",
    control = control
  )

  expect_identical(
    first$corrected_data,
    second$corrected_data
  )
  expect_identical(
    first$record_summary,
    second$record_summary
  )
})

test_that("correct_data rejects invalid user inputs clearly", {
  records <- make_correct_data_test_records()
  dag <- make_correct_data_test_dag()

  expect_error(
    correct_data(
      data = records[, -1L, drop = FALSE],
      dag = dag,
      record_id = "RECORD_ID"
    ),
    "columns must match DAG variables and order"
  )

  duplicated <- records
  duplicated$RECORD_ID[2L] <- duplicated$RECORD_ID[1L]

  expect_error(
    correct_data(
      data = duplicated,
      dag = dag,
      record_id = "RECORD_ID"
    ),
    "record_id values must be unique"
  )

  expect_error(
    correct_data(
      data = records,
      dag = dag,
      record_id = "RECORD_ID",
      strata = "UNKNOWN_STRATUM"
    ),
    "Unknown strata variables"
  )

  expect_error(
    correct_data(
      data = records,
      dag = dag,
      record_id = "RECORD_ID",
      vaux_metadata = make_external_vaux_metadata(),
      immutable_variables = "VAUX_EXT"
    ),
    "auxiliary variables cannot be listed in immutable_variables"
  )

  expect_error(
    correct_data(
      data = records,
      dag = dag,
      record_id = "RECORD_ID",
      immutable_variables = "LEDIT_1"
    ),
    "auxiliary variables cannot be listed in immutable_variables"
  )
})

test_that("batched correction checkpoints and resumes without recomputation", {
  calls <- 0L
  testthat::local_mocked_bindings(
    routing_check = function(...) list(full_report = data.frame()),
    .sspice_validated_donors = function(...) {
      list(data = NULL, supplied = 0L, retained = 0L)
    },
    .sspice_correct_record = function(record, ...) {
      calls <<- calls + 1L
      make_mock_record_result(
        record = record,
        resolved = TRUE,
        new_value = record$X
      )
    },
    .package = "SSPICE"
  )

  checkpoint_directory <- file.path(
    tempdir(),
    paste0("sspice-batch-test-", Sys.getpid(), "-", sample.int(1e6, 1L))
  )
  unlink(
    checkpoint_directory,
    recursive = TRUE,
    force = TRUE
  )
  control <- correction_control(
    seed = 987L,
    batch_size = 1L,
    resume_batches = TRUE
  )
  records <- make_correct_data_test_records()
  dag <- make_correct_data_test_dag()

  first <- correct_data(
    data = records,
    dag = dag,
    record_id = "RECORD_ID",
    imputation_method = "uniform",
    control = control,
    batch_output_dir = checkpoint_directory
  )

  expect_equal(calls, 2L)
  expect_equal(first$parallel$batch_size, 1L)
  expect_equal(first$parallel$batches_total, 2L)
  expect_equal(first$parallel$batches_reused, 0L)
  expect_true(all(file.exists(file.path(
    checkpoint_directory,
    c("batch_000001.rds", "batch_000002.rds", "batch_manifest.rds")
  ))))

  second <- correct_data(
    data = records,
    dag = dag,
    record_id = "RECORD_ID",
    imputation_method = "uniform",
    control = control,
    batch_output_dir = checkpoint_directory
  )

  expect_equal(calls, 2L)
  expect_equal(second$parallel$batches_reused, 2L)
  expect_identical(second$corrected_data, first$corrected_data)
  expect_identical(second$record_summary, first$record_summary)

  unlink(
    checkpoint_directory,
    recursive = TRUE,
    force = TRUE
  )
})
