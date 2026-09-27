make_test_synthetic_dag <- function() {
  variable_names <- c("CASE_KEY", "A", "B", "C")

  Bt <- matrix(
    c(
      1L, -1L,  0L,  0L,
      0L,  1L, -1L,  0L,
      0L,  1L,  0L, -1L,
      0L,  0L,  1L, -1L
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(NULL, variable_names)
  )

  A <- matrix(
    0L,
    nrow = 4L,
    ncol = 4L,
    dimnames = list(variable_names, variable_names)
  )

  A["CASE_KEY", "A"] <- 1L
  A["A", c("B", "C")] <- 1L
  A["B", "C"] <- 1L

  routes <- data.frame(
    V1 = variable_names,
    V2 = c(1L, 2L, 1L, 1L),
    V3 = c(1000L, 1L, 1L, 1L),
    V4 = c(9999L, 2L, 1L, 1L),
    V5 = c(0L, 0L, 0L, 0L),
    V6 = c(0L, 0L, 0L, 0L),
    V7 = c(NA, NA, NA, NA),
    V8 = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  list(
    routes_numeric = routes,
    varnames = variable_names,
    Bt = Bt,
    A = A
  )
}

test_that("admissible synthetic records follow the routing DAG", {
  dag <- make_test_synthetic_dag()

  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 100L,
    id_col = "CASE_KEY",
    seed = 123L
  )

  expect_s3_class(generated, "data.frame")
  expect_equal(nrow(generated), 100L)
  expect_identical(names(generated), dag$varnames)
  expect_false(anyDuplicated(generated$CASE_KEY) > 0L)
  expect_true(all(generated$A %in% c(1, 2)))

  first_route <- generated$A == 1
  second_route <- generated$A == 2

  expect_true(all(!is.na(generated$B[first_route])))
  expect_true(all(!is.na(generated$C[first_route])))
  expect_true(all(is.na(generated$B[second_route])))
  expect_true(all(!is.na(generated$C[second_route])))
})

test_that("synthetic generation is reproducible for a fixed seed", {
  dag <- make_test_synthetic_dag()

  first <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 40L,
    id_col = "CASE_KEY",
    seed = 777L
  )

  second <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 40L,
    id_col = "CASE_KEY",
    seed = 777L
  )

  expect_identical(first, second)
})

test_that("item mappings are used in generated data", {
  mapping_file <- tempfile(
    pattern = "sspice-synthetic-mapping-",
    fileext = ".csv"
  )
  on.exit(unlink(mapping_file), add = TRUE)

  utils::write.csv(
    data.frame(
      varname = c("A", "A"),
      route = c(1L, 2L),
      dset = c(10L, 20L)
    ),
    file = mapping_file,
    row.names = FALSE
  )

  generated <- SSPICE:::generate_admissible_records(
    dag = make_test_synthetic_dag(),
    n_records = 100L,
    id_col = "CASE_KEY",
    items_mapping_file = mapping_file,
    seed = 321L
  )

  expect_true(all(generated$A %in% c(10, 20)))
  expect_true(any(generated$A == 10))
  expect_true(any(generated$A == 20))
})

test_that("controlled violations produce a complete ground truth", {
  dag <- make_test_synthetic_dag()

  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 100L,
    id_col = "CASE_KEY",
    seed = 456L
  )

  injected <- SSPICE:::inject_routing_violations(
    data = generated,
    dag = dag,
    id_col = "CASE_KEY",
    proportion = 0.5,
    seed = 654L
  )

  expect_named(injected, c("data", "ground_truth"))
  expect_equal(nrow(injected$data), nrow(generated))
  expect_identical(names(injected$data), names(generated))
  expect_equal(nrow(injected$ground_truth), nrow(generated))
  expect_identical(
    injected$data$CASE_KEY,
    generated$CASE_KEY
  )
  expect_identical(
    injected$ground_truth$CASE_KEY,
    generated$CASE_KEY
  )
  expect_equal(sum(injected$ground_truth$is_violated), 50L)
  expect_true(
    all(
      injected$ground_truth$injection_type[
        injected$ground_truth$is_violated
      ] %in% c(
        "REMOVE_REQUIRED_VALUE",
        "FILL_SKIPPED_VALUE"
      )
    )
  )

  changed_columns <- vapply(
    seq_len(nrow(generated)),
    function(i) {
      sum(
        xor(
          is.na(generated[i, ]),
          is.na(injected$data[i, ])
        ) |
          (
            !is.na(generated[i, ]) &
              !is.na(injected$data[i, ]) &
              generated[i, ] != injected$data[i, ]
          )
      )
    },
    integer(1)
  )

  expect_equal(
    changed_columns,
    as.integer(injected$ground_truth$is_violated)
  )
})

test_that("candidate construction supports both violation types", {
  dag <- make_test_synthetic_dag()
  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 100L,
    id_col = "CASE_KEY",
    seed = 33L
  )

  selected_row <- which(generated$A == 2)[1L]
  domains <- SSPICE:::.sspice_synthetic_domains(
    dag$routes_numeric,
    dag$varnames
  )
  rows_by_source <- SSPICE:::.sspice_bt_rows_by_source(
    domains,
    nrow(dag$Bt)
  )

  candidates <- SSPICE:::.sspice_injection_candidates(
    record = generated[selected_row, , drop = FALSE],
    routes = dag$routes_numeric,
    varnames = dag$varnames,
    Bt = dag$Bt,
    domains = domains,
    rows_by_source = rows_by_source,
    id_col = "CASE_KEY"
  )

  expect_setequal(
    unique(candidates$type),
    c("REMOVE_REQUIRED_VALUE", "FILL_SKIPPED_VALUE")
  )
})

test_that("zero injection proportion leaves data unchanged", {
  dag <- make_test_synthetic_dag()

  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 25L,
    id_col = "CASE_KEY",
    seed = 91L
  )

  injected <- SSPICE:::inject_routing_violations(
    data = generated,
    dag = dag,
    id_col = "CASE_KEY",
    proportion = 0,
    seed = 92L
  )

  expect_identical(injected$data, generated)
  expect_false(any(injected$ground_truth$is_violated))
  expect_true(all(is.na(injected$ground_truth$injected_var)))
})

test_that("routing_check detects injected violations without false positives", {
  dag <- make_test_synthetic_dag()
  test_directory <- tempfile(
    pattern = "sspice-synthetic-routing-check-"
  )
  dir.create(test_directory)
  on.exit(
    unlink(test_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- file.path(test_directory, "items_mapping.csv")
  utils::write.csv(
    data.frame(
      varname = character(),
      route = numeric(),
      dset = numeric()
    ),
    file = mapping_file,
    row.names = FALSE
  )

  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 40L,
    id_col = "CASE_KEY",
    seed = 12L
  )

  admissible_result <- routing_check(
    dset_data = generated,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = "CASE_KEY",
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file)
  )

  expect_equal(nrow(admissible_result$full_report), 0L)

  injected <- SSPICE:::inject_routing_violations(
    data = generated,
    dag = dag,
    id_col = "CASE_KEY",
    proportion = 1,
    seed = 13L
  )

  routing_result <- routing_check(
    dset_data = injected$data,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = "CASE_KEY",
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file)
  )

  injected_ids <- as.character(
    injected$ground_truth$CASE_KEY[
      injected$ground_truth$is_violated
    ]
  )
  detected_ids <- as.character(
    unique(routing_result$full_report$CASE_KEY)
  )

  expect_equal(length(injected_ids), 40L)
  expect_gt(length(detected_ids), 0L)
  expect_true(all(detected_ids %in% injected_ids))
})

test_that("invalid generation and injection arguments are rejected", {
  dag <- make_test_synthetic_dag()

  expect_error(
    SSPICE:::generate_admissible_records(
      dag = dag,
      n_records = 0L
    ),
    "positive integer"
  )

  expect_error(
    SSPICE:::generate_admissible_records(
      dag = dag,
      optional_fill_probability = 1.5
    ),
    "between zero and one"
  )

  expect_error(
    SSPICE:::generate_admissible_records(
      dag = dag[c("Bt", "A")],
      n_records = 1L
    ),
    "DAG object is missing"
  )

  expect_error(
    SSPICE:::generate_admissible_records(
      dag = dag,
      n_records = 9001L,
      id_col = "CASE_KEY"
    ),
    "cannot provide unique values"
  )

  generated <- SSPICE:::generate_admissible_records(
    dag = dag,
    n_records = 5L,
    id_col = "CASE_KEY",
    seed = 1L
  )

  expect_error(
    SSPICE:::inject_routing_violations(
      data = generated,
      dag = dag,
      id_col = "UNKNOWN"
    ),
    "id_col is absent"
  )

  expect_error(
    SSPICE:::inject_routing_violations(
      data = generated,
      dag = dag,
      id_col = "CASE_KEY",
      proportion = -0.1
    ),
    "between zero and one"
  )
})
