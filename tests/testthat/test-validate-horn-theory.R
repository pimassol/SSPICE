make_test_horn_dag <- function() {
  variable_names <- c("A", "B", "C")

  Bt <- matrix(
    c(
      1L, -1L,  0L,
      1L, -1L,  0L,
      0L,  1L, -1L
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(NULL, variable_names)
  )

  A <- matrix(
    0L,
    nrow = 3L,
    ncol = 3L,
    dimnames = list(variable_names, variable_names)
  )

  A["A", "B"] <- 1L
  A["B", "C"] <- 1L

  routes <- data.frame(
    V1 = variable_names,
    V2 = c(2L, 1L, 1L),
    V3 = c(1L, 1L, 1L),
    V4 = c(2L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  list(
    routes_numeric = routes,
    varnames = variable_names,
    Bt = Bt,
    A = A
  )
}

horn_summary_value <- function(result, measure) {
  result$summary$value[
    result$summary$measure == measure
  ]
}

horn_comparison_value <- function(result, comparison) {
  result$stored_rule_comparison$value[
    result$stored_rule_comparison$comparison == comparison
  ]
}

test_that("a canonical routing DAG generates a valid Horn theory", {
  result <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 25L,
    max_seed_size = 3L,
    seed = 123L
  )

  expect_s3_class(result, "sspice_horn_validation")
  expect_true(all(result$canonical_tests$passed))
  expect_true(all(result$closure_tests$equivalent))
  expect_equal(nrow(result$closure_failures), 0L)
  expect_true(all(result$minimality_tests$indispensable))
  expect_equal(nrow(result$expanded_items), 3L)
  expect_equal(nrow(result$elementary_rules), 3L)
  expect_equal(
    horn_summary_value(result, "n_bt_rows"),
    3
  )
  expect_equal(
    horn_summary_value(result, "n_elementary_rules"),
    3
  )
})

test_that("elementary Horn literals preserve modal semantics", {
  result <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 0L
  )

  expected_keys <- paste(
    c("VALUE:A=1", "VALUE:A=2", "VALUE:B=1"),
    c("FILLED:B", "FILLED:B", "FILLED:C"),
    sep = "\u001F"
  )

  expect_setequal(result$elementary_rules$key, expected_keys)
  expect_setequal(
    result$elementary_rules$rule_type,
    "TARGET_FILLED"
  )
  expect_setequal(
    result$elementary_rules$bt_row,
    1:3
  )
})

test_that("technical value-to-filled bridges can be enabled or disabled", {
  with_bridges <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 0L,
    add_bridges = TRUE
  )

  without_bridges <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 0L,
    add_bridges = FALSE
  )

  expect_true(
    any(
      with_bridges$closure_basis$rule_type ==
        "VALUE_TO_FILLED_BRIDGE"
    )
  )
  expect_false(
    any(
      without_bridges$closure_basis$rule_type ==
        "VALUE_TO_FILLED_BRIDGE"
    )
  )
  expect_equal(
    nrow(without_bridges$closure_basis),
    nrow(without_bridges$elementary_rules)
  )
  expect_true(all(with_bridges$closure_tests$equivalent))
  expect_true(all(without_bridges$closure_tests$equivalent))
})

test_that("RULES.dat is reproduced exactly", {
  rules_file <- tempfile(
    pattern = "sspice-horn-rules-",
    fileext = ".dat"
  )
  on.exit(unlink(rules_file), add = TRUE)

  writeLines(
    c(
      "A(1) B< )",
      "A(2) B< )",
      "B(1) C< )"
    ),
    con = rules_file,
    useBytes = TRUE
  )

  result <- validate_horn_theory(
    dag = make_test_horn_dag(),
    rules_file = rules_file,
    n_random = 0L
  )

  expect_equal(
    horn_comparison_value(result, "GENERATED_ATOMIC_RULES"),
    3
  )
  expect_equal(
    horn_comparison_value(result, "STORED_ATOMIC_RULES"),
    3
  )
  expect_equal(
    horn_comparison_value(result, "ONLY_GENERATED"),
    0
  )
  expect_equal(
    horn_comparison_value(result, "ONLY_STORED"),
    0
  )
  expect_true(
    as.logical(
      horn_comparison_value(result, "EXACT_EQUALITY")
    )
  )
})

test_that("differences from a stored rule file are reported", {
  rules_file <- tempfile(
    pattern = "sspice-horn-wrong-rules-",
    fileext = ".dat"
  )
  on.exit(unlink(rules_file), add = TRUE)

  writeLines(
    c(
      "A(1) B< )",
      "A(2) B< )"
    ),
    con = rules_file,
    useBytes = TRUE
  )

  result <- validate_horn_theory(
    dag = make_test_horn_dag(),
    rules_file = rules_file,
    n_random = 0L
  )

  expect_false(
    as.logical(
      horn_comparison_value(result, "EXACT_EQUALITY")
    )
  )
  expect_equal(
    horn_comparison_value(result, "ONLY_GENERATED"),
    1
  )
  expect_equal(
    horn_comparison_value(result, "ONLY_STORED"),
    0
  )
})

test_that("item mappings are applied before RULES.dat comparison", {
  test_directory <- tempfile(
    pattern = "sspice-horn-mapping-"
  )
  dir.create(test_directory)
  on.exit(
    unlink(test_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- file.path(test_directory, "items_mapping.csv")
  rules_file <- file.path(test_directory, "RULES.dat")

  utils::write.csv(
    data.frame(
      varname = c("A", "A"),
      route = c(1L, 2L),
      dset = c(10L, 20L)
    ),
    file = mapping_file,
    row.names = FALSE
  )

  writeLines(
    c(
      "A(10) B< )",
      "A(20) B< )",
      "B(1) C< )"
    ),
    con = rules_file,
    useBytes = TRUE
  )

  result <- validate_horn_theory(
    dag = make_test_horn_dag(),
    rules_file = rules_file,
    items_mapping_file = mapping_file,
    n_random = 0L
  )

  expect_setequal(
    result$expanded_items$item_value,
    c(10, 20, 1)
  )
  expect_true(
    as.logical(
      horn_comparison_value(result, "EXACT_EQUALITY")
    )
  )
})

test_that("random closure tests are reproducible", {
  first <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 20L,
    max_seed_size = 3L,
    seed = 777L
  )

  second <- validate_horn_theory(
    dag = make_test_horn_dag(),
    n_random = 20L,
    max_seed_size = 3L,
    seed = 777L
  )

  expect_identical(first$closure_tests, second$closure_tests)
  expect_equal(
    sum(first$closure_tests$test_type == "RANDOM_MULTI_SEED"),
    20L
  )
})

test_that("invalid Horn-validation inputs are rejected clearly", {
  valid_dag <- make_test_horn_dag()

  expect_error(
    validate_horn_theory(
      dag = valid_dag[setdiff(names(valid_dag), "A")],
      n_random = 0L
    ),
    "missing"
  )

  cyclic_dag <- valid_dag
  cyclic_dag$A["C", "A"] <- 1L

  expect_error(
    validate_horn_theory(
      dag = cyclic_dag,
      n_random = 0L
    ),
    "not a DAG"
  )

  inconsistent_dag <- valid_dag
  inconsistent_dag$Bt <- inconsistent_dag$Bt[-1L, , drop = FALSE]

  expect_error(
    validate_horn_theory(
      dag = inconsistent_dag,
      n_random = 0L
    ),
    "differs from the number of modal transitions"
  )

  malformed_dag <- valid_dag
  malformed_dag$Bt[1L, ] <- c(1L, 1L, -1L)

  expect_error(
    validate_horn_theory(
      dag = malformed_dag,
      n_random = 0L
    ),
    "Invalid modal row in Bt"
  )

  expect_error(
    validate_horn_theory(
      dag = valid_dag,
      items_mapping_file = tempfile(),
      n_random = 0L
    ),
    "Item-mapping file not found"
  )
})
