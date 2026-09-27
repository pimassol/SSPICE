test_that("Step 10 correction script contains the complete workflow", {
  project_root <- testthat::test_path("..", "..")
  script_file <- file.path(
    project_root,
    "scripts",
    "10_correct_data.R"
  )

  testthat::skip_if_not(
    file.exists(script_file),
    "Development Step 10 script is not included in the built package."
  )

  expect_true(file.exists(script_file))
  expect_silent(parse(file = script_file))

  script_lines <- readLines(
    script_file,
    warn = FALSE
  )
  script_text <- paste(script_lines, collapse = "\n")

  required_fragments <- c(
    "questionnaire_extended_DAG.rds",
    "prepared_data_with_logical_edits.rds",
    "logical_edit_rules.csv",
    "cached_routing_check",
    "correction_cache_policy",
    "routing_cache_used",
    "routing_cache_refreshed",
    "exact_records",
    "erroneous_records",
    ".sspice_build_donor_cache",
    ".sspice_recombine_correction_data",
    "getFromNamespace",
    "getExportedValue",
    "correction_strata",
    "correction_control",
    "correct_data",
    "correction_result.rds",
    "corrected_questionnaire_data.rds",
    "correction_record_summary.csv",
    "correction_record_status.csv",
    "unresolved_records.csv",
    "correction_run_summary.csv"
  )

  expect_true(all(vapply(
    required_fragments,
    grepl,
    logical(1),
    x = script_text,
    fixed = TRUE
  )))

  removed_fragments <- c(
    "correction_log.rds",
    "correction_log.csv",
    "correction_energy_trace.rds",
    "correction_energy_trace.csv",
    "correction_result$corrections",
    "correction_result$energy_trace"
  )

  expect_false(any(vapply(
    removed_fragments,
    grepl,
    logical(1),
    x = script_text,
    fixed = TRUE
  )))

  expect_false(
    grepl(
      'config_value("correction_strata", "FPROV")',
      script_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      'config_value("correction_strata", character())',
      gsub("[[:space:]]+", " ", script_text),
      fixed = TRUE
    )
  )
})

test_that("Step 10 exposes optional operational configuration", {
  script_file <- file.path(
    testthat::test_path("..", ".."),
    "scripts",
    "10_correct_data.R"
  )

  testthat::skip_if_not(
    file.exists(script_file),
    "Development Step 10 script is not included in the built package."
  )

  script_text <- paste(
    readLines(script_file, warn = FALSE),
    collapse = "\n"
  )

  configurable_fields <- c(
    "correction_record_limit",
    "correction_initial_check_file",
    "correction_cache_policy",
    "correction_donor_data_file",
    "correction_donor_data_validated",
    "correction_donor_check_file",
    "correction_priors_file",
    "correction_domains_file",
    "correction_control_args",
    "correction_batch_size",
    "correction_resume_batches",
    "correction_fixed_variables",
    "correction_imputation_method",
    "correction_output_dir"
  )

  expect_true(all(vapply(
    configurable_fields,
    grepl,
    logical(1),
    x = script_text,
    fixed = TRUE
  )))

  expect_true(grepl(
    'if (!exists("config", inherits = FALSE))',
    script_text,
    fixed = TRUE
  ))
  expect_true(grepl("parallel_requested", script_text, fixed = TRUE))
  expect_true(grepl("parallel_workers", script_text, fixed = TRUE))
  expect_true(grepl(
    "original_exact_records",
    script_text,
    fixed = TRUE
  ))
})

test_that("Step 10 retains the calibrated SA parameters by default", {
  control <- correction_control()

  expect_equal(control$final_temperature, 1e-4)
  expect_equal(control$cooling_fraction, 0.90)
  expect_equal(control$lambda_change, 2)
  expect_equal(control$penalty_blank, 2)
  expect_equal(control$penalty_fill, 1)
  expect_equal(control$penalty_flip, 3)
  expect_equal(control$top_k, 10L)
  expect_equal(control$future_weight, 0.50)
  expect_false(control$use_parallel)
  expect_null(control$parallel_workers)
  expect_null(control$batch_size)
  expect_false(control$resume_batches)
})

test_that("Step 10 enables bounded atomic batch orchestration", {
  script_file <- file.path(
    testthat::test_path("..", ".."),
    "scripts",
    "10_correct_data.R"
  )

  testthat::skip_if_not(
    file.exists(script_file),
    "Development Step 10 script is not included in the built package."
  )
  script_text <- paste(readLines(script_file, warn = FALSE), collapse = "\n")

  expect_true(grepl(
    'config_value("correction_batch_size", 100L)',
    script_text,
    fixed = TRUE
  ))
  expect_true(grepl(
    'config_value("correction_resume_batches", TRUE)',
    script_text,
    fixed = TRUE
  ))
  expect_true(grepl("batch_output_dir", script_text, fixed = TRUE))
  expect_true(grepl("batches_reused", script_text, fixed = TRUE))
})
