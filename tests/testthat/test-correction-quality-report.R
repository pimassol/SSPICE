make_quality_dag <- function() {
  variables <- c("ID", "STRATUM", "QNUM", "QCAT", "TEXT", "VAUX_E", "LEDIT_1")
  list(
    varnames = variables,
    record_id = "ID",
    logical_edit_names = "LEDIT_1",
    routes_numeric = matrix(c(
      1, 1, 1, 99,
      2, 2, 1, 2,
      3, 1, 0, 100,
      4, 2, 1, 2,
      5, 0, NA, NA,
      6, 2, 1, 2,
      7, 2, 1, 2
    ), nrow = 7L, byrow = TRUE)
  )
}

make_quality_data <- function() {
  data.frame(
    ID = 1:3,
    STRATUM = c(1L, 1L, 2L),
    QNUM = c(10, 20, 30),
    QCAT = c(1L, 2L, 1L),
    TEXT = c("a", "b", "c"),
    VAUX_E = c(1L, 2L, 1L),
    LEDIT_1 = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

make_quality_result <- function(corrected) {
  structure(list(
    corrected_data = corrected,
    final_routing_check = list(full_report = data.frame()),
    record_summary = data.frame(
      ID = 1:2,
      final_routing_violations = 0L,
      final_logical_edit_violations = 0L,
      final_vaux_violations = 0L,
      final_energy = 0,
      resolved = TRUE
    ),
    unresolved_records = data.frame(),
    donor_diagnostics = list(supplied = 3L, retained = 3L),
    control = correction_control(minimum_donors = 1L),
    immutable_variables = c("ID", "STRATUM"),
    recalculable_variables = "LEDIT_1",
    sampled_vaux = character(),
    mode_locked_vaux = "VAUX_E",
    frozen_external_vaux = "VAUX_E",
    strata = "STRATUM",
    imputation_method = "donor",
    vaux_classification = list(logical_groups = character()),
    parallel = list(workers_used = 2L, batches_total = 1L, batches_reused = 0L)
  ), class = "sspice_correction_result")
}

test_that("quality report uses routing classification without heuristics", {
  original <- make_quality_data()
  corrected <- original
  result <- correction_quality_report(
    original_data = original,
    correction_result = make_quality_result(corrected),
    dag = make_quality_dag()
  )
  profile <- result$variable_profile
  expect_identical(
    profile$measurement_type[match("QNUM", profile$variable)],
    "QUANTITATIVE"
  )
  expect_identical(
    profile$measurement_type[match("QCAT", profile$variable)],
    "QUALITATIVE"
  )
  expect_identical(
    profile$measurement_type[match("TEXT", profile$variable)],
    "STRING"
  )
})

test_that("quality report describes canonical logical groups", {
  original <- make_quality_data()
  dag <- make_quality_dag()
  dag$routes_original <- data.frame(
    V1 = c("A", "B", "C", "D"),
    V2 = 1, V3 = 1, V4 = 1,
    V5 = c(1, 1, -2, -2),
    V6 = 0,
    stringsAsFactors = FALSE
  )
  result <- correction_quality_report(
    original_data = original,
    correction_result = make_quality_result(original),
    dag = dag
  )
  expect_equal(nrow(result$logical_groups), 2L)
  expect_setequal(result$logical_groups$group_type, c("OR", "XOR"))
  expect_true(all(result$logical_groups$member_count == 2L))
})

test_that("quality report measures changes and invasiveness", {
  original <- make_quality_data()
  corrected <- original
  corrected$QNUM[1L] <- 15
  corrected$QCAT[2L] <- NA_integer_
  result <- correction_quality_report(
    original_data = original,
    corrected_data = corrected,
    correction_result = make_quality_result(corrected),
    dag = make_quality_dag(),
    donor_data = original
  )
  expect_equal(nrow(result$changes), 2L)
  expect_setequal(result$changes$change_type, c("MODE_CHANGED", "BLANKED"))
  metrics <- stats::setNames(result$summary$value, result$summary$metric)
  expect_equal(unname(metrics["records_changed"]), 2)
  expect_equal(unname(metrics["cells_changed"]), 2)
  qnum <- result$uncertainty[result$uncertainty$variable == "QNUM", ]
  expect_identical(
    qnum$status,
    "QUANTITATIVE_VALUE_CHANGE_OUT_OF_SCOPE"
  )
  expect_true(is.na(qnum$entropy))
  expect_false(any(c("mean", "variance", "standard_deviation") %in%
    names(result$uncertainty)))
  expect_identical(qnum$structural_action, "VALUE_CHANGED")
  expect_true(all(c(
    "normalized_entropy", "support_size", "domain_size", "structural_action"
  ) %in% names(result$uncertainty)))
  qcat <- result$uncertainty[result$uncertainty$variable == "QCAT", ]
  expect_identical(qcat$status, "NOT_APPLICABLE")
  quantitative <- result$quantitative_structural_changes
  expect_equal(quantitative$filled[quantitative$variable == "QNUM"], 0L)
  expect_equal(quantitative$blanked[quantitative$variable == "QNUM"], 0L)
  expect_equal(
    quantitative$numeric_value_changed[quantitative$variable == "QNUM"], 1L
  )
})

test_that("quantitative reporting is limited to structural presence and absence", {
  original <- make_quality_data()
  corrected <- original
  original$QNUM[1L] <- NA_real_
  corrected$QNUM[1L] <- 15
  corrected$QNUM[2L] <- NA_real_
  result <- correction_quality_report(
    original_data = original,
    corrected_data = corrected,
    correction_result = make_quality_result(corrected),
    dag = make_quality_dag(),
    donor_data = make_quality_data()
  )
  quantitative <- result$quantitative_structural_changes
  expect_equal(quantitative$filled[quantitative$variable == "QNUM"], 1L)
  expect_equal(quantitative$blanked[quantitative$variable == "QNUM"], 1L)
  expect_equal(
    quantitative$structural_changes[quantitative$variable == "QNUM"], 2L
  )
  rows <- result$uncertainty[result$uncertainty$variable == "QNUM", ]
  expect_true(all(is.na(rows$entropy)))
  expect_true(all(is.na(rows$selected_probability)))
  expect_setequal(
    rows$status,
    c(
      "QUANTITATIVE_FILLED_STRUCTURALLY",
      "QUANTITATIVE_BLANKED_STRUCTURALLY"
    )
  )
})

test_that("missing initial check is reported as unavailable, not zero", {
  original <- make_quality_data()
  result <- correction_quality_report(
    original_data = original,
    correction_result = make_quality_result(original),
    dag = make_quality_dag()
  )
  metrics <- stats::setNames(result$summary$value, result$summary$metric)
  expect_true(is.na(metrics[["records_exact_initial"]]))
  expect_true(is.na(metrics[["records_erroneous_initial"]]))
  expect_true(is.na(metrics[["violations_before"]]))
})

test_that("quality report orders violations and writes artifacts", {
  original <- make_quality_data()
  corrected <- original
  initial <- list(full_report = data.frame(
    ID = c(1L, 1L, 2L),
    varname_label = c("QNUM", "QNUM", "QCAT"),
    message = "test",
    stringsAsFactors = FALSE
  ))
  output <- tempfile("sspice-quality-")
  result <- correction_quality_report(
    original_data = original,
    correction_result = make_quality_result(corrected),
    dag = make_quality_dag(),
    initial_check = initial,
    output_dir = output
  )
  expect_identical(result$violations_by_variable_before$variable[1L], "QNUM")
  expect_equal(result$violations_by_variable_before$violations[1L], 2L)
  expect_true(file.exists(file.path(output, "correction_quality_report.rds")))
  expect_true(file.exists(file.path(output, "correction_quality_report.html")))
  expect_true(file.exists(file.path(output, "changes.csv")))
  html <- paste(
    readLines(
      file.path(output, "correction_quality_report.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(html, "1. Dataset and correction setup", fixed = TRUE)
  expect_match(html, "4. Correction invasiveness", fixed = TRUE)
  expect_match(
    html,
    "7. Categorical imputation uncertainty and quantitative structural actions",
    fixed = TRUE
  )
  expect_match(html, "8. Unresolved records", fixed = TRUE)
  expect_match(html, "not multiple-imputation variance estimates", fixed = TRUE)
  expect_match(
    html,
    "For quantitative variables SSPICE reports only whether a value was filled or blanked",
    fixed = TRUE
  )
  expect_match(html, "Quantitative variables: presence/absence actions", fixed = TRUE)
  expect_false(grepl("mean variance", html, fixed = TRUE))
  expect_false(grepl("uniform-domain fallback", html, fixed = TRUE))
})

test_that("quality report does not modify its inputs", {
  original <- make_quality_data()
  corrected <- original
  result_object <- make_quality_result(corrected)
  original_copy <- original
  result_copy <- result_object
  invisible(correction_quality_report(
    original_data = original,
    correction_result = result_object,
    dag = make_quality_dag()
  ))
  expect_identical(original, original_copy)
  expect_identical(result_object, result_copy)
})

test_that("canonical OR and XOR nodes are classified as logical groups", {
  original <- make_quality_data()
  original$VOR1 <- 1L
  original$VXOR2 <- 1L
  dag <- make_quality_dag()
  dag$varnames <- c(dag$varnames, "VOR1", "VXOR2")
  dag$routes_numeric <- rbind(
    dag$routes_numeric,
    c(NA, 0, NA, NA),
    c(NA, 0, NA, NA)
  )
  dag$routes_original <- data.frame(
    V1 = c("A", "B", "C", "D"),
    V2 = 1, V3 = 1, V4 = 1,
    V5 = c(1, 1, -2, -2),
    V6 = 0,
    stringsAsFactors = FALSE
  )
  correction_result <- make_quality_result(original)
  correction_result$mode_locked_vaux <- c("VOR1", "VXOR2")
  correction_result$vaux_classification$logical_groups <- character()

  result <- correction_quality_report(
    original_data = original,
    correction_result = correction_result,
    dag = dag
  )
  roles <- stats::setNames(
    result$variable_profile$role,
    result$variable_profile$variable
  )
  metrics <- stats::setNames(result$summary$value, result$summary$metric)

  expect_identical(unname(roles[c("VOR1", "VXOR2")]),
                   c("LOGICAL_GROUP", "LOGICAL_GROUP"))
  expect_equal(unname(metrics["logical_groups"]), 2)
  expect_setequal(
    SSPICE:::.sspice_quality_canonical_logical_nodes(dag),
    c("VOR1", "VXOR2")
  )
})
