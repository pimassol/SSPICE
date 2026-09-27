make_correction_test_dag <- function() {
  routes <- matrix(
    c(
      1, 2, 1, 2, 0, 1, 2, 3,
      2, 3, 1, 3, 0, 0, NA, NA,
      3, 2, 1, 2, 0, 0, NA, NA
    ),
    nrow = 3L,
    byrow = TRUE
  )

  colnames(routes) <- paste0("V", seq_len(ncol(routes)))

  Bt <- matrix(
    c(
       1, -1,  0,
       1,  0, -1,
       0,  1, -1,
       0,  1, -1,
       0,  1, -1
    ),
    nrow = 5L,
    byrow = TRUE
  )

  colnames(Bt) <- c(
    "ROOT",
    "TARGET",
    "END"
  )

  list(
    routes_numeric = routes,
    Bt = Bt,
    varnames = c(
      "ROOT",
      "TARGET",
      "END"
    ),
    logical_edit_names = character()
  )
}

make_correction_test_donors <- function() {
  data.frame(
    RECORD_ID = seq_len(10L),
    FPROV = c(
      10, 10, 10, 10,
      20, 20, 20,
      10, 10, 20
    ),
    ROOT = c(
      1, 1, 1, 1,
      1, 1, 1,
      2, 2, 2
    ),
    TARGET = c(
      1, 1, 1, 2,
      2, 2, 3,
      3, 3, 1
    ),
    END = rep(1, 10L),
    stringsAsFactors = FALSE
  )
}

test_that("routing domains are extracted correctly", {
  dag <- make_correction_test_dag()

  domains <- SSPICE:::.sspice_domains_from_routes(
    routes = dag$routes_numeric,
    variable_names = dag$varnames
  )

  expect_identical(
    names(domains),
    dag$varnames
  )

  expect_equal(
    domains$ROOT,
    1:2
  )

  expect_equal(
    domains$TARGET,
    1:3
  )

  expect_equal(
    domains$END,
    1:2
  )
})

test_that("named zero-one fixed-variable indicators are interpreted correctly", {
  fixed <- SSPICE:::.sspice_normalize_fixed_variables(
    fixed_variables = c(
      RECORD_ID = 1,
      FPROV = 1,
      ROOT = 0,
      TARGET = 0
    ),
    names_data = c(
      "RECORD_ID",
      "FPROV",
      "ROOT",
      "TARGET"
    )
  )

  expect_identical(
    fixed,
    c(
      "RECORD_ID",
      "FPROV"
    )
  )

  expect_error(
    SSPICE:::.sspice_normalize_fixed_variables(
      fixed_variables = "UNKNOWN",
      names_data = c(
        "RECORD_ID",
        "FPROV"
      )
    ),
    "Unknown fixed variables"
  )
})

test_that("user priors are normalized and checked against domains", {
  domains <- list(
    TARGET = 1:3
  )

  priors <- SSPICE:::.sspice_validate_priors(
    priors = list(
      TARGET = c(
        "1" = 2,
        "2" = 3,
        "3" = 5
      )
    ),
    domains = domains
  )

  expect_equal(
    priors$TARGET,
    c(
      "1" = 0.2,
      "2" = 0.3,
      "3" = 0.5
    )
  )

  expect_error(
    SSPICE:::.sspice_validate_priors(
      priors = list(
        TARGET = c(
          "1" = 0.5,
          "4" = 0.5
        )
      ),
      domains = domains
    ),
    "outside its domain"
  )
})

test_that("the active structural predecessor is obtained from Bt", {
  dag <- make_correction_test_dag()

  active_record <- data.frame(
    ROOT = 1,
    TARGET = NA_real_,
    END = NA_real_
  )

  skipped_record <- data.frame(
    ROOT = 2,
    TARGET = NA_real_,
    END = 1
  )

  expect_identical(
    SSPICE:::.sspice_structural_predecessors(
      variable = "TARGET",
      record = active_record,
      dag = dag,
      active_only = TRUE
    ),
    "ROOT"
  )

  expect_length(
    SSPICE:::.sspice_structural_predecessors(
      variable = "TARGET",
      record = skipped_record,
      dag = dag,
      active_only = TRUE
    ),
    0L
  )

  expect_identical(
    SSPICE:::.sspice_structural_predecessors(
      variable = "TARGET",
      record = skipped_record,
      dag = dag,
      active_only = FALSE
    ),
    "ROOT"
  )
})

test_that("donor matrices condition on X(i-1) and FPROV", {
  dag <- make_correction_test_dag()
  donors <- make_correction_test_donors()

  record <- data.frame(
    RECORD_ID = 1001,
    FPROV = 10,
    ROOT = 1,
    TARGET = NA_real_,
    END = NA_real_
  )

  control <- correction_control(
    minimum_donors = 2L,
    top_k = 10L,
    allow_unconditional_donor = FALSE
  )

  distribution <- SSPICE:::.sspice_node_distribution(
    variable = "TARGET",
    record = record,
    dag = dag,
    domain = 1:3,
    donor_data = donors,
    strata = "FPROV",
    method = "donor",
    priors = list(),
    control = control
  )

  probabilities <- stats::setNames(
    distribution$probabilities,
    distribution$values
  )

  expect_identical(
    distribution$source,
    "conditional_donor"
  )

  expect_identical(
    distribution$predecessors_used,
    "ROOT"
  )

  expect_identical(
    distribution$strata_used,
    "FPROV"
  )

  expect_equal(
    distribution$donor_count,
    4L
  )

  expect_equal(
    unname(probabilities[c("1", "2")]),
    c(0.75, 0.25)
  )

  expect_false(
    "3" %in% names(probabilities)
  )
})

test_that("strata are relaxed while X(i-1) is retained", {
  dag <- make_correction_test_dag()
  donors <- make_correction_test_donors()

  record <- data.frame(
    RECORD_ID = 1002,
    FPROV = 99,
    ROOT = 1,
    TARGET = NA_real_,
    END = NA_real_
  )

  distribution <- SSPICE:::.sspice_node_distribution(
    variable = "TARGET",
    record = record,
    dag = dag,
    domain = 1:3,
    donor_data = donors,
    strata = "FPROV",
    method = "donor",
    priors = list(),
    control = correction_control(
      minimum_donors = 2L,
      allow_unconditional_donor = FALSE
    )
  )

  expect_identical(
    distribution$predecessors_used,
    "ROOT"
  )

  expect_identical(
    distribution$strata_used,
    ""
  )

  expect_equal(
    distribution$donor_count,
    7L
  )
})

test_that("user-prior and uniform fallbacks remain inside the node domain", {
  dag <- make_correction_test_dag()

  record <- data.frame(
    RECORD_ID = 1003,
    FPROV = 10,
    ROOT = 1,
    TARGET = NA_real_,
    END = NA_real_
  )

  prior_distribution <- SSPICE:::.sspice_node_distribution(
    variable = "TARGET",
    record = record,
    dag = dag,
    domain = 1:3,
    donor_data = NULL,
    strata = "FPROV",
    method = "donor",
    priors = list(
      TARGET = c(
        "1" = 0.1,
        "2" = 0.2,
        "3" = 0.7
      )
    ),
    control = correction_control()
  )

  expect_identical(
    prior_distribution$source,
    "user_prior"
  )

  expect_equal(
    prior_distribution$probabilities,
    c(0.1, 0.2, 0.7)
  )

  uniform_distribution <- SSPICE:::.sspice_node_distribution(
    variable = "TARGET",
    record = record,
    dag = dag,
    domain = 1:3,
    donor_data = NULL,
    strata = "FPROV",
    method = "uniform",
    priors = list(),
    control = correction_control()
  )

  expect_identical(
    uniform_distribution$source,
    "uniform"
  )

  expect_equal(
    uniform_distribution$values,
    as.character(1:3)
  )

  expect_equal(
    uniform_distribution$probabilities,
    rep(1 / 3, 3L)
  )
})

test_that("node proposals modify only the selected node", {
  dag <- make_correction_test_dag()
  donors <- make_correction_test_donors()

  record <- data.frame(
    RECORD_ID = 2001,
    FPROV = 10,
    ROOT = 1,
    TARGET = NA_real_,
    END = NA_real_
  )

  proposals <- SSPICE:::.sspice_node_mode_proposals(
    record = record,
    variable = "TARGET",
    role = "node",
    dag = dag,
    domains = list(
      ROOT = 1:2,
      TARGET = 1:3,
      END = 1:2
    ),
    donor_data = donors,
    strata = "FPROV",
    imputation_method = "donor",
    priors = list(),
    control = correction_control(
      minimum_donors = 2L
    )
  )

  expect_true(
    length(proposals) >= 1L
  )

  for (proposal in proposals) {
    expect_identical(
      proposal$changed_variables,
      "TARGET"
    )

    expect_identical(
      proposal$record$RECORD_ID,
      record$RECORD_ID
    )

    expect_identical(
      proposal$record$FPROV,
      record$FPROV
    )

    expect_identical(
      proposal$record$ROOT,
      record$ROOT
    )

    expect_true(
      proposal$record$TARGET %in% 1:3
    )
  }
})
