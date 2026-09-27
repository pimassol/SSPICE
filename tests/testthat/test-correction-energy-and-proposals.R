make_energy_test_dag <- function() {
  routes <- matrix(
    c(
      1, 2, 1, 2, 0, 1, 2, 3,
      2, 3, 1, 3, 0, 0, NA, NA,
      3, 2, 1, 2, 0, 0, NA, NA
    ),
    nrow = 3L,
    byrow = TRUE
  )

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

  colnames(Bt) <- c("GATE", "X", "Y")

  list(
    routes_numeric = routes,
    Bt = Bt,
    varnames = c("GATE", "X", "Y"),
    logical_edit_names = character()
  )
}

make_energy_test_domains <- function() {
  list(
    GATE = 1:2,
    X = 1:3,
    Y = 1:2
  )
}

test_that("correction control exposes valid MCMC-SA parameters", {
  control <- correction_control()

  expect_s3_class(control, "sspice_correction_control")
  expect_equal(control$routing_weight, 1)
  expect_equal(control$logical_edit_weight, 1)
  expect_equal(control$lambda_change, 2)
  expect_equal(control$penalty_blank, 2)
  expect_equal(control$penalty_fill, 1)
  expect_equal(control$penalty_flip, 3)
  expect_equal(control$branch_fill_threshold, 0.5)
  expect_equal(control$cooling_fraction, 0.90)
  expect_equal(control$max_nodes_per_iteration, 1L)
  expect_equal(control$logical_repair_max_evaluations, 1000L)
  expect_equal(control$logical_repair_max_changes, 2L)
  expect_equal(control$logical_repair_beam_width, 10L)
  expect_true(control$freeze_strata)

  expect_error(
    correction_control(final_temperature = 0),
    "finite and non-negative"
  )
  expect_error(
    correction_control(branch_fill_threshold = 1.1),
    "between zero and one"
  )
  expect_error(
    correction_control(cooling_fraction = 1),
    "strictly between zero and one"
  )
  expect_error(
    correction_control(logical_repair_max_evaluations = 0L),
    "positive integers"
  )
})

test_that("the fast Bt oracle accepts numeric modes stored as character", {
  dag <- make_energy_test_dag()
  oracle <- SSPICE:::.sspice_build_routing_oracle(
    dag = dag,
    record_id = "GATE"
  )
  numeric_record <- data.frame(
    GATE = 1,
    X = 1,
    Y = 1
  )
  character_record <- data.frame(
    GATE = "1",
    X = "1",
    Y = "1",
    stringsAsFactors = FALSE
  )
  numeric_state <- SSPICE:::.sspice_fast_routing_state(
    numeric_record,
    oracle
  )
  character_state <- SSPICE:::.sspice_fast_routing_state(
    character_record,
    oracle
  )
  expect_equal(character_state$activation, numeric_state$activation)
  expect_equal(character_state$routing_violations, 0L)
  expect_equal(character_state$logical_edit_violations, 0L)
})

test_that("fast state evaluation does not call the full routing check", {
  dag <- make_energy_test_dag()
  oracle <- SSPICE:::.sspice_build_routing_oracle(dag, "GATE")
  record <- data.frame(GATE = 1, X = 1, Y = 1)
  testthat::local_mocked_bindings(
    routing_check = function(...) stop("full routing check was called"),
    .package = "SSPICE"
  )
  state <- SSPICE:::.sspice_evaluate_record_state(
    record = record,
    original = record,
    dag = dag,
    record_id = "GATE",
    fixed_variables = "GATE",
    control = correction_control(),
    routing_oracle = oracle
  )
  expect_equal(SSPICE:::.sspice_constraint_score(state), 0L)
})

test_that("the fast oracle reproduces per-edge routing localization", {
  dag <- make_energy_test_dag()
  oracle <- SSPICE:::.sspice_build_routing_oracle(dag, "GATE")
  record <- data.frame(
    GATE = 1L,
    X = NA_integer_,
    Y = 1L
  )

  fast <- SSPICE:::.sspice_fast_routing_state(record, oracle)

  mapping_file <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      varname = character(),
      new = character(),
      old = character(),
      stringsAsFactors = FALSE
    ),
    mapping_file,
    row.names = FALSE
  )

  reference <- routing_check(
    dset_data = record,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = "GATE",
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    logical_edit_names = character()
  )

  expect_identical(
    as.character(fast$full_report$varname_label),
    as.character(reference$full_report$varname_label)
  )
  expect_identical(
    as.character(fast$full_report$routevar_label),
    as.character(reference$full_report$routevar_label)
  )
  expect_identical(fast$full_report$varname_label, "X")
  expect_identical(fast$full_report$routevar_label, "GATE")
  expect_equal(unname(fast$activation[match("Y", dag$varnames)]), 0)
})

test_that("SA temperature cools linearly and then remains constant", {
  temperatures <- vapply(
    1:10,
    SSPICE:::.sspice_sa_temperature,
    numeric(1),
    max_iterations = 10L,
    initial_temperature = 4,
    final_temperature = 1e-4,
    cooling_fraction = 0.90
  )

  expect_equal(temperatures[1L], 4)
  expect_equal(temperatures[9L], 1e-4)
  expect_equal(temperatures[10L], 1e-4)
  expect_true(all(diff(temperatures[1:9]) < 0))
  expect_equal(
    diff(temperatures[1:9]),
    rep(diff(temperatures[1:2]), 8L),
    tolerance = 1e-12
  )
})

test_that("minimum-change distance distinguishes fill blank and flip", {
  original <- data.frame(
    X = 1,
    Y = NA_real_,
    Z = 1,
    FIXED = 1
  )

  proposed <- data.frame(
    X = 2,
    Y = 3,
    Z = NA_real_,
    FIXED = 2
  )

  control <- correction_control(
    penalty_fill = 1,
    penalty_blank = 2,
    penalty_flip = 3
  )

  expect_equal(
    SSPICE:::.sspice_change_distance(
      record = proposed,
      original = original,
      fixed_variables = "FIXED",
      control = control
    ),
    6
  )
})

test_that("future cost penalizes only additional local violations", {
  current <- list(
    routing_violations = 2L,
    logical_edit_violations = 1L
  )

  improved <- list(
    routing_violations = 1L,
    logical_edit_violations = 0L
  )

  worsened <- list(
    routing_violations = 3L,
    logical_edit_violations = 2L
  )

  expect_equal(
    SSPICE:::.sspice_future_cost(current, improved),
    0
  )
  expect_equal(
    SSPICE:::.sspice_future_cost(current, worsened),
    2
  )
})

test_that("node proposals use admissible modes and preserve metadata", {
  record <- data.frame(
    GATE = 1L,
    X = 1L,
    Y = NA_integer_
  )

  proposals <- SSPICE:::.sspice_node_mode_proposals(
    record = record,
    variable = "X",
    role = "node",
    dag = make_energy_test_dag(),
    domains = make_energy_test_domains(),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    control = correction_control(top_k = 10L)
  )

  expect_length(proposals, 2L)
  expect_setequal(
    vapply(proposals, function(x) x$new_values, character(1)),
    c("2", "3")
  )
  expect_true(all(vapply(
    proposals,
    function(x) x$move_type == "flip",
    logical(1)
  )))
  expect_true(all(vapply(
    proposals,
    function(x) x$proposal_source == "uniform",
    logical(1)
  )))
  expect_identical(record$X, 1L)
})

test_that("blank proposals affect only populated requested variables", {
  record <- data.frame(
    A = 1,
    B = NA_real_,
    C = 3
  )

  proposal <- SSPICE:::.sspice_blank_proposal(
    record = record,
    variables = c("A", "B"),
    move_type = "repair_skipped_branch"
  )

  expect_identical(proposal$changed_variables, "A")
  expect_true(is.na(proposal$record$A))
  expect_true(is.na(proposal$record$B))
  expect_equal(proposal$record$C, 3)
  expect_equal(proposal$proposal_source, "routing_structure")

  expect_null(
    SSPICE:::.sspice_blank_proposal(
      record = record,
      variables = "B",
      move_type = "blank"
    )
  )
})

test_that("gate proposals are generated only above the branch threshold", {
  dag <- make_energy_test_dag()
  domains <- make_energy_test_domains()
  context <- list(
    nodes = c("GATE", "X", "Y"),
    gates = "GATE",
    segment = c("X", "Y")
  )

  filled_record <- data.frame(
    GATE = 2L,
    X = 1L,
    Y = 1L
  )

  sparse_record <- data.frame(
    GATE = 2L,
    X = NA_integer_,
    Y = NA_integer_
  )

  filled_proposals <- SSPICE:::.sspice_generate_correction_proposals(
    record = filled_record,
    context = context,
    dag = dag,
    domains = domains,
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(branch_fill_threshold = 0.5)
  )

  sparse_proposals <- SSPICE:::.sspice_generate_correction_proposals(
    record = sparse_record,
    context = context,
    dag = dag,
    domains = domains,
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(branch_fill_threshold = 0.5)
  )

  expect_true(any(vapply(
    filled_proposals,
    function(x) x$move_type == "gate_flip",
    logical(1)
  )))
  expect_false(any(vapply(
    sparse_proposals,
    function(x) x$move_type == "gate_flip",
    logical(1)
  )))
  expect_true(any(vapply(
    filled_proposals,
    function(x) x$move_type == "repair_skipped_branch",
    logical(1)
  )))
})

test_that("fixed variables are excluded from every proposal", {
  record <- data.frame(
    GATE = 2L,
    X = 1L,
    Y = 1L
  )

  proposals <- SSPICE:::.sspice_generate_correction_proposals(
    record = record,
    context = list(
      nodes = c("GATE", "X", "Y"),
      gates = "GATE",
      segment = c("X", "Y")
    ),
    dag = make_energy_test_dag(),
    domains = make_energy_test_domains(),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = c("GATE", "X"),
    control = correction_control()
  )

  changed <- unique(unlist(lapply(
    proposals,
    `[[`,
    "changed_variables"
  )))

  expect_false("GATE" %in% changed)
  expect_false("X" %in% changed)
  expect_true("Y" %in% changed)
})

test_that("simulated annealing always accepts non-increasing energy", {
  expect_true(
    SSPICE:::.sspice_accept_sa_proposal(
      current_energy = 10,
      proposed_energy = 9,
      temperature = 0.01
    )
  )
  expect_true(
    SSPICE:::.sspice_accept_sa_proposal(
      current_energy = 10,
      proposed_energy = 10,
      temperature = 0.01
    )
  )

  set.seed(123L)

  expect_false(
    SSPICE:::.sspice_accept_sa_proposal(
      current_energy = 0,
      proposed_energy = 100,
      temperature = 1e-6
    )
  )
})

test_that("refreshing derived variables preserves every ordinary variable", {
  testthat::local_mocked_bindings(
    create_vaux_variables = function(
        inpdata, vauxs_data, extDFpath, routing_vnames) {
      result <- inpdata
      result$FASE <- NA_integer_
      result$VERSFASE <- NA_integer_
      result$SK3_ <- NA_integer_
      result$ARIL <- NA_integer_
      result$VAUX_END <- 2L
      result
    },
    create_logical_edit_variables = function(
        inpdata, logical_edits_data) {
      result <- inpdata
      result$FASE <- NA_integer_
      result$VERSFASE <- NA_integer_
      result$SK3_ <- NA_integer_
      result$ARIL <- NA_integer_
      result$LEDIT_1 <- 1L
      result
    },
    .package = "SSPICE"
  )

  record <- data.frame(
    ID = 1L,
    FASE = 80L,
    VERSFASE = 0L,
    SK3_ = 3L,
    ARIL = 2008L,
    BASE = 1L,
    VAUX_END = NA_integer_,
    LEDIT_1 = NA_integer_
  )

  vaux_metadata <- data.frame(
    V1 = "VAUX_END",
    V2 = "2",
    V3 = "BASE",
    V4 = "",
    V5 = "",
    V6 = "1",
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  logical_edit_metadata <- data.frame(
    V1 = "LEDIT_1",
    V2 = "1",
    V3 = "VAUX_END",
    V4 = "",
    V5 = "",
    V6 = "2",
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = "VAUX_END",
    frozen_values = list(),
    changed_variables = NULL,
    dependency_index = list(
      vaux = list(VAUX_END = "BASE"),
      logical_edits = list(LEDIT_1 = "VAUX_END")
    )
  )

  expect_identical(names(refreshed), names(record))
  expect_identical(refreshed$FASE, 80L)
  expect_identical(refreshed$VERSFASE, 0L)
  expect_identical(refreshed$SK3_, 3L)
  expect_identical(refreshed$ARIL, 2008L)
  expect_identical(refreshed$BASE, 1L)
  # changed_variables = NULL explicitly requests a complete refresh when this
  # low-level helper is called directly.  correct_record() no longer uses that
  # form at initialization; after a proposal it always supplies the variables
  # actually changed.
  expect_identical(refreshed$VAUX_END, 2L)
  expect_identical(refreshed$LEDIT_1, 1L)
})
