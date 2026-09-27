sspice_internal <- function(name) {
  getFromNamespace(name, "SSPICE")
}

testthat::test_that("the fast oracle uses the package dummy encoding", {
  oracle <- list(
    variable_names = c("ID", "A", "B"),
    number_items = c(1L, 3L, 1L)
  )
  record <- data.frame(ID = 1L, A = 2L, B = NA_integer_)
  observed <- sspice_internal(".sspice_oracle_dummy")(record, oracle)
  expected <- sspice_internal("dummize_vector")(
    as.list(record),
    oracle$number_items
  )
  testthat::expect_identical(observed, expected)
})

testthat::test_that("a reached missing string keeps routing flow continuous", {
  dag <- list(
    varnames = c("ID", "TEXT", "NEXT", "END"),
    routes_numeric = matrix(
      c(
        1,  1, 1, 1, 0, 0,
        2,  0, NA, 1, 0, 0,
        3,  1, 1, 1, 0, 0,
        4,  1, 1, 1, 0, 0
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    Bt = matrix(
      c(
         1, -1,  0,  0,
         0,  1, -1,  0,
         0,  0,  1, -1
      ),
      nrow = 3L,
      byrow = TRUE
    ),
    logical_edit_names = character()
  )
  oracle <- sspice_internal(".sspice_build_routing_oracle")(
    dag = dag,
    record_id = "ID"
  )
  record <- data.frame(
    ID = 1L,
    TEXT = NA_character_,
    NEXT = 1L,
    END = 1L,
    stringsAsFactors = FALSE
  )
  dummy <- sspice_internal(".sspice_oracle_dummy")(record, oracle)
  state <- sspice_internal(".sspice_fast_routing_state")(record, oracle)

  testthat::expect_identical(as.integer(dummy), rep(1L, 4L))
  testthat::expect_equal(as.numeric(oracle$B %*% dummy), c(1, 0, 0, 0))
  testthat::expect_identical(state$routing_violations, 0L)
  testthat::expect_false("TEXT" %in% state$full_report$varname_label)
  testthat::expect_true(is.na(record$TEXT))
})

testthat::test_that("a reached required string cannot remain missing", {
  dag <- list(
    varnames = c("ID", "TEXT", "NEXT", "END"),
    routes_numeric = matrix(
      c(
        1,  1, 1, 1, 0, 0,
        2,  0, 1, 1, 0, 0,
        3,  1, 1, 1, 0, 0,
        4,  1, 1, 1, 0, 0
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    Bt = matrix(
      c(
         1, -1,  0,  0,
         0,  1, -1,  0,
         0,  0,  1, -1
      ),
      nrow = 3L,
      byrow = TRUE
    ),
    logical_edit_names = character()
  )
  oracle <- sspice_internal(".sspice_build_routing_oracle")(
    dag = dag,
    record_id = "ID"
  )
  record <- data.frame(
    ID = 1L,
    TEXT = NA_character_,
    NEXT = 1L,
    END = 1L,
    stringsAsFactors = FALSE
  )
  dummy <- sspice_internal(".sspice_oracle_dummy")(record, oracle)
  state <- sspice_internal(".sspice_fast_routing_state")(record, oracle)

  testthat::expect_identical(as.integer(dummy), c(1L, 0L, 1L, 1L))
  testthat::expect_true("TEXT" %in% state$full_report$varname_label)
  text_rows <- state$full_report$varname_label == "TEXT"
  testthat::expect_true(any(state$full_report$errcode[text_rows] == -1L))
})

testthat::test_that("a skipped missing string stays inactive", {
  dag <- list(
    varnames = c("GATE", "TEXT", "NEXT", "END"),
    routes_numeric = matrix(
      c(
        1,  2, 1, 2, 0, 1, 1, 3,
        2,  0, NA, 1, 0, 0, NA, NA,
        3,  1, 1, 1, 0, 0, NA, NA,
        4,  1, 1, 1, 0, 0, NA, NA
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    Bt = matrix(
      c(
         1,  0, -1,  0,
         1, -1,  0,  0,
         0,  1, -1,  0,
         0,  0,  1, -1
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    logical_edit_names = character()
  )
  oracle <- sspice_internal(".sspice_build_routing_oracle")(
    dag = dag,
    record_id = "GATE"
  )
  record <- data.frame(
    GATE = 1L,
    TEXT = NA_character_,
    NEXT = 1L,
    END = 1L,
    stringsAsFactors = FALSE
  )
  dummy <- sspice_internal(".sspice_oracle_dummy")(record, oracle)
  state <- sspice_internal(".sspice_fast_routing_state")(record, oracle)

  testthat::expect_identical(as.integer(dummy), c(1L, 0L, 0L, 1L, 1L))
  testthat::expect_identical(state$routing_violations, 0L)
  testthat::expect_false("TEXT" %in% state$full_report$varname_label)
})

testthat::test_that("a filled skipped string remains a routing violation", {
  dag <- list(
    varnames = c("GATE", "TEXT", "NEXT", "END"),
    routes_numeric = matrix(
      c(
        1,  2, 1, 2, 0, 1, 1, 3,
        2,  0, NA, 1, 0, 0, NA, NA,
        3,  1, 1, 1, 0, 0, NA, NA,
        4,  1, 1, 1, 0, 0, NA, NA
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    Bt = matrix(
      c(
         1,  0, -1,  0,
         1, -1,  0,  0,
         0,  1, -1,  0,
         0,  0,  1, -1
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    logical_edit_names = character()
  )
  oracle <- sspice_internal(".sspice_build_routing_oracle")(
    dag = dag,
    record_id = "GATE"
  )
  record <- data.frame(
    GATE = 1L,
    TEXT = "annotation",
    NEXT = 1L,
    END = 1L,
    stringsAsFactors = FALSE
  )
  dummy <- sspice_internal(".sspice_oracle_dummy")(record, oracle)
  state <- sspice_internal(".sspice_fast_routing_state")(record, oracle)

  testthat::expect_identical(as.integer(dummy), c(1L, 0L, 1L, 1L, 1L))
  testthat::expect_true("TEXT" %in% state$full_report$varname_label)
  text_rows <- state$full_report$varname_label == "TEXT"
  testthat::expect_true(any(state$full_report$errcode[text_rows] == 1L))
})

testthat::test_that("derived dependencies are propagated transitively", {
  index <- list(
    vaux = list(VAUX_A = "A", VAUX_B = "VAUX_A"),
    logical_edits = list(LEDIT_1 = "VAUX_B", LEDIT_2 = "Z")
  )
  affected <- sspice_internal(".sspice_affected_derived_variables")(
    "A",
    index
  )
  testthat::expect_identical(affected$vaux, c("VAUX_A", "VAUX_B"))
  testthat::expect_identical(affected$logical_edits, "LEDIT_1")
})

testthat::test_that("a must-missing violation generates one blank move", {
  record <- data.frame(A = 2L, B = NA_integer_)
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = "A",
    must_be_present = character()
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1:3, B = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )
  testthat::expect_identical(proposal$changed_variables, "A")
  testthat::expect_identical(proposal$move_type, "blank")
  testthat::expect_true(is.na(proposal$record$A))
})

testthat::test_that("constraint stopping score excludes change distance", {
  state <- list(
    routing_violations = 0L,
    logical_edit_violations = 0L,
    vaux_violations = 0L,
    change_cost = 99,
    total_energy = 198
  )
  score <- sspice_internal(".sspice_constraint_score")(state)
  testthat::expect_identical(score, 0L)
})

testthat::test_that("donors can supply a node without an explicit domain", {
  record <- data.frame(
    FPROV = "A",
    X = NA_real_,
    stringsAsFactors = FALSE
  )
  donors <- data.frame(
    FPROV = rep("A", 6L),
    X = c(10, 10, 20, 10, 20, 10),
    stringsAsFactors = FALSE
  )
  context <- list(
    violated_nodes = "X",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "X"
  )
  dag <- list(
    varnames = names(record),
    Bt = matrix(0, nrow = 2L, ncol = 2L),
    routes_numeric = matrix(
      c(1, 1, 1, 1, 0, 0, 2, -1, 0, 100, 0, 0),
      nrow = 2L,
      byrow = TRUE
    )
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = dag,
    domains = list(FPROV = NULL, X = NULL),
    donor_data = donors,
    strata = "FPROV",
    imputation_method = "donor",
    priors = list(),
    fixed_variables = "FPROV",
    control = correction_control(minimum_donors = 5L),
    routing_oracle = NULL
  )
  testthat::expect_false(is.null(proposal))
  testthat::expect_identical(proposal$changed_variables, "X")
  testthat::expect_false(is.na(proposal$record$X))
})

testthat::test_that("routing columns and observed codes define every domain", {
  routes <- matrix(
    c(
      1, 3, 10, 20,
      2, 1, 1, 4,
      3, -1, 0, 0
    ),
    nrow = 3L,
    byrow = TRUE
  )
  data <- data.frame(
    A = c(10, 15),
    Q = c(1, 4),
    FPROV = c("001", "002"),
    stringsAsFactors = FALSE
  )
  donors <- data.frame(
    A = 20,
    Q = 2,
    FPROV = "003",
    stringsAsFactors = FALSE
  )
  domains <- sspice_internal(".sspice_domains_from_routes")(
    routes,
    names(data),
    data = data,
    donor_data = donors
  )
  testthat::expect_equal(domains$A, c(10, 15, 20))
  testthat::expect_equal(domains$Q, 1:4)
  testthat::expect_setequal(domains$FPROV, c("001", "002", "003"))
})

testthat::test_that("the current violated node falls back to its routing domain", {
  record <- data.frame(A = 1L, B = NA_integer_)
  donors <- data.frame(A = rep(1L, 6L), B = rep(1L, 6L))
  context <- list(
    violated_nodes = c("A", "B"),
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "B"
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1:2, B = 1:2),
    donor_data = donors,
    strata = character(),
    imputation_method = c(A = "donor", B = "uniform"),
    priors = list(),
    fixed_variables = character(),
    control = correction_control(allow_unconditional_donor = TRUE),
    routing_oracle = NULL
  )
  testthat::expect_false(is.null(proposal))
  testthat::expect_identical(proposal$changed_variables, "A")
  testthat::expect_identical(proposal$record$A, 2L)
  testthat::expect_identical(
    proposal$proposal_source,
    "routing_domain_fallback"
  )
  testthat::expect_true(is.na(proposal$record$B))
})

testthat::test_that("a singleton routing domain yields no admissible proposal", {
  record <- data.frame(A = 1L)
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "A"
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1L),
    donor_data = data.frame(A = 1L),
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )

  testthat::expect_null(proposal)
})

testthat::test_that("a singleton first node does not block a later violated node", {
  record <- data.frame(A = 1L, B = NA_integer_)
  context <- list(
    violated_nodes = c("A", "B"),
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = c("A", "B")
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1L, B = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )

  testthat::expect_false(is.null(proposal))
  testthat::expect_identical(proposal$changed_variables, "B")
  testthat::expect_false(is.na(proposal$record$B))
})

testthat::test_that("unviolated gates and neighbours are never fallback targets", {
  record <- data.frame(
    TARGET = 1L,
    FASE = 80L,
    SK3_ = 3L,
    ARIL = 2008L
  )
  context <- list(
    violated_nodes = "TARGET",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "TARGET",
    gates = "FASE",
    responsible_gates = character(),
    nodes = c("TARGET", "FASE", "SK3_", "ARIL"),
    routing_oracle = NULL
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(
      TARGET = 1L,
      FASE = c(80L, 90L),
      SK3_ = c(3L, 4L),
      ARIL = c(2008L, 2009L)
    ),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )

  testthat::expect_null(proposal)
  testthat::expect_identical(record$FASE, 80L)
  testthat::expect_identical(record$SK3_, 3L)
  testthat::expect_identical(record$ARIL, 2008L)
})

testthat::test_that("an unrestorable external NA does not block other violations", {
  record <- data.frame(VAUX_EXT = NA_integer_, B = NA_integer_)
  context <- list(
    violated_nodes = c("VAUX_EXT", "B"),
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = c("VAUX_EXT", "B")
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(VAUX_EXT = 1:2, B = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL,
    locked_values = list(VAUX_EXT = NA_integer_)
  )

  testthat::expect_false(is.null(proposal))
  testthat::expect_identical(proposal$changed_variables, "B")
  testthat::expect_true(is.na(proposal$record$VAUX_EXT))
  testthat::expect_false(is.na(proposal$record$B))
})

testthat::test_that("an immovable violated node does not alter its predecessor", {
  record <- data.frame(A = 1L, GATE = 1L)
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "A",
    gates = "GATE",
    responsible_gates = "GATE",
    nodes = "GATE"
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1L, GATE = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )

  testthat::expect_null(proposal)
  testthat::expect_identical(record$A, 1L)
  testthat::expect_identical(record$GATE, 1L)
})

testthat::test_that("a modifiable violated node is proposed before its responsible gate", {
  record <- data.frame(A = 1L, GATE = 1L)
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "A",
    gates = "GATE",
    responsible_gates = "GATE",
    nodes = c("A", "GATE")
  )

  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1:2, GATE = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL
  )

  testthat::expect_identical(proposal$changed_variables, "A")
  testthat::expect_identical(proposal$record$GATE, 1L)
})

testthat::test_that("empirical proposal probabilities are not energy tilted", {
  record <- data.frame(A = 1L, B = NA_integer_)
  donors <- data.frame(
    A = c(2L, 3L, 3L, 3L),
    B = NA_integer_
  )
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "A",
    routing_oracle = NULL
  )
  oracle <- list(
    incidence = matrix(
      c(
        0,  0,
        0, -1,
        0,  0,
        0,  1
      ),
      nrow = 4L,
      byrow = TRUE
    ),
    number_items = c(3L, 1L),
    variable_names = c("A", "B"),
    record_id = "A",
    record_id_index = 1L,
    logical_edit_names = character(),
    vaux_names = character(),
    item_mapping = NULL
  )
  set.seed(123L)
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(A = 1:3, B = 1L),
    donor_data = donors,
    strata = character(),
    imputation_method = c(A = "donor", B = "uniform"),
    priors = list(),
    fixed_variables = character(),
    control = correction_control(
      minimum_donors = 1L,
      allow_unconditional_donor = TRUE
    ),
    routing_oracle = oracle
  )
  testthat::expect_true(
    isTRUE(all.equal(proposal$probability, 0.25)) ||
      isTRUE(all.equal(proposal$probability, 0.75))
  )
  testthat::expect_identical(
    proposal$proposal_source,
    "marginal_donor"
  )
})

testthat::test_that("a fixed violated node does not alter its predecessor", {
  record <- data.frame(GATE = 1L, A = 1L)
  context <- list(
    violated_nodes = "A",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "A",
    gates = "GATE",
    responsible_gates = "GATE",
    nodes = c("GATE", "A"),
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(GATE = 1:2, A = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "A",
    control = correction_control(),
    routing_oracle = NULL
  )
  testthat::expect_null(proposal)
  testthat::expect_identical(record$GATE, 1L)
  testthat::expect_identical(record$A, 1L)
})

testthat::test_that("non-recomputable endogenous VAUX are frozen", {
  index <- list(
    vaux = list(VAUX4 = c("FREPRI", "FRESEC", "FRETER")),
    logical_edits = list()
  )
  observed <- sspice_internal(".sspice_classify_vaux")(
    dependency_index = index,
    data_names = c("VOR15", "VAUX4"),
    endogenous_vaux = "VAUX4",
    external_vaux = character()
  )
  testthat::expect_length(observed$recomputable, 0L)
  testthat::expect_identical(observed$frozen_by_canonicalization, "VAUX4")
  testthat::expect_identical(observed$frozen, "VAUX4")
  testthat::expect_identical(
    observed$missing_inputs$VAUX4,
    c("FREPRI", "FRESEC", "FRETER")
  )
})

testthat::test_that("recomputable and external VAUX remain distinct", {
  index <- list(
    vaux = list(VAUX_SUM = c("QPROP1", "QPROP2")),
    logical_edits = list()
  )
  observed <- sspice_internal(".sspice_classify_vaux")(
    dependency_index = index,
    data_names = c("QPROP1", "QPROP2", "VAUX_SUM", "VAUX_EXT"),
    endogenous_vaux = "VAUX_SUM",
    external_vaux = "VAUX_EXT"
  )
  testthat::expect_identical(observed$recomputable, "VAUX_SUM")
  testthat::expect_identical(observed$external, "VAUX_EXT")
  testthat::expect_identical(observed$frozen, "VAUX_EXT")
})

testthat::test_that("logical OR VAUX are mode-locked and not recomputed", {
  index <- list(
    vaux = list(VOR1 = c("A", "B")),
    logical_edits = list()
  )
  observed <- sspice_internal(".sspice_classify_vaux")(
    dependency_index = index,
    data_names = c("A", "B", "VOR1"),
    endogenous_vaux = "VOR1",
    external_vaux = character(),
    logical_group_vaux = "VOR1"
  )
  testthat::expect_length(observed$recomputable, 0L)
  testthat::expect_identical(observed$logical_groups, "VOR1")
  testthat::expect_identical(observed$frozen, "VOR1")
})

testthat::test_that("routing metadata distinguish quantitative nodes", {
  dag <- list(
    varnames = c("QPROP1", "STATUS"),
    routes_numeric = matrix(
      c(
        1, 1, 0, 100,
        2, 2, 1, 2
      ),
      nrow = 2L,
      byrow = TRUE
    )
  )
  record <- data.frame(
    QPROP1 = "25",
    STATUS = 1L,
    stringsAsFactors = FALSE
  )
  testthat::expect_true(
    sspice_internal(".sspice_is_quantitative_node")(
      "QPROP1", record, dag
    )
  )
  testthat::expect_false(
    sspice_internal(".sspice_is_quantitative_node")(
      "STATUS", record, dag
    )
  )
})

testthat::test_that("arithmetic VAUX constraints restrict numeric values", {
  metadata <- data.frame(
    V1 = rep("VAUX_SUM", 4L),
    V2 = c(1, 1, 2, 2),
    V3 = c("QPROP1", "QPROP2", "QPROP1", "QPROP2"),
    V4 = NA_character_,
    V5 = NA_character_,
    V6 = NA_character_,
    V7 = NA_character_,
    V8 = NA_character_,
    V9 = c("+", "<100", "+", "=100"),
    stringsAsFactors = FALSE
  )
  record <- data.frame(QPROP1 = 30, QPROP2 = 80, VAUX_SUM = NA_real_)
  constraints <- sspice_internal(".sspice_arithmetic_constraints")(
    "QPROP2", record, metadata
  )
  testthat::expect_true(
    sspice_internal(".sspice_numeric_value_satisfies")(60, constraints)
  )
  testthat::expect_true(
    sspice_internal(".sspice_numeric_value_satisfies")(70, constraints)
  )
  testthat::expect_false(
    sspice_internal(".sspice_numeric_value_satisfies")(80, constraints)
  )
})

testthat::test_that("quantitative proposals respect an arithmetic VAUX", {
  metadata <- data.frame(
    V1 = rep("VAUX_SUM", 4L),
    V2 = c(1, 1, 2, 2),
    V3 = c("QPROP1", "QPROP2", "QPROP1", "QPROP2"),
    V4 = NA_character_,
    V5 = NA_character_,
    V6 = NA_character_,
    V7 = NA_character_,
    V8 = NA_character_,
    V9 = c("+", "<100", "+", "=100"),
    stringsAsFactors = FALSE
  )
  record <- data.frame(QPROP1 = 30, QPROP2 = 80)
  donors <- data.frame(
    QPROP1 = c(30, 30, 30),
    QPROP2 = c(60, 70, 80)
  )
  dag <- list(
    varnames = names(record),
    routes_numeric = matrix(
      c(
        1, 1, 0, 100,
        2, 1, 0, 100
      ),
      nrow = 2L,
      byrow = TRUE
    )
  )
  context <- list(
    violated_nodes = "QPROP2",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "QPROP2",
    gates = character(),
    nodes = "QPROP2",
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = dag,
    domains = list(QPROP1 = 0:100, QPROP2 = 0:100),
    donor_data = donors,
    strata = character(),
    imputation_method = "donor",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(
      minimum_donors = 1L,
      allow_unconditional_donor = TRUE
    ),
    routing_oracle = NULL,
    vaux_metadata = metadata
  )
  testthat::expect_true(proposal$record$QPROP2 %in% c(60, 70))
  testthat::expect_identical(
    proposal$proposal_source,
    "conditional_numeric_donor"
  )
})

testthat::test_that("a frozen VAUX is never blanked, imputed, or bypassed through its predecessor", {
  record <- data.frame(GATE = 1L, VAUX_EXT = 2L)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = character(),
    must_be_missing = "VAUX_EXT",
    must_be_present = character(),
    gates = "GATE",
    responsible_gates = "GATE",
    nodes = c("GATE", "VAUX_EXT"),
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(GATE = 1:2, VAUX_EXT = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "VAUX_EXT",
    control = correction_control(),
    locked_values = list()
  )
  testthat::expect_null(proposal)
  testthat::expect_identical(record$GATE, 1L)
  testthat::expect_identical(record$VAUX_EXT, 2L)
})

testthat::test_that("an ordinary must-missing violation blanks the violated variable", {
  record <- data.frame(
    ALTRO = 2L,
    XPRODUZ = 1L
  )
  context <- list(
    violated_nodes = "XPRODUZ",
    logical_edit_inputs = character(),
    must_be_missing = "XPRODUZ",
    must_be_present = character(),
    gates = "ALTRO",
    responsible_gates = "ALTRO",
    inferred_responsible_gates = "ALTRO",
    nodes = c("ALTRO", "XPRODUZ"),
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(ALTRO = 1:2, XPRODUZ = 0:1),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    locked_values = list()
  )
  testthat::expect_identical(proposal$changed_variables, "XPRODUZ")
  testthat::expect_true(is.na(proposal$record$XPRODUZ))
  testthat::expect_identical(proposal$record$ALTRO, 2L)
})

testthat::test_that("a rejected donor mode falls back to another mode of the same variable", {
  testthat::local_mocked_bindings(
    .sspice_candidate_node_modes = function(...) {
      list(
        values = 2L,
        probabilities = 1,
        source = "conditional_donor",
        predecessors_used = "DEBCAR",
        strata_used = "FPROV",
        donor_count = 10L,
        quantitative = FALSE
      )
    },
    .sspice_mode_jump_profile = function(
        variable, value, record, dag, fixed_variables = character()) {
      if (identical(as.integer(value), 2L)) {
        return(list(
          jumps = TRUE,
          filled_share = 1,
          immutable_filled = character(),
          filled_variables = "AFTER"
        ))
      }
      list(
        jumps = FALSE,
        filled_share = 0,
        immutable_filled = character(),
        filled_variables = character()
      )
    },
    .package = "SSPICE"
  )
  record <- data.frame(
    DEBCAR = 1L,
    PAGALDEB = NA_integer_,
    AFTER = 1L
  )
  context <- list(
    violated_nodes = "PAGALDEB",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "PAGALDEB",
    gates = "DEBCAR",
    responsible_gates = "DEBCAR",
    inferred_responsible_gates = "DEBCAR",
    nodes = c("DEBCAR", "PAGALDEB"),
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(DEBCAR = 1:3, PAGALDEB = 1:2, AFTER = 1:2),
    donor_data = data.frame(DEBCAR = 1L, PAGALDEB = 2L, AFTER = 1L),
    strata = character(),
    imputation_method = "donor",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    locked_values = list()
  )
  testthat::expect_identical(proposal$changed_variables, "PAGALDEB")
  testthat::expect_identical(proposal$record$PAGALDEB, 1L)
  testthat::expect_identical(proposal$record$DEBCAR, 1L)
  testthat::expect_identical(
    proposal$proposal_source,
    "routing_domain_fallback"
  )
})

testthat::test_that("an isolated external VAUX yields no correction move", {
  record <- data.frame(VAUX_EXT = 2L)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = character(),
    must_be_missing = "VAUX_EXT",
    must_be_present = character(),
    gates = character(),
    nodes = "VAUX_EXT",
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(VAUX_EXT = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "VAUX_EXT",
    control = correction_control(),
    locked_values = list()
  )
  testthat::expect_null(proposal)
  testthat::expect_identical(record$VAUX_EXT, 2L)
})

testthat::test_that("a mode-locked logical VAUX may become missing", {
  record <- data.frame(VAUX_GROUP = 2L)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = character(),
    must_be_missing = "VAUX_GROUP",
    must_be_present = character(),
    gates = character(),
    nodes = "VAUX_GROUP",
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(VAUX_GROUP = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "VAUX_GROUP",
    control = correction_control(),
    locked_values = list(VAUX_GROUP = 2L)
  )
  testthat::expect_identical(proposal$changed_variables, "VAUX_GROUP")
  testthat::expect_true(is.na(proposal$record$VAUX_GROUP))
  testthat::expect_identical(proposal$move_type, "blank")
})

testthat::test_that("a mode-locked logical VAUX restores only its original mode", {
  record <- data.frame(VAUX_GROUP = NA_integer_)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "VAUX_GROUP",
    gates = character(),
    nodes = "VAUX_GROUP",
    routing_oracle = NULL
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(VAUX_GROUP = 1:3),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "VAUX_GROUP",
    control = correction_control(),
    locked_values = list(VAUX_GROUP = 2L)
  )
  testthat::expect_identical(proposal$changed_variables, "VAUX_GROUP")
  testthat::expect_identical(proposal$record$VAUX_GROUP, 2L)
  testthat::expect_identical(
    proposal$proposal_source,
    "mode_locked_vaux_original_mode"
  )
})

testthat::test_that("an endogenous VAUX is proposed from its own domain first", {
  record <- data.frame(BASE = 1L, VAUX_END = 2L)
  context <- list(
    violated_nodes = "VAUX_END",
    logical_edit_inputs = "BASE",
    must_be_missing = character(),
    must_be_present = character(),
    gates = character(),
    nodes = c("BASE", "VAUX_END"),
    routing_oracle = NULL
  )
  metadata <- data.frame(
    V1 = "VAUX_END",
    V2 = "1",
    V3 = "BASE",
    V4 = "",
    V5 = "",
    V6 = "",
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(BASE = 1:2, VAUX_END = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    vaux_metadata = metadata,
    locked_values = list()
  )
  testthat::expect_identical(proposal$changed_variables, "VAUX_END")
  testthat::expect_identical(proposal$record$BASE, 1L)
  testthat::expect_true(proposal$record$VAUX_END %in% 1:2)
})

testthat::test_that("an inactive endogenous VAUX may be blanked directly", {
  record <- data.frame(BASE = 1L, VAUX_END = 2L)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = "BASE",
    must_be_missing = "VAUX_END",
    must_be_present = character(),
    gates = character(),
    nodes = "BASE",
    routing_oracle = NULL
  )
  metadata <- data.frame(
    V1 = "VAUX_END", V2 = "1", V3 = "BASE",
    V4 = "", V5 = "", V6 = "1", V7 = "", V8 = "", V9 = "NULL",
    stringsAsFactors = FALSE
  )
  proposal <- sspice_internal(".sspice_generate_one_correction_proposal")(
    record = record,
    context = context,
    dag = list(varnames = names(record)),
    domains = list(BASE = 1:2, VAUX_END = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    vaux_metadata = metadata,
    locked_values = list()
  )
  testthat::expect_identical(proposal$changed_variables, "VAUX_END")
  testthat::expect_identical(proposal$move_type, "blank")
  testthat::expect_true(is.na(proposal$record$VAUX_END))
  testthat::expect_identical(record$BASE, 1L)
})

testthat::test_that("an optional filled LEDIT input proposes a blank", {
  record <- data.frame(PROPR1 = 1L, PROPR2 = 2L)
  context <- list(
    violated_nodes = character(),
    logical_edit_inputs = "PROPR2",
    must_be_missing = character(),
    must_be_present = character(),
    gates = character(),
    nodes = "PROPR2",
    routing_oracle = NULL
  )
  dag <- list(
    varnames = names(record),
    routes_numeric = matrix(
      c(
        1, 1, 1, 20,
        2, 1, NA, 20
      ),
      nrow = 2L,
      byrow = TRUE
    )
  )
  proposal <- sspice_internal(
    ".sspice_generate_one_correction_proposal"
  )(
    record = record,
    context = context,
    dag = dag,
    domains = list(PROPR1 = 1:20, PROPR2 = 2L),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    routing_oracle = NULL,
    vaux_metadata = NULL,
    locked_values = list()
  )
  testthat::expect_identical(proposal$changed_variables, "PROPR2")
  testthat::expect_identical(proposal$move_type, "logical_blank")
  testthat::expect_true(is.na(proposal$record$PROPR2))
})

testthat::test_that("arithmetic LEDIT accepts numeric character and factor operands", {
  metadata <- data.frame(
    V1 = c("LEDIT_SUM", "LEDIT_SUM", "LEDIT_SUM", "LEDIT_SUM"),
    V2 = c("1", "1", "2", "2"),
    V3 = c("Q1", "Q2", "Q1", "Q2"),
    V4 = NA_character_,
    V5 = NA_character_,
    V6 = c(NA, NA, "blank", "blank"),
    V7 = NA_character_,
    V8 = NA_character_,
    V9 = c("+", "=100 NULL", "AND", "NULL"),
    stringsAsFactors = FALSE
  )
  cases <- list(
    numeric = data.frame(Q1 = c(40, NA), Q2 = c(60, NA)),
    character = data.frame(
      Q1 = c("40", NA_character_),
      Q2 = c("60", NA_character_),
      stringsAsFactors = FALSE
    ),
    factor = data.frame(
      Q1 = factor(c("40", NA_character_)),
      Q2 = factor(c("60", NA_character_))
    )
  )
  for (input in cases) {
    result <- SSPICE:::create_logical_edit_variables(
      inpdata = input,
      logical_edits_data = metadata
    )
    testthat::expect_equal(result$LEDIT_SUM, c(1, 2))
    testthat::expect_identical(result$Q1, input$Q1)
    testthat::expect_identical(result$Q2, input$Q2)
  }
})

testthat::test_that("generic LEDIT repair searches domains and protects other edits", {
  metadata <- data.frame(
    V1 = c("LEDIT_TARGET", "LEDIT_PROTECTED"),
    V2 = c("1", "1"),
    V3 = c("Q1", "Q2"),
    V4 = "", V5 = "", V6 = "", V7 = "", V8 = "", V9 = "NULL",
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    create_logical_edit_variables = function(inpdata, logical_edits_data) {
      result <- inpdata
      total <- rowSums(cbind(result$Q1, result$Q2), na.rm = TRUE)
      result$LEDIT_TARGET <- ifelse(total == 100, 1L, NA_integer_)
      result$LEDIT_PROTECTED <- ifelse(
        is.na(result$Q2), 1L, NA_integer_
      )
      result
    },
    .sspice_fast_routing_state = function(
        record, oracle, build_report = FALSE) {
      list(
        routing_violations = as.integer(!is.na(record$Q2[1L])),
        logical_edit_violations = 0L,
        vaux_violations = 0L
      )
    },
    .package = "SSPICE"
  )
  record <- data.frame(
    Q1 = 75L,
    Q2 = NA_integer_,
    LEDIT_TARGET = NA_integer_,
    LEDIT_PROTECTED = 1L
  )
  dag <- list(
    varnames = names(record),
    routes_numeric = matrix(
      c(
        1, 1, NA, 100,
        2, 1, NA, 100,
        3, 2, 1, 2,
        4, 2, 1, 2
      ),
      nrow = 4L,
      byrow = TRUE
    )
  )
  context <- list(
    logical_edit_inputs = c("Q1", "Q2"),
    routing_oracle = list(mock_oracle = TRUE)
  )
  proposal <- SSPICE:::.sspice_find_logical_edit_repair(
    record = record,
    edit_name = "LEDIT_TARGET",
    logical_edit_metadata = metadata,
    context = context,
    dag = dag,
    domains = list(Q1 = 1:100, Q2 = 1:100),
    fixed_variables = c("LEDIT_TARGET", "LEDIT_PROTECTED"),
    control = correction_control()
  )
  testthat::expect_identical(proposal$changed_variables, "Q1")
  testthat::expect_identical(proposal$record$Q1, 100L)
  testthat::expect_identical(proposal$record$LEDIT_TARGET, 1L)
  testthat::expect_identical(proposal$record$LEDIT_PROTECTED, 1L)
})

testthat::test_that("a reduction of H_violations is always accepted", {
  current_state <- list(
    routing_violations = 3L,
    logical_edit_violations = 1L,
    vaux_violations = 0L,
    total_energy = 10
  )
  candidate_state <- list(
    routing_violations = 1L,
    logical_edit_violations = 0L,
    vaux_violations = 0L,
    total_energy = 100
  )
  accepted <- sspice_internal(
    ".sspice_accept_correction_proposal"
  )(
    current_state = current_state,
    candidate_state = candidate_state,
    transition_energy = 100,
    temperature = 1e-4
  )
  testthat::expect_true(accepted)
})

testthat::test_that("an excluded donor mode falls back to the same node domain", {
  record <- data.frame(NODE = NA_integer_)
  context <- list(
    violated_nodes = "NODE",
    logical_edit_inputs = character(),
    must_be_missing = character(),
    must_be_present = "NODE",
    vaux_nodes = character(),
    vaux_consistency_nodes = character(),
    routing_oracle = NULL
  )
  dag <- list(
    varnames = "NODE",
    routes_numeric = matrix(c(1, 2, 1, 2, 0, 0), nrow = 1L)
  )
  testthat::local_mocked_bindings(
    .sspice_candidate_node_modes = function(...) {
      list(
        values = "2",
        probabilities = 1,
        source = "conditional_donor",
        predecessors_used = "",
        strata_used = "",
        donor_count = 10L,
        quantitative = FALSE
      )
    },
    .package = "SSPICE"
  )
  proposal <- SSPICE:::.sspice_generate_one_correction_proposal(
    record = record,
    context = context,
    dag = dag,
    domains = list(NODE = 1:2),
    donor_data = NULL,
    donor_cache = NULL,
    strata = character(),
    imputation_method = "donor",
    priors = list(),
    fixed_variables = character(),
    control = correction_control(),
    excluded_modes = list(NODE = "2")
  )
  testthat::expect_identical(proposal$changed_variables, "NODE")
  testthat::expect_identical(proposal$record$NODE, 1L)
  testthat::expect_identical(
    proposal$proposal_source,
    "routing_domain_fallback"
  )
})

testthat::test_that("a costly jump from a refreshed VAUX rejects the driver mode", {
  before <- data.frame(BASE = 2L, VAUX = NA_integer_, SKIPPED = 1L)
  refreshed <- before
  refreshed$VAUX <- 2L
  testthat::local_mocked_bindings(
    .sspice_affected_derived_variables = function(...) {
      list(vaux = "VAUX", logical_edits = character())
    },
    .sspice_mode_jump_profile = function(...) {
      list(
        jumps = TRUE,
        filled_share = 1,
        immutable_filled = character(),
        filled_variables = "SKIPPED"
      )
    },
    .package = "SSPICE"
  )
  result <- SSPICE:::.sspice_propagate_refreshed_vaux_routing(
    before_refresh = before,
    refreshed = refreshed,
    changed_variables = "BASE",
    dag = list(varnames = names(before)),
    fixed_variables = character(),
    control = correction_control(branch_fill_threshold = 0.5),
    vaux_metadata = data.frame(),
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    routing_variable_names = names(before),
    endogenous_vaux = "VAUX",
    frozen_values = list(),
    mode_locked_values = list(),
    dependency_index = list(vaux = list(VAUX = "BASE")),
    suppress_frozen = character()
  )
  testthat::expect_false(result$usable)
  testthat::expect_identical(result$rejected_vaux, "VAUX")
  testthat::expect_identical(result$record$VAUX, 2L)
})

testthat::test_that("a local refresh leaves unrelated VAUX untouched", {
  recomputed <- character()
  metadata <- data.frame(
    V1 = c("VAUX_A", "VAUX_UNRELATED"),
    V2 = c(1L, 1L),
    V3 = c("BASE_A", "BASE_B"),
    stringsAsFactors = FALSE
  )
  dependency_index <- list(
    vaux = list(
      VAUX_A = "BASE_A",
      VAUX_UNRELATED = "BASE_B"
    ),
    logical_edits = list()
  )
  record <- data.frame(
    BASE_A = 2L,
    BASE_B = 1L,
    VAUX_A = 1L,
    VAUX_UNRELATED = 9L
  )

  testthat::local_mocked_bindings(
    create_vaux_variables = function(
        inpdata, vauxs_data, extDFpath, routing_vnames) {
      variable <- as.character(vauxs_data[[1L]][1L])
      recomputed <<- c(recomputed, variable)
      inpdata[[variable]] <- if (identical(variable, "VAUX_A")) 2L else 7L
      inpdata
    },
    .package = "SSPICE"
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = metadata,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = c("VAUX_A", "VAUX_UNRELATED"),
    frozen_values = list(),
    changed_variables = "BASE_A",
    dependency_index = dependency_index,
    suppress_frozen = character()
  )

  testthat::expect_identical(recomputed, "VAUX_A")
  testthat::expect_identical(refreshed$VAUX_A, 2L)
  testthat::expect_identical(refreshed$VAUX_UNRELATED, 9L)
})
