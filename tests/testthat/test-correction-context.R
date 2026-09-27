make_context_test_dag <- function() {
  routes <- matrix(
    c(
      1, 2, 1, 2, 0, 1, 2, 4,
      2, 1, 1, 1, 0, 0, NA, NA,
      3, 1, 1, 1, 0, 0, NA, NA,
      4, 1, 1, 1, 0, 0, NA, NA,
      5, 2, 1, 2, 0, 0, NA, NA
    ),
    nrow = 5L,
    byrow = TRUE
  )

  colnames(routes) <- paste0(
    "V",
    seq_len(ncol(routes))
  )

  Bt <- matrix(
    c(
       1, -1,  0,  0, 0,
       1,  0,  0, -1, 0,
       0,  1, -1,  0, 0,
       0,  0,  1, -1, 0,
       0,  0,  0,  1, -1
    ),
    nrow = 5L,
    byrow = TRUE
  )

  colnames(Bt) <- c(
    "GATE",
    "A",
    "B",
    "TARGET",
    "LEDIT_1"
  )

  list(
    routes_numeric = routes,
    Bt = Bt,
    varnames = c(
      "GATE",
      "A",
      "B",
      "TARGET",
      "LEDIT_1"
    ),
    logical_edit_names = "LEDIT_1"
  )
}

test_that("a derived VAUX is represented by its base inputs", {
  record <- data.frame(
    BASE = 1L,
    VAUX_END = 2L
  )
  dag <- list(
    varnames = names(record),
    routes_numeric = matrix(
      c(
        1, 2, 1, 2,
        2, 2, 1, 2
      ),
      nrow = 2L,
      byrow = TRUE
    ),
    Bt = matrix(0, nrow = 2L, ncol = 2L),
    logical_edit_names = character()
  )
  state <- list(
    routing_check = list(
      full_report = data.frame(
        RECORD_ID = 1L,
        varname_label = "VAUX_END",
        routevar_label = NA_character_,
        violation_source = "ROUTING",
        stringsAsFactors = FALSE
      ),
      activation = stats::setNames(c(0, -1), names(record))
    )
  )
  dependency_index <- list(
    vaux = list(VAUX_END = "BASE"),
    logical_edits = list()
  )
  context <- SSPICE:::.sspice_violation_context(
    state = state,
    record = record,
    dag = dag,
    record_id = "RECORD_ID",
    fixed_variables = character(),
    dependency_index = dependency_index
  )
  expect_false("VAUX_END" %in% context$nodes)
  expect_false("VAUX_END" %in% context$violated_nodes)
  expect_identical(context$vaux_inputs, "BASE")
  expect_true("BASE" %in% context$nodes)
})

make_context_test_record <- function(
    gate = 2,
    a = 1,
    b = 1,
    target = 1,
    logical_edit = 1) {
  data.frame(
    GATE = gate,
    A = a,
    B = b,
    TARGET = target,
    LEDIT_1 = logical_edit,
    stringsAsFactors = FALSE
  )
}

make_context_test_state <- function(
    variable,
    route_variable = NA_character_,
    source = "ROUTING") {
  report <- data.frame(
    RECORD_ID = 1,
    varname_label = variable,
    routevar_label = route_variable,
    violation_source = source,
    stringsAsFactors = FALSE
  )

  list(
    routing_check = list(
      full_report = report
    )
  )
}

make_context_edit_metadata <- function() {
  data.frame(
    V1 = c(
      "LEDIT_1",
      "LEDIT_1"
    ),
    V2 = c(
      "1",
      "1"
    ),
    V3 = c(
      "A",
      "B"
    ),
    V4 = c(
      "",
      ""
    ),
    V5 = c(
      "",
      ""
    ),
    V6 = c(
      "",
      ""
    ),
    V7 = c(
      "",
      ""
    ),
    V8 = c(
      "",
      ""
    ),
    V9 = c(
      "AND",
      "NULL"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("Bt rows retain their structural predecessor and item index", {
  dag <- make_context_test_dag()

  metadata <- SSPICE:::.sspice_bt_row_metadata(
    Bt = dag$Bt,
    routes = dag$routes_numeric,
    variable_names = dag$varnames
  )

  expect_equal(
    nrow(metadata),
    nrow(dag$Bt)
  )

  expect_equal(
    metadata$predecessor[1:2],
    c(
      "GATE",
      "GATE"
    )
  )

  expect_equal(
    metadata$item_index[1:2],
    c(
      1L,
      2L
    )
  )

  expect_equal(
    metadata$predecessor[3:5],
    c(
      "A",
      "B",
      "TARGET"
    )
  )
})

test_that("gate targets handle missing or non-scalar candidate values", {
  dag <- make_context_test_dag()

  expect_identical(
    SSPICE:::.sspice_gate_target(
      gate = 1L,
      value = NA_real_,
      routes = dag$routes_numeric
    ),
    2L
  )

  expect_identical(
    SSPICE:::.sspice_gate_target(
      gate = 1L,
      value = numeric(),
      routes = dag$routes_numeric
    ),
    2L
  )

  expect_identical(
    SSPICE:::.sspice_gate_target(
      gate = 1L,
      value = c(1, 2),
      routes = dag$routes_numeric
    ),
    2L
  )

  expect_identical(
    SSPICE:::.sspice_gate_target(
      gate = 1L,
      value = 2,
      routes = dag$routes_numeric
    ),
    4L
  )
})

test_that("violation context accepts a localized report row", {
  expect_true(
    "violation_row" %in%
      names(formals(SSPICE:::.sspice_violation_context))
  )

  dag <- make_context_test_dag()
  record <- make_context_test_record()
  state <- list(
    routing_check = list(
      full_report = rbind(
        data.frame(
          RECORD_ID = 1L,
          varname_label = "A",
          routevar_label = "GATE",
          violation_source = "ROUTING",
          stringsAsFactors = FALSE
        ),
        data.frame(
          RECORD_ID = 1L,
          varname_label = "B",
          routevar_label = "A",
          violation_source = "ROUTING",
          stringsAsFactors = FALSE
        )
      )
    )
  )

  expect_no_error(
    SSPICE:::.sspice_violation_context(
      state = state,
      record = record,
      dag = dag,
      record_id = "RECORD_ID",
      fixed_variables = character(),
      violation_row = 2L
    )
  )
})

test_that("routing-context incoming arcs are determined by Bt", {
  dag <- make_context_test_dag()

  ordinary_record <- make_context_test_record(
    gate = 1
  )

  skipped_record <- make_context_test_record(
    gate = 2
  )

  expect_identical(
    SSPICE:::.sspice_structural_predecessors(
      variable = "A",
      record = ordinary_record,
      dag = dag,
      active_only = TRUE
    ),
    "GATE"
  )

  expect_length(
    SSPICE:::.sspice_structural_predecessors(
      variable = "A",
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
      active_only = TRUE
    ),
    c(
      "GATE",
      "B"
    )
  )
})

test_that("routing-context modes stored as character are scaled correctly", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    gate = "2"
  )

  expect_identical(
    SSPICE:::.sspice_structural_predecessors(
      variable = "TARGET",
      record = record,
      dag = dag,
      active_only = TRUE
    ),
    c(
      "GATE",
      "B"
    )
  )

  expect_length(
    SSPICE:::.sspice_structural_predecessors(
      variable = "A",
      record = record,
      dag = dag,
      active_only = TRUE
    ),
    0L
  )
})

test_that("the responsible gate is recovered for a skipped node", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    gate = 2
  )

  gate_index <- SSPICE:::.sspice_find_responsible_gate(
    node = match(
      "A",
      dag$varnames
    ),
    record = record,
    dag = dag
  )

  expect_identical(
    gate_index,
    1L
  )

  expect_identical(
    dag$varnames[gate_index],
    "GATE"
  )
})

test_that("the complete skipped segment is identified", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    gate = 2
  )

  segment <- SSPICE:::.sspice_jump_segment(
    gate = match(
      "GATE",
      dag$varnames
    ),
    record = record,
    dag = dag
  )

  expect_identical(
    segment,
    c(
      2L,
      3L
    )
  )

  expect_identical(
    dag$varnames[segment],
    c(
      "A",
      "B"
    )
  )
})

test_that("routing violations produce a local correction context", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    gate = 2
  )

  context <- SSPICE:::.sspice_violation_context(
    state = make_context_test_state(
      variable = "A",
      route_variable = "GATE",
      source = "ROUTING"
    ),
    record = record,
    dag = dag,
    record_id = "RECORD_ID",
    fixed_variables = character(),
    logical_edit_metadata = make_context_edit_metadata()
  )

  expect_setequal(
    context$nodes,
    c(
      "GATE",
      "A",
      "B"
    )
  )

  expect_identical(
    context$gates,
    "GATE"
  )

  expect_identical(
    context$responsible_gates,
    "GATE"
  )

  expect_identical(
    context$segment,
    c(
      "A",
      "B"
    )
  )

  expect_identical(
    context$violated_nodes,
    "A"
  )
})

test_that("logical-edit violations recover their declared input variables", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    logical_edit = NA_real_
  )

  context <- SSPICE:::.sspice_violation_context(
    state = make_context_test_state(
      variable = "LEDIT_1",
      source = "LOGICAL_EDIT"
    ),
    record = record,
    dag = dag,
    record_id = "RECORD_ID",
    fixed_variables = character(),
    logical_edit_metadata = make_context_edit_metadata()
  )

  expect_setequal(
    context$logical_edit_inputs,
    c(
      "A",
      "B"
    )
  )

  expect_setequal(
    context$nodes,
    c(
      "A",
      "B"
    )
  )

  expect_false(
    "LEDIT_1" %in% context$nodes
  )
})

test_that("fixed gates are excluded from the correction neighbourhood", {
  dag <- make_context_test_dag()
  record <- make_context_test_record(
    gate = 2
  )

  context <- SSPICE:::.sspice_violation_context(
    state = make_context_test_state(
      variable = "A",
      route_variable = "GATE"
    ),
    record = record,
    dag = dag,
    record_id = "RECORD_ID",
    fixed_variables = "GATE",
    logical_edit_metadata = make_context_edit_metadata()
  )

  expect_false(
    "GATE" %in% context$nodes
  )

  expect_length(
    context$gates,
    0L
  )

  expect_length(
    context$responsible_gates,
    0L
  )

  expect_setequal(
    context$nodes,
    c(
      "A",
      "B"
    )
  )
})

test_that("an empty routing report produces an empty context", {
  dag <- make_context_test_dag()

  state <- list(
    routing_check = list(
      full_report = data.frame()
    )
  )

  context <- SSPICE:::.sspice_violation_context(
    state = state,
    record = make_context_test_record(),
    dag = dag,
    record_id = "RECORD_ID",
    fixed_variables = character(),
    logical_edit_metadata = make_context_edit_metadata()
  )

  expect_length(
    context$nodes,
    0L
  )

  expect_length(
    context$gates,
    0L
  )

  expect_length(
    context$responsible_gates,
    0L
  )

  expect_length(
    context$segment,
    0L
  )
})
