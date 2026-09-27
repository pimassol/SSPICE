make_record_test_state <- function(
    routing = 0L,
    logical = 0L,
    vaux = 0L,
    change = 0,
    energy = routing + logical + vaux + change) {
  list(
    routing_violations = as.integer(routing),
    logical_edit_violations = as.integer(logical),
    vaux_violations = as.integer(vaux),
    change_cost = change,
    total_energy = energy,
    routing_check = list(
      full_report = data.frame()
    )
  )
}

make_record_test_dag <- function() {
  list(
    routes_numeric = matrix(
      c(
        1, 2, 1, 2,
        2, 3, 1, 3
      ),
      nrow = 2L,
      byrow = TRUE
    ),
    Bt = matrix(
      c(
        1, -1,
        1, -1
      ),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(NULL, c("ID", "X"))
    ),
    varnames = c("ID", "X"),
    logical_edit_names = character()
  )
}

test_that("constraint score counts every type of violation", {
  state <- make_record_test_state(
    routing = 2L,
    logical = 3L,
    vaux = 4L
  )

  expect_equal(
    SSPICE:::.sspice_constraint_score(state),
    9L
  )
})

test_that("automatic SA temperature covers the elementary change scale", {
  control <- correction_control(
    lambda_change = 2,
    penalty_fill = 1,
    penalty_blank = 2,
    penalty_flip = 3
  )
  scale <- control$lambda_change * max(
    control$penalty_fill,
    control$penalty_blank,
    control$penalty_flip
  )

  expect_equal(scale, 6)
  expect_gt(
    SSPICE:::.sspice_sa_temperature(
      iteration = 1L,
      max_iterations = 100L,
      initial_temperature = scale,
      final_temperature = control$final_temperature,
      cooling_fraction = control$cooling_fraction
    ),
    1
  )
})

test_that("state comparison gives priority to constraint satisfaction", {
  reference <- make_record_test_state(
    routing = 2L,
    change = 0,
    energy = 2
  )

  fewer_constraints <- make_record_test_state(
    routing = 1L,
    change = 100,
    energy = 101
  )

  same_constraints_less_change <- make_record_test_state(
    routing = 2L,
    change = 1,
    energy = 3
  )

  reference$change_cost <- 2
  reference$total_energy <- 2

  expect_true(
    SSPICE:::.sspice_state_is_better(
      fewer_constraints,
      reference
    )
  )
  expect_true(
    SSPICE:::.sspice_state_is_better(
      same_constraints_less_change,
      reference
    )
  )
  expect_false(
    SSPICE:::.sspice_state_is_better(
      reference,
      fewer_constraints
    )
  )
})

test_that("base changes refresh endogenous VAUX and LEDIT while external VAUX stays frozen", {
  testthat::local_mocked_bindings(
    create_vaux_variables = function(
      inpdata,
      vauxs_data,
      extDFpath,
      routing_vnames
    ) {
      inpdata$VAUX_END <- inpdata$X * 10
      inpdata
    },
    create_logical_edit_variables = function(
      inpdata,
      logical_edits_data
    ) {
      inpdata$LEDIT_1 <- ifelse(
        inpdata$X == 2,
        1,
        NA_real_
      )
      inpdata
    },
    .package = "SSPICE"
  )

  record <- data.frame(
    ID = 1,
    X = 2,
    VAUX_EXT = -1,
    VAUX_END = NA_real_,
    LEDIT_1 = NA_real_
  )

  vaux_metadata <- data.frame(
    V1 = "VAUX_END",
    V2 = 1,
    V3 = "X",
    V4 = "",
    V5 = "",
    V6 = 2,
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  edit_metadata <- data.frame(
    V1 = "LEDIT_1",
    V2 = 1,
    V3 = "X",
    V4 = "",
    V5 = "",
    V6 = 2,
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = edit_metadata,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = "VAUX_END",
    frozen_values = list(VAUX_EXT = 99)
  )


  expect_identical(names(refreshed), names(record))
  expect_equal(refreshed$VAUX_END, 20)
  expect_equal(refreshed$LEDIT_1, 1)
  expect_equal(refreshed$VAUX_EXT, 99)
})

test_that("a directly proposed VAUX is preserved for the consistency check", {
  testthat::local_mocked_bindings(
    create_vaux_variables = function(...) {
      stop("a directly changed VAUX must not be recomputed immediately")
    },
    .package = "SSPICE"
  )

  record <- data.frame(
    ID = 1L,
    X = 2L,
    VAUX_END = 1L
  )

  metadata <- data.frame(
    V1 = "VAUX_END",
    V2 = 1,
    V3 = "X",
    V4 = "",
    V5 = "",
    V6 = 2,
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = metadata,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = "VAUX_END",
    frozen_values = list(),
    changed_variables = "VAUX_END",
    dependency_index = list(
      vaux = list(VAUX_END = "X"),
      logical_edits = list()
    )
  )

  expect_identical(refreshed$VAUX_END, 1L)
})

test_that("local correction defers only missing future endogenous VAUX", {
  recomputed <- character()
  testthat::local_mocked_bindings(
    create_vaux_variables = function(
      inpdata,
      vauxs_data,
      extDFpath,
      routing_vnames
    ) {
      variable <- as.character(vauxs_data[[1L]][1L])
      recomputed <<- c(recomputed, variable)
      inpdata[[variable]] <- inpdata$X * 10L
      inpdata
    },
    create_logical_edit_variables = function(
      inpdata,
      logical_edits_data
    ) {
      inpdata$LEDIT_1 <- 1L
      inpdata
    },
    .package = "SSPICE"
  )

  record <- data.frame(
    ID = 1L,
    X = 2L,
    VAUX_FUTURE = NA_integer_,
    VAUX_ACTIVE = 1L,
    LEDIT_1 = NA_integer_
  )
  vaux_metadata <- data.frame(
    V1 = c("VAUX_FUTURE", "VAUX_ACTIVE"),
    V2 = 1L,
    V3 = "X",
    V4 = "",
    V5 = "",
    V6 = 2L,
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )
  edit_metadata <- data.frame(
    V1 = "LEDIT_1",
    V2 = 1L,
    V3 = "X",
    V4 = "",
    V5 = "",
    V6 = 2L,
    V7 = "",
    V8 = "",
    V9 = "NULL",
    stringsAsFactors = FALSE
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = edit_metadata,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = c("VAUX_FUTURE", "VAUX_ACTIVE"),
    frozen_values = list(),
    changed_variables = "X",
    dependency_index = list(
      vaux = list(VAUX_FUTURE = "X", VAUX_ACTIVE = "X"),
      logical_edits = list(LEDIT_1 = "X")
    ),
    defer_missing_endogenous_vaux = TRUE
  )

  expect_identical(refreshed$VAUX_FUTURE, NA_integer_)
  expect_identical(refreshed$VAUX_ACTIVE, 20L)
  expect_identical(refreshed$LEDIT_1, 1L)
  expect_identical(recomputed, "VAUX_ACTIVE")
})

test_that("dependent endogenous VAUX variables are refreshed in dependency order", {
  testthat::local_mocked_bindings(
    create_vaux_variables = function(
      inpdata,
      vauxs_data,
      extDFpath,
      routing_vnames
    ) {
      variable <- as.character(vauxs_data[[1L]][1L])
      if (variable == "VAUX_A") {
        inpdata$VAUX_A <- inpdata$X + 1L
      } else if (variable == "VAUX_B") {
        inpdata$VAUX_B <- inpdata$VAUX_A * 2L
      }
      inpdata
    },
    .package = "SSPICE"
  )

  record <- data.frame(
    ID = 1L,
    X = 2L,
    VAUX_A = NA_integer_,
    VAUX_B = NA_integer_
  )

  metadata <- data.frame(
    V1 = c("VAUX_B", "VAUX_A"),
    V2 = c(1, 1),
    V3 = c("VAUX_A", "X"),
    V4 = c("", ""),
    V5 = c("", ""),
    V6 = c(2, 2),
    V7 = c("", ""),
    V8 = c("", ""),
    V9 = c("NULL", "NULL"),
    stringsAsFactors = FALSE
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = metadata,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = c("VAUX_B", "VAUX_A"),
    frozen_values = list(),
    changed_variables = "X",
    dependency_index = list(
      vaux = list(VAUX_B = "VAUX_A", VAUX_A = "X"),
      logical_edits = list()
    )
  )

  expect_equal(refreshed$VAUX_A, 3L)
  expect_equal(refreshed$VAUX_B, 6L)
})

test_that("arithmetic endogenous VAUX is refreshed after a base change", {
  make_row <- function(value, operand, token) {
    data.frame(
      V1 = "VAUX_PROP3",
      V2 = value,
      V3 = operand,
      V4 = "",
      V5 = "",
      V6 = "",
      V7 = "",
      V8 = "",
      V9 = token,
      stringsAsFactors = FALSE
    )
  }

  metadata <- rbind(
    make_row(1, "QPROP1", "+"),
    make_row(1, "QPROP2", "+"),
    make_row(1, "QPROP3", "<100"),
    make_row(2, "QPROP1", "+"),
    make_row(2, "QPROP2", "+"),
    make_row(2, "QPROP3", "=100")
  )

  record <- data.frame(
    ID = 1L,
    QPROP1 = 25,
    QPROP2 = 50,
    QPROP3 = 25,
    VAUX_PROP3 = NA_real_
  )

  refreshed <- SSPICE:::.sspice_refresh_derived_variables(
    record = record,
    vaux_metadata = metadata,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    routing_variable_names = names(record),
    endogenous_vaux = "VAUX_PROP3",
    frozen_values = list(),
    changed_variables = "QPROP3",
    dependency_index = list(
      vaux = list(
        VAUX_PROP3 = c("QPROP1", "QPROP2", "QPROP3")
      ),
      logical_edits = list()
    )
  )

  expect_equal(refreshed$VAUX_PROP3, 2)
})

test_that("a locally correctable record returns only final diagnostics", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(
      record,
      original,
      dag,
      record_id,
      fixed_variables,
      control,
      vaux_inconsistencies = 0L,
      items_mapping_file = NULL,
      routing_oracle = NULL,
      ...
    ) {
      if (record$X == 2) {
        return(make_record_test_state())
      }
      state <- make_record_test_state(
        routing = 1L,
        energy = 1
      )
      state$routing_check$full_report <- data.frame(
        ID = record$ID,
        varname_label = "X",
        routevar_label = "ID",
        violation_source = "ROUTING"
      )
      state
    },
    .sspice_violation_context = function(...) {
      list(
        nodes = "X",
        gates = character(),
        segment = character(),
        violated_nodes = "X",
        logical_edit_inputs = character()
      )
    },
    .sspice_generate_one_correction_proposal = function(record, ...) {
      proposed <- record
      proposed$X <- 2
      list(
        record = proposed,
        changed_variables = "X",
        old_values = as.character(record$X),
        new_values = "2",
        move_type = "flip",
        proposal_source = "uniform",
        predecessors_used = "ID",
        strata_used = "",
        donor_count = 0L,
        probability = 1
      )
    },
    .sspice_refresh_derived_variables = function(record, ...) record,
    .sspice_accept_sa_proposal = function(...) TRUE,
    .package = "SSPICE"
  )

  original <- data.frame(
    ID = 1L,
    X = 1L
  )

  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = make_record_test_dag(),
    record_id = "ID",
    domains = list(ID = integer(), X = 1:3),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 3L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list()
  )

  expect_equal(result$corrected_record$X, 2L)
  expect_true(result$resolved)
  expect_equal(result$final_routing_violations, 0L)
  expect_equal(result$final_logical_edit_violations, 0L)
  expect_equal(result$final_vaux_violations, 0L)
  expect_false("corrections" %in% names(result))
  expect_false("energy_trace" %in% names(result))
})

test_that("a record with no modifiable base node remains unresolved", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(...) {
      state <- make_record_test_state(routing = 1L, energy = 1)
      state$routing_check$full_report <- data.frame(
        ID = 1L,
        varname_label = "VAUX_EXT",
        routevar_label = NA_character_,
        violation_source = "VAUX"
      )
      state
    },
    .sspice_violation_context = function(...) {
      list(
        nodes = character(),
        gates = character(),
        segment = character(),
        violated_nodes = character(),
        logical_edit_inputs = character()
      )
    },
    .sspice_generate_one_correction_proposal = function(...) NULL,
    .sspice_refresh_derived_variables = function(record, ...) record,
    .package = "SSPICE"
  )

  original <- data.frame(ID = 1L, VAUX_EXT = 2L)
  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(varnames = names(original)),
    record_id = "ID",
    domains = list(ID = integer(), VAUX_EXT = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = c("ID", "VAUX_EXT"),
    control = correction_control(max_iterations = 3L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list(VAUX_EXT = 2L)
  )

  expect_identical(result$corrected_record, original)
  expect_false(result$resolved)
  expect_equal(result$final_routing_violations, 1L)
  expect_false("corrections" %in% names(result))
  expect_false("energy_trace" %in% names(result))
})

test_that("a rejected blank leaves no suppression in the next iteration", {
  proposal_number <- 0L
  observed_suppressions <- list()

  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(...) {
      state <- make_record_test_state(routing = 1L, energy = 1)
      state$routing_check$full_report <- data.frame(
        ID = 1L,
        varname_label = "X",
        routevar_label = "ID",
        violation_source = "ROUTING"
      )
      state
    },
    .sspice_violation_context = function(...) {
      list(
        nodes = "X",
        gates = character(),
        segment = character(),
        violated_nodes = "X",
        logical_edit_inputs = character()
      )
    },
    .sspice_generate_one_correction_proposal = function(record, ...) {
      proposal_number <<- proposal_number + 1L
      proposed <- record
      if (proposal_number == 1L) {
        proposed$VAUX_EXT <- NA_integer_
        return(list(
          record = proposed,
          changed_variables = "VAUX_EXT",
          old_values = as.character(record$VAUX_EXT),
          new_values = NA_character_,
          move_type = "blank",
          proposal_source = "routing_structure",
          predecessors_used = "",
          strata_used = "",
          donor_count = 0L,
          probability = 1
        ))
      }
      proposed$X <- 2L
      list(
        record = proposed,
        changed_variables = "X",
        old_values = as.character(record$X),
        new_values = "2",
        move_type = "flip",
        proposal_source = "uniform",
        predecessors_used = "ID",
        strata_used = "",
        donor_count = 0L,
        probability = 1
      )
    },
    .sspice_refresh_derived_variables = function(
        record, ..., suppress_frozen = character()) {
      observed_suppressions[[length(observed_suppressions) + 1L]] <<-
        suppress_frozen
      record
    },
    .sspice_accept_correction_proposal = function(...) FALSE,
    .package = "SSPICE"
  )

  original <- data.frame(
    ID = 1L,
    X = 1L,
    VAUX_EXT = 2L
  )

  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(
      varnames = names(original),
      logical_edit_names = character()
    ),
    record_id = "ID",
    domains = list(ID = integer(), X = 1:2, VAUX_EXT = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 2L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list(VAUX_EXT = 2L)
  )

  expect_identical(result$corrected_record, original)
  expect_false(result$resolved)
  # There is no longer an initial global refresh.  The first call belongs to
  # the rejected blank proposal; because that proposal is rejected, its
  # suppression does not leak into the following proposal.
  expect_length(observed_suppressions, 2L)
  expect_identical(observed_suppressions[[1L]], "VAUX_EXT")
  expect_identical(observed_suppressions[[2L]], character())
})

test_that("the best state survives a later accepted worsening move", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(
      record,
      original,
      dag,
      record_id,
      fixed_variables,
      control,
      vaux_inconsistencies = 0L,
      items_mapping_file = NULL,
      routing_oracle = NULL,
      ...
    ) {
      constraints <- c(`1` = 3L, `2` = 1L, `3` = 2L)[
        as.character(record$X)
      ]
      state <- make_record_test_state(
        routing = constraints,
        energy = constraints
      )
      state$routing_check$full_report <- data.frame(
        ID = record$ID,
        varname_label = "X",
        routevar_label = "ID",
        violation_source = "ROUTING"
      )
      state
    },
    .sspice_violation_context = function(...) {
      list(
        nodes = "X",
        gates = character(),
        segment = character(),
        violated_nodes = "X",
        logical_edit_inputs = character()
      )
    },
    .sspice_generate_one_correction_proposal = function(record, ...) {
      next_value <- if (record$X == 1L) 2L else 3L
      proposed <- record
      proposed$X <- next_value
      list(
        record = proposed,
        changed_variables = "X",
        old_values = as.character(record$X),
        new_values = as.character(next_value),
        move_type = "flip",
        proposal_source = "uniform",
        predecessors_used = "ID",
        strata_used = "",
        donor_count = 0L,
        probability = 1
      )
    },
    .sspice_refresh_derived_variables = function(record, ...) record,
    .sspice_accept_sa_proposal = function(...) TRUE,
    .package = "SSPICE"
  )

  original <- data.frame(
    ID = 1L,
    X = 1L
  )

  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = make_record_test_dag(),
    record_id = "ID",
    domains = list(ID = integer(), X = 1:3),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 2L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list()
  )

  expect_equal(result$corrected_record$X, 2L)
  expect_equal(result$final_routing_violations, 1L)
  expect_false(result$resolved)
})

test_that("an already valid record remains unchanged", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(...) {
      make_record_test_state()
    },
    .package = "SSPICE"
  )

  original <- data.frame(
    ID = 1L,
    X = 2L
  )

  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = make_record_test_dag(),
    record_id = "ID",
    domains = list(ID = integer(), X = 1:3),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 3L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list()
  )

  expect_identical(result$corrected_record, original)
  expect_true(result$resolved)
  expect_false("corrections" %in% names(result))
  expect_false("energy_trace" %in% names(result))
})

test_that("an H-reducing LEDIT repair is written into current and best", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(record, ...) {
      if (record$Q == 2L) return(make_record_test_state())
      state <- make_record_test_state(logical = 1L, energy = 1)
      state$routing_check$full_report <- data.frame(
        ID = record$ID,
        varname = 3L,
        varname_label = "LEDIT_1",
        routevar_label = NA_character_,
        violation_source = "LOGICAL_EDIT",
        stringsAsFactors = FALSE
      )
      # Deliberately disagree with the stable report index: a localized
      # LOGICAL_EDIT must still be processed and written into the record.
      state$routing_check$violation_variables <- 2L
      state$routing_check$violation_sources <- "LOGICAL_EDIT"
      state
    },
    .sspice_violation_context = function(...) {
      list(
        logical_edit_inputs = character(),
        vaux_inputs = character(),
        routing_oracle = NULL
      )
    },
    .sspice_find_logical_edit_repair = function(record, ...) {
      proposed <- record
      proposed$Q <- 2L
      proposed$LEDIT_1 <- 1L
      list(
        record = proposed,
        changed_variables = "Q",
        old_values = "1",
        new_values = "2",
        move_type = "logical_edit_repair",
        proposal_source = "logical_edit_domain_search"
      )
    },
    .sspice_generate_one_correction_proposal = function(...) {
      stop("generic proposal must not handle a localized LEDIT")
    },
    .sspice_refresh_derived_variables = function(record, ...) record,
    .package = "SSPICE"
  )

  original <- data.frame(ID = 1L, Q = 1L, LEDIT_1 = NA_integer_)
  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(
      varnames = names(original),
      logical_edit_names = "LEDIT_1"
    ),
    record_id = "ID",
    domains = list(ID = integer(), Q = 1:2, LEDIT_1 = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = c("ID", "LEDIT_1"),
    control = correction_control(max_iterations = 1L),
    vaux_metadata = NULL,
    logical_edit_metadata = data.frame(V1 = "LEDIT_1", V3 = "Q"),
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list(),
    dependency_index = list(
      vaux = list(),
      logical_edits = list(LEDIT_1 = "Q")
    )
  )

  expect_identical(result$corrected_record$Q, 2L)
  expect_identical(result$corrected_record$LEDIT_1, 1L)
  expect_true(result$resolved)
})

test_that("max_iterations counts complete sweeps, not queue elements", {
  visited_rows <- integer()

  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(...) {
      state <- make_record_test_state(routing = 2L, energy = 2)
      state$routing_check$full_report <- data.frame(
        ID = c(1L, 1L),
        varname = c(2L, 3L),
        varname_label = c("X", "LEDIT_1"),
        routevar_label = c("ID", NA_character_),
        violation_source = c("ROUTING", "LOGICAL_EDIT"),
        stringsAsFactors = FALSE
      )
      state
    },
    .sspice_violation_context = function(..., violation_row) {
      visited_rows <<- c(visited_rows, violation_row)
      list(
        nodes = "X",
        gates = character(),
        segment = character(),
        violated_nodes = "X",
        logical_edit_inputs = character()
      )
    },
    .sspice_generate_one_correction_proposal = function(record, ...) {
      proposed <- record
      proposed$X <- if (record$X == 1L) 2L else 1L
      list(
        record = proposed,
        changed_variables = "X",
        old_values = as.character(record$X),
        new_values = as.character(proposed$X),
        move_type = "flip",
        proposal_source = "uniform",
        predecessors_used = "ID",
        strata_used = "",
        donor_count = 0L,
        probability = 1
      )
    },
    .sspice_refresh_derived_variables = function(record, ...) record,
    .sspice_accept_correction_proposal = function(...) FALSE,
    .package = "SSPICE"
  )

  original <- data.frame(ID = 1L, X = 1L, LEDIT_1 = NA_integer_)

  SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(varnames = names(original), logical_edit_names = "LEDIT_1"),
    record_id = "ID",
    domains = list(ID = integer(), X = 1:2, LEDIT_1 = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = c("ID", "LEDIT_1"),
    control = correction_control(max_iterations = 1L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list()
  )

  expect_identical(visited_rows, c(1L, 2L))
})

test_that("a costly refreshed-VAUX jump retries the driver domain", {
  testthat::local_mocked_bindings(
    .sspice_evaluate_record_state = function(record, ...) {
      if (!is.na(record$NODE) && record$NODE == 1L) {
        return(make_record_test_state())
      }
      state <- make_record_test_state(routing = 1L, energy = 1)
      state$routing_check$full_report <- data.frame(
        ID = record$ID,
        varname = 2L,
        varname_label = "NODE",
        routevar_label = "ID",
        violation_source = "ROUTING",
        stringsAsFactors = FALSE
      )
      state$routing_check$violation_variables <- 2L
      state$routing_check$violation_sources <- "ROUTING"
      state
    },
    .sspice_violation_context = function(...) {
      list(
        violated_nodes = "NODE",
        logical_edit_inputs = character(),
        vaux_inputs = character(),
        routing_oracle = NULL
      )
    },
    .sspice_generate_one_correction_proposal = function(
        record, excluded_modes = list(), ...) {
      excluded <- excluded_modes$NODE
      if (is.null(excluded)) excluded <- character()
      value <- if ("2" %in% excluded) 1L else 2L
      proposed <- record
      proposed$NODE <- value
      list(
        record = proposed,
        changed_variables = "NODE",
        old_values = NA_character_,
        new_values = as.character(value),
        move_type = "fill",
        proposal_source = if (value == 2L) {
          "conditional_donor"
        } else {
          "routing_domain_fallback"
        }
      )
    },
    .sspice_refresh_derived_variables = function(record, ...) record,
    .sspice_propagate_refreshed_vaux_routing = function(
        before_refresh, refreshed, changed_variables,
        suppress_frozen, ...) {
      if (refreshed$NODE == 2L) {
        return(list(
          record = refreshed,
          changed_variables = changed_variables,
          suppress_frozen = suppress_frozen,
          usable = FALSE,
          rejected_vaux = "VAUX",
          jump_profile = list(filled_share = 1)
        ))
      }
      list(
        record = refreshed,
        changed_variables = changed_variables,
        suppress_frozen = suppress_frozen,
        usable = TRUE,
        rejected_vaux = character(),
        jump_profile = NULL
      )
    },
    .package = "SSPICE"
  )

  original <- data.frame(ID = 1L, NODE = NA_integer_)
  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(varnames = names(original), logical_edit_names = character()),
    record_id = "ID",
    domains = list(ID = integer(), NODE = 1:2),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 1L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = character(),
    frozen_values = list(),
    dependency_index = list(vaux = list(), logical_edits = list())
  )

  expect_identical(result$corrected_record$NODE, 1L)
  expect_true(result$resolved)
})

test_that("the initial record is evaluated without a global derived refresh", {
  refresh_calls <- 0L
  testthat::local_mocked_bindings(
    .sspice_refresh_derived_variables = function(...) {
      refresh_calls <<- refresh_calls + 1L
      stop("unexpected initial refresh")
    },
    .sspice_evaluate_record_state = function(record, ...) {
      make_record_test_state()
    },
    .package = "SSPICE"
  )

  original <- data.frame(
    ID = 1L,
    BASE = 1L,
    VAUX_A = 2L,
    VAUX_UNRELATED = 1L
  )
  result <- SSPICE:::.sspice_correct_record(
    record = original,
    original = original,
    dag = list(
      varnames = names(original),
      logical_edit_names = character()
    ),
    record_id = "ID",
    domains = list(),
    donor_data = NULL,
    strata = character(),
    imputation_method = "uniform",
    priors = list(),
    fixed_variables = "ID",
    control = correction_control(max_iterations = 1L),
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    external_data_dir = tempdir(),
    endogenous_vaux = c("VAUX_A", "VAUX_UNRELATED"),
    frozen_values = list(),
    dependency_index = list(
      vaux = list(VAUX_A = "BASE", VAUX_UNRELATED = "OTHER"),
      logical_edits = list()
    )
  )

  expect_identical(refresh_calls, 0L)
  expect_identical(result$corrected_record, original)
  expect_true(result$resolved)
})

test_that("an endogenous VAUX mode brings coherent donor inputs backward", {
  record <- data.frame(
    ID = 1L,
    A = 1L,
    B = 3L,
    VAUX_A = NA_integer_
  )
  donors <- data.frame(
    ID = 2:3,
    A = c(2L, 2L),
    B = c(4L, 4L),
    VAUX_A = c(2L, 2L)
  )
  metadata <- data.frame(
    V1 = c("VAUX_A", "VAUX_A"),
    V2 = c(2L, 2L),
    V3 = c("A", "B"),
    V4 = c("", ""),
    V5 = c("", ""),
    V6 = c(2L, 4L),
    V7 = c("", ""),
    V8 = c("", ""),
    V9 = c("AND", "NULL"),
    stringsAsFactors = FALSE
  )

  proposal <- SSPICE:::.sspice_vaux_donor_coherence_proposal(
    record = record,
    vaux_var = "VAUX_A",
    target_mode = 2L,
    vaux_metadata = metadata,
    donor_data = donors,
    fixed_variables = "ID",
    routing_oracle = NULL,
    control = correction_control()
  )

  expect_false(is.null(proposal))
  expect_identical(proposal$record$VAUX_A, 2L)
  expect_identical(proposal$record$A, 2L)
  expect_identical(proposal$record$B, 4L)
  expect_setequal(proposal$changed_variables, c("VAUX_A", "A", "B"))
  expect_identical(
    proposal$move_type,
    "vaux_endogenous_backward_coherence"
  )
})
