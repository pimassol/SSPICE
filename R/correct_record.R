.sspice_constraint_score <- function(state) {
  state$routing_violations + state$logical_edit_violations +
    state$vaux_violations
}

.sspice_state_is_better <- function(candidate, reference) {
  candidate_constraints <- .sspice_constraint_score(candidate)
  reference_constraints <- .sspice_constraint_score(reference)
  candidate_constraints < reference_constraints ||
    (candidate_constraints == reference_constraints &&
       candidate$change_cost < reference$change_cost) ||
    (candidate_constraints == reference_constraints &&
       candidate$change_cost == reference$change_cost &&
       candidate$total_energy < reference$total_energy)
}

.sspice_accept_correction_proposal <- function(
    current_state, candidate_state, transition_energy, temperature) {
  current_constraints <- .sspice_constraint_score(current_state)
  candidate_constraints <- .sspice_constraint_score(candidate_state)
  if (candidate_constraints < current_constraints) return(TRUE)
  .sspice_accept_sa_proposal(
    current_state$total_energy,
    transition_energy,
    temperature
  )
}

.sspice_sa_temperature <- function(
    iteration,
    max_iterations,
    initial_temperature,
    final_temperature,
    cooling_fraction = 0.90) {
  if (max_iterations <= 1L) {
    return(as.numeric(initial_temperature))
  }
  cooling_end <- floor(cooling_fraction * max_iterations)
  cooling_end <- max(2L, min(max_iterations, cooling_end))
  if (iteration >= cooling_end) {
    return(as.numeric(final_temperature))
  }
  final_temperature +
    (initial_temperature - final_temperature) *
      (cooling_end - iteration) /
      (cooling_end - 1L)
}

.sspice_violation_queue <- function(state) {
  report <- state$routing_check$full_report
  score <- .sspice_constraint_score(state)
  size <- if (is.null(report)) 0L else nrow(report)
  if ((score == 0L && size != 0L) || (score > 0L && size == 0L)) {
    stop(
      "Internal inconsistency between H and the localized violations.",
      call. = FALSE
    )
  }
  if (!size) {
    return(list(rows = integer(), variables = integer(), sources = character()))
  }
  list(
    rows = seq_len(size),
    variables = if ("varname" %in% names(report)) {
      as.integer(report$varname)
    } else {
      rep(NA_integer_, size)
    },
    sources = if ("violation_source" %in% names(report)) {
      as.character(report$violation_source)
    } else {
      rep("ROUTING", size)
    }
  )
}

.sspice_queue_item_is_active <- function(state, variable, source) {
  variables <- state$routing_check$violation_variables %||% integer()
  sources <- state$routing_check$violation_sources %||% character()
  if (!length(variables)) return(.sspice_constraint_score(state) > 0L)
  if (is.na(variable)) return(TRUE)
  any(variables == variable & sources == source)
}

.sspice_propagate_refreshed_vaux_routing <- function(
    before_refresh, refreshed, changed_variables, dag, fixed_variables,
    control, vaux_metadata, logical_edit_metadata, external_data_dir,
    routing_variable_names, endogenous_vaux, frozen_values,
    mode_locked_values, dependency_index, suppress_frozen) {
  all_changed <- unique(as.character(changed_variables))
  if (is.null(dependency_index) || !length(endogenous_vaux)) {
    return(list(
      record = refreshed,
      changed_variables = all_changed,
      suppress_frozen = suppress_frozen,
      usable = TRUE,
      rejected_vaux = character(),
      jump_profile = NULL
    ))
  }
  previous <- before_refresh
  current <- refreshed
  pending_changes <- all_changed
  max_passes <- max(1L, length(dag$varnames))
  pass <- 0L

  repeat {
    pass <- pass + 1L
    affected <- .sspice_affected_derived_variables(
      pending_changes,
      dependency_index
    )
    candidate_vaux <- unique(c(
      intersect(pending_changes, endogenous_vaux),
      intersect(affected$vaux %||% character(), endogenous_vaux)
    ))
    candidate_vaux <- intersect(candidate_vaux, names(current))
    changed_vaux <- candidate_vaux[vapply(
      candidate_vaux,
      function(variable) {
        old <- previous[[variable]][1L]
        new <- current[[variable]][1L]
        if (is.na(old) && is.na(new)) return(FALSE)
        if (xor(is.na(old), is.na(new))) return(TRUE)
        as.character(old) != as.character(new)
      },
      logical(1L)
    )]

    if (!length(changed_vaux)) break

    blankable_locked <- unique(c(
      names(frozen_values),
      names(mode_locked_values)
    ))
    protected_variables <- setdiff(
      fixed_variables,
      blankable_locked
    )
    variables_to_blank <- character()

    for (variable in changed_vaux) {
      value <- current[[variable]][1L]
      if (is.na(value)) next
      profile <- .sspice_mode_jump_profile(
        variable = variable,
        value = value,
        record = current,
        dag = dag,
        fixed_variables = protected_variables
      )
      if (!isTRUE(profile$jumps)) next
      jump_is_usable <-
        profile$filled_share <= control$branch_fill_threshold &&
        !length(profile$immutable_filled)
      if (!jump_is_usable) {
        return(list(
          record = current,
          changed_variables = all_changed,
          suppress_frozen = suppress_frozen,
          usable = FALSE,
          rejected_vaux = variable,
          jump_profile = profile
        ))
      }
      variables_to_blank <- unique(c(
        variables_to_blank,
        profile$filled_variables
      ))
    }

    variables_to_blank <- intersect(
      setdiff(variables_to_blank, protected_variables),
      names(current)
    )
    variables_to_blank <- variables_to_blank[vapply(
      current[variables_to_blank],
      function(column) !is.na(column[1L]),
      logical(1L)
    )]
    if (!length(variables_to_blank)) break

    previous <- current
    for (variable in variables_to_blank) current[[variable]] <- NA
    blanked_frozen <- intersect(variables_to_blank, names(frozen_values))
    suppress_frozen <- unique(c(suppress_frozen, blanked_frozen))
    all_changed <- unique(c(all_changed, variables_to_blank))
    pending_changes <- variables_to_blank
    current <- .sspice_refresh_derived_variables(
      current, vaux_metadata, logical_edit_metadata,
      external_data_dir, routing_variable_names, endogenous_vaux,
      frozen_values, changed_variables = variables_to_blank,
      dependency_index = dependency_index,
      suppress_frozen = suppress_frozen,
      defer_missing_endogenous_vaux = TRUE
    )
    if (pass >= max_passes) break
  }

  list(
    record = current,
    changed_variables = all_changed,
    suppress_frozen = suppress_frozen,
    usable = TRUE,
    rejected_vaux = character(),
    jump_profile = NULL
  )
}

.sspice_correct_record <- function(
    record, original, dag, record_id, domains, donor_data, strata,
    imputation_method, priors, fixed_variables, control,
    vaux_metadata, logical_edit_metadata, external_data_dir,
    endogenous_vaux, frozen_values, mode_locked_values = list(),
    items_mapping_file = NULL,
    routing_oracle = NULL, dependency_index = NULL,
    donor_cache = NULL) {
  if (is.null(dependency_index) && !is.null(routing_oracle)) {
    dependency_index <- routing_oracle$dependency_index
  }
  proposal_vaux_metadata <- if (
    is.null(vaux_metadata) || !length(endogenous_vaux)
  ) {
    NULL
  } else {
    vaux_metadata[
      as.character(vaux_metadata[[1L]]) %in% endogenous_vaux,
      ,
      drop = FALSE
    ]
  }
  suppress_frozen_vars <- character()
  
  # The input record already comes from prepare_data(), where VAUX and
  # logical-edit variables have been constructed.  Preserve that exact state
  # for the initial localization.  Derived variables are refreshed locally
  # only after a proposal, using proposal$changed_variables and the dependency
  # index below.
  current <- record
  current_state <- .sspice_evaluate_record_state(
    current, original, dag, record_id, fixed_variables, control,
    items_mapping_file = items_mapping_file,
    routing_oracle = routing_oracle,
    vaux_metadata = vaux_metadata,
    external_data_dir = external_data_dir,
    endogenous_vaux = endogenous_vaux,
    build_report = TRUE
  )
  best <- current
  best_state <- current_state
  initial_constraints <- .sspice_constraint_score(current_state)
  max_iterations <- control$max_iterations %||%
    length(dag$varnames)
  elementary_change_scale <- control$lambda_change * max(
    control$penalty_fill,
    control$penalty_blank,
    control$penalty_flip
  )
  initial_temperature <- control$initial_temperature %||%
    max(
      control$final_temperature,
      floor(as.integer(initial_constraints)),
      elementary_change_scale
    )
  iteration <- 0L
  while (iteration < max_iterations &&
         .sspice_constraint_score(current_state) > 0L) {
    iteration <- iteration + 1L
    sweep_state <- current_state
    queue <- .sspice_violation_queue(sweep_state)
    if (!length(queue$rows)) break

    temperature <- .sspice_sa_temperature(
      iteration = iteration,
      max_iterations = max_iterations,
      initial_temperature = initial_temperature,
      final_temperature = control$final_temperature,
      cooling_fraction = control$cooling_fraction
    )

    for (queue_position in seq_along(queue$rows)) {
      if (.sspice_constraint_score(current_state) == 0L) break
      queue_source <- queue$sources[[queue_position]]
      # A LOGICAL_EDIT row in the stable sweep report is authoritative.  Its
      # derived-variable index can differ from the compact live index after an
      # incremental refresh; applying the generic active-row filter here would
      # silently skip the base-variable repair for every sweep.
      if (!identical(queue_source, "LOGICAL_EDIT") &&
          !.sspice_queue_item_is_active(
        current_state,
        queue$variables[[queue_position]],
        queue_source
      )) next
    context <- .sspice_violation_context(
      state = sweep_state,
      record = current,
      dag = dag,
      record_id = record_id,
      fixed_variables = fixed_variables,
      logical_edit_metadata = logical_edit_metadata,
      violation_row = queue$rows[[queue_position]]
    )
    # A localized logical-edit violation must act on its base variables.  Use
    # the queue row as the authoritative edit identity and bypass the generic
    # node fallback; otherwise a valid repair can be found in isolation but
    # never written into the MCMC record.
    logical_edit_name <- character()
    if (identical(queue_source, "LOGICAL_EDIT")) {
      report <- sweep_state$routing_check$full_report
      report_row <- queue$rows[[queue_position]]
      if (!is.null(report) && report_row <= nrow(report) &&
          "varname_label" %in% names(report)) {
        logical_edit_name <- as.character(report$varname_label[[report_row]])
      }
      logical_edit_name <- intersect(
        logical_edit_name,
        dag$logical_edit_names %||% character()
      )
    }
    proposal <- NULL
    proposal_is_logical <- FALSE
    if (length(logical_edit_name)) {
      context$violated_logical_edits <- logical_edit_name
      indexed_inputs <- dependency_index$logical_edits[[logical_edit_name]]
      if (!is.null(indexed_inputs)) {
        context$logical_edit_inputs <- unique(c(
          indexed_inputs,
          context$vaux_inputs %||% character()
        ))
      }
      proposal <- .sspice_find_logical_edit_repair(
        record = current,
        edit_name = logical_edit_name,
        logical_edit_metadata = logical_edit_metadata,
        context = context,
        dag = dag,
        domains = domains,
        fixed_variables = fixed_variables,
        control = control
      )
      proposal_is_logical <- !is.null(proposal)
    }
    excluded_modes <- list()
    repeat {
      if (is.null(proposal) && !length(logical_edit_name)) {
        proposal <- .sspice_generate_one_correction_proposal(
          record = current,
          context = context,
          dag = dag,
          domains = domains,
          donor_data = donor_data,
          donor_cache = donor_cache,
          strata = strata,
          imputation_method = imputation_method,
          priors = priors,
          fixed_variables = fixed_variables,
          control = control,
          vaux_metadata = proposal_vaux_metadata,
          logical_edit_metadata = logical_edit_metadata,
          locked_values = mode_locked_values,
          excluded_modes = excluded_modes
        )
      }
      if (is.null(proposal)) break

      candidate_suppress_frozen_vars <- suppress_frozen_vars
      changed_frozen <- intersect(
        proposal$changed_variables,
        names(frozen_values)
      )
      blanked_frozen <- changed_frozen[vapply(
        changed_frozen,
        function(variable) is.na(proposal$record[[variable]][1L]),
        logical(1L)
      )]
      if (length(blanked_frozen)) {
        candidate_suppress_frozen_vars <- unique(c(
          candidate_suppress_frozen_vars,
          blanked_frozen
        ))
      }

      refreshed <- .sspice_refresh_derived_variables(
        proposal$record, vaux_metadata, logical_edit_metadata,
        external_data_dir, names(record), endogenous_vaux, frozen_values,
        changed_variables = proposal$changed_variables,
        dependency_index = dependency_index,
        suppress_frozen = candidate_suppress_frozen_vars,
        defer_missing_endogenous_vaux = TRUE
      )
      propagated <- .sspice_propagate_refreshed_vaux_routing(
        before_refresh = proposal$record,
        refreshed = refreshed,
        changed_variables = proposal$changed_variables,
        dag = dag,
        fixed_variables = fixed_variables,
        control = control,
        vaux_metadata = vaux_metadata,
        logical_edit_metadata = logical_edit_metadata,
        external_data_dir = external_data_dir,
        routing_variable_names = names(record),
        endogenous_vaux = endogenous_vaux,
        frozen_values = frozen_values,
        mode_locked_values = mode_locked_values,
        dependency_index = dependency_index,
        suppress_frozen = candidate_suppress_frozen_vars
      )
      if (isTRUE(propagated$usable)) {
        refreshed <- propagated$record
        proposal$changed_variables <- propagated$changed_variables
        candidate_suppress_frozen_vars <- propagated$suppress_frozen
        break
      }
      if (isTRUE(proposal_is_logical) ||
          !length(proposal$changed_variables)) {
        proposal <- NULL
        break
      }
      driver <- proposal$changed_variables[[1L]]
      driver_value <- proposal$record[[driver]][1L]
      if (is.na(driver_value)) {
        proposal <- NULL
        break
      }
      excluded_modes[[driver]] <- unique(c(
        excluded_modes[[driver]] %||% character(),
        as.character(driver_value)
      ))
      proposal <- NULL
    }
    if (is.null(proposal)) next

    candidate_state <- .sspice_evaluate_record_state(
      refreshed, original, dag, record_id, fixed_variables, control,
      items_mapping_file = items_mapping_file,
      routing_oracle = routing_oracle,
      vaux_metadata = vaux_metadata,
      external_data_dir = external_data_dir,
      endogenous_vaux = endogenous_vaux,
      build_report = FALSE,
      previous_state = current_state,
      changed_variables = proposal$changed_variables
    )
    transition_energy <- candidate_state$total_energy +
      control$future_weight * .sspice_future_cost(
        current_state,
        candidate_state
      )
    candidate <- list(
      record = refreshed,
      state = candidate_state
    )
    deterministic_logical_repair <-
      identical(proposal$move_type, "logical_edit_repair") &&
      .sspice_constraint_score(candidate_state) <
        .sspice_constraint_score(current_state)
    accepted <- deterministic_logical_repair ||
      .sspice_accept_correction_proposal(
        current_state = current_state,
        candidate_state = candidate_state,
        transition_energy = transition_energy,
        temperature = temperature
      )
    if (accepted) {
      suppress_frozen_vars <- candidate_suppress_frozen_vars
      current <- candidate$record
      current_state <- candidate$state
      if (.sspice_state_is_better(current_state, best_state)) {
        best <- current
        best_state <- current_state
      }
      if (.sspice_constraint_score(current_state) == 0L) break
    }
    }

    # Relocalize once per complete sweep.  The queue is intentionally stable
    # while it is scanned; accepted moves can remove or create violations.
    if (.sspice_constraint_score(current_state) > 0L) {
      current_state <- .sspice_evaluate_record_state(
        current, original, dag, record_id, fixed_variables, control,
        items_mapping_file = items_mapping_file,
        routing_oracle = routing_oracle,
        vaux_metadata = vaux_metadata,
        external_data_dir = external_data_dir,
        endogenous_vaux = endogenous_vaux,
        build_report = TRUE
      )
    }
  }

  # Final per-record diagnostics remain oracle based.  The sole canonical
  # routing_check() is run by correct_data() after records are recombined.
  best_state <- .sspice_evaluate_record_state(
    best, original, dag, record_id, fixed_variables, control,
    items_mapping_file = items_mapping_file,
    routing_oracle = routing_oracle,
    vaux_metadata = vaux_metadata,
    external_data_dir = external_data_dir,
    endogenous_vaux = endogenous_vaux,
    build_report = TRUE
  )
  invisible(.sspice_violation_queue(best_state))


  list(
    corrected_record = best,
    final_routing_violations = best_state$routing_violations,
    final_logical_edit_violations = best_state$logical_edit_violations,
    final_vaux_violations = best_state$vaux_violations,
    final_energy = best_state$total_energy,
    resolved = .sspice_constraint_score(best_state) == 0L
  )
}
