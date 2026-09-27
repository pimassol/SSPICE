.sspice_cast_mode <- function(value, template) {
  if (is.integer(template)) return(as.integer(value))
  if (is.numeric(template)) return(as.numeric(value))
  if (is.factor(template)) return(factor(value, levels = levels(template)))
  as.character(value)
}

.sspice_is_quantitative_node <- function(variable, record, dag) {
  index <- match(variable, dag$varnames)
  if (is.na(index) || is.null(dag$routes_numeric) ||
      ncol(dag$routes_numeric) < 4L) return(FALSE)
  number_items <- suppressWarnings(as.integer(dag$routes_numeric[index, 2L]))
  minimum <- suppressWarnings(as.numeric(dag$routes_numeric[index, 3L]))
  maximum <- suppressWarnings(as.numeric(dag$routes_numeric[index, 4L]))
  identical(number_items, 1L) &&
    is.finite(minimum) && is.finite(maximum) && maximum > minimum
}

.sspice_node_allows_missing <- function(variable, dag) {
  index <- match(variable, dag$varnames)
  if (is.na(index) || is.null(dag$routes_numeric) ||
      ncol(dag$routes_numeric) < 3L) return(FALSE)
  minimum <- suppressWarnings(as.numeric(dag$routes_numeric[index, 3L]))
  is.na(minimum)
}

.sspice_arithmetic_constraints <- function(variable, record, vaux_metadata) {
  if (is.null(vaux_metadata) || !nrow(vaux_metadata) ||
      ncol(vaux_metadata) < 9L) return(list())
  metadata <- vaux_metadata
  names(metadata)[seq_len(9L)] <- paste0("V", seq_len(9L))
  vaux_names <- unique(trimws(as.character(metadata$V1)))
  constraints <- list()
  for (vaux in vaux_names) {
    rows <- metadata[trimws(as.character(metadata$V1)) == vaux, , drop = FALSE]
    current_operands <- character()
    current_operators <- character()
    rules <- list()
    for (index in seq_len(nrow(rows))) {
      operand <- trimws(as.character(rows$V3[index]))
      token <- trimws(as.character(rows$V9[index]))
      token <- trimws(sub("(?i)(^|\\s)NULL\\s*$", "", token, perl = TRUE))
      if (!is.na(operand) && nzchar(operand) && operand != ".") {
        current_operands <- c(current_operands, operand)
      }
      if (token %in% c("+", ") + (")) {
        current_operators <- c(current_operators, "+")
        next
      }
      if (grepl("^(<=|>=|==|=|<|>)\\s*[-+]?[0-9]+([.][0-9]+)?$", token)) {
        comparator <- sub("^\\s*(<=|>=|==|=|<|>).*?$", "\\1", token)
        threshold <- suppressWarnings(as.numeric(sub(
          "^\\s*(<=|>=|==|=|<|>)\\s*", "", token
        )))
        additive <- length(current_operands) >= 1L &&
          length(current_operators) == max(0L, length(current_operands) - 1L) &&
          all(current_operators == "+")
        if (additive && variable %in% current_operands && is.finite(threshold)) {
          other_variables <- current_operands[current_operands != variable]
          if (all(other_variables %in% names(record))) {
            other_values <- suppressWarnings(as.numeric(vapply(
              other_variables,
              function(name) as.character(record[[name]][1L]),
              character(1)
            )))
            if (all(is.finite(other_values))) {
              rules[[length(rules) + 1L]] <- list(
                comparator = comparator,
                threshold = threshold,
                other_sum = sum(other_values)
              )
            }
          }
        }
      }
      current_operands <- character()
      current_operators <- character()
    }
    if (length(rules)) constraints[[vaux]] <- rules
  }
  constraints
}

.sspice_numeric_value_satisfies <- function(value, constraints) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value) || !length(constraints)) return(is.finite(value))
  all(vapply(constraints, function(rules) {
    any(vapply(rules, function(rule) {
      total <- rule$other_sum + value
      switch(
        rule$comparator,
        "<" = total < rule$threshold,
        "<=" = total <= rule$threshold,
        ">" = total > rule$threshold,
        ">=" = total >= rule$threshold,
        "=" = isTRUE(all.equal(total, rule$threshold)),
        "==" = isTRUE(all.equal(total, rule$threshold)),
        FALSE
      )
    }, logical(1)))
  }, logical(1)))
}

.sspice_node_mode_proposals <- function(
    record, variable, role, dag, domains, donor_data, strata,
    imputation_method, priors, control, donor_cache = NULL) {
  distribution <- .sspice_candidate_node_modes(
    variable, record, dag, domains, donor_data, strata,
    imputation_method, priors, control, donor_cache = donor_cache
  )
  current <- record[[variable]][1L]
  result <- vector("list", 0L)
  for (index in seq_along(distribution$values)) {
    value <- .sspice_cast_mode(distribution$values[index], record[[variable]])
    if (!is.na(current) && as.character(value) == as.character(current)) next
    proposed <- record
    proposed[[variable]] <- value
    result[[length(result) + 1L]] <- list(
      record = proposed,
      changed_variables = variable,
      old_values = as.character(current),
      new_values = as.character(value),
      move_type = if (role == "gate") "gate_flip" else
        if (is.na(current)) "fill" else "flip",
      proposal_source = distribution$source,
      predecessors_used = distribution$predecessors_used,
      strata_used = distribution$strata_used,
      donor_count = distribution$donor_count,
      probability = distribution$probabilities[index]
    )
  }
  result
}

.sspice_blank_proposal <- function(record, variables, move_type) {
  variables <- variables[vapply(record[variables], function(x) !is.na(x[1L]), logical(1))]
  if (!length(variables)) return(NULL)
  proposed <- record
  old <- vapply(proposed[variables], function(x) as.character(x[1L]), character(1))
  for (variable in variables) proposed[[variable]] <- NA
  list(
    record = proposed,
    changed_variables = variables,
    old_values = old,
    new_values = rep(NA_character_, length(variables)),
    move_type = move_type,
    proposal_source = "routing_structure",
    predecessors_used = "",
    strata_used = "",
    donor_count = 0L,
    probability = NA_real_
  )
}

.sspice_vaux_topdown_proposal <- function(record, vaux_var, target_mode, vaux_metadata) {
  if (is.null(vaux_metadata) || !nrow(vaux_metadata)) return(NULL)
  rows <- vaux_metadata[trimws(as.character(vaux_metadata[[1L]])) == vaux_var &
                          trimws(as.character(vaux_metadata[[2L]])) == as.character(target_mode), , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  proposed <- record
  changed_vars <- character()
  old_vals <- character()
  new_vals <- character()
  
  for (i in seq_len(nrow(rows))) {
    base_var <- trimws(as.character(rows[[3L]][i]))
    val_expr <- trimws(as.character(rows[[9L]][i]))
    if (base_var %in% names(proposed) && base_var != "." && nzchar(base_var)) {
      clean_val <- gsub("[=><]", "", val_expr)
      clean_val <- trimws(clean_val)
      if (nzchar(clean_val)) {
        old_val <- as.character(proposed[[base_var]][1L])
        cast_val <- .sspice_cast_mode(clean_val, proposed[[base_var]])
        proposed[[base_var]] <- cast_val
        changed_vars <- c(changed_vars, base_var)
        old_vals <- c(old_vals, old_val)
        new_vals <- c(new_vals, as.character(cast_val))
      }
    }
  }
  if (!length(changed_vars)) return(NULL)
  list(
    record = proposed,
    changed_variables = changed_vars,
    old_values = old_vals,
    new_values = new_vals,
    move_type = "vaux_endogenous_topdown",
    proposal_source = "vaux_statement_coherence",
    predecessors_used = "",
    strata_used = "",
    donor_count = 0L,
    probability = 1.0
  )
}

.sspice_vaux_donor_coherence_proposal <- function(
    record, vaux_var, target_mode, vaux_metadata, donor_data,
    fixed_variables, routing_oracle, control) {
  if (is.null(vaux_metadata) || !nrow(vaux_metadata) ||
      is.null(donor_data) || !nrow(donor_data) ||
      !vaux_var %in% names(record) ||
      !vaux_var %in% names(donor_data)) return(NULL)

  rows <- trimws(as.character(vaux_metadata[[1L]])) == vaux_var
  inputs <- unique(trimws(as.character(vaux_metadata[[3L]][rows])))
  inputs <- intersect(
    inputs[!is.na(inputs) & nzchar(inputs) & inputs != "."],
    intersect(names(record), names(donor_data))
  )
  if (!length(inputs)) return(NULL)

  matching <- !is.na(donor_data[[vaux_var]]) &
    as.character(donor_data[[vaux_var]]) == as.character(target_mode)
  donors <- donor_data[matching, inputs, drop = FALSE]
  if (!nrow(donors)) return(NULL)
  donors <- unique(donors)

  maximum <- control$logical_repair_max_evaluations %||% 1000L
  if (nrow(donors) > maximum) {
    donors <- donors[seq_len(maximum), , drop = FALSE]
  }

  candidates <- vector("list", 0L)
  for (row_index in seq_len(nrow(donors))) {
    proposed <- record
    proposed[[vaux_var]] <- .sspice_cast_mode(
      target_mode,
      record[[vaux_var]]
    )
    for (input in inputs) {
      proposed[[input]] <- .sspice_cast_mode(
        donors[[input]][row_index],
        record[[input]]
      )
    }

    changed_inputs <- inputs[vapply(
      inputs,
      function(input) {
        old <- record[[input]][1L]
        new <- proposed[[input]][1L]
        if (is.na(old) && is.na(new)) return(FALSE)
        if (xor(is.na(old), is.na(new))) return(TRUE)
        as.character(old) != as.character(new)
      },
      logical(1L)
    )]
    if (length(intersect(changed_inputs, fixed_variables))) next

    changed <- unique(c(vaux_var, changed_inputs))
    structural <- 0L
    if (!is.null(routing_oracle)) {
      fast <- .sspice_fast_routing_state(
        proposed,
        routing_oracle,
        build_report = FALSE
      )
      structural <- fast$routing_violations + fast$vaux_violations
    }
    candidates[[length(candidates) + 1L]] <- list(
      record = proposed,
      changed = changed,
      structural = structural,
      cost = .sspice_logical_repair_cost(
        record,
        proposed,
        changed,
        control
      )
    )
  }
  if (!length(candidates)) return(NULL)

  structural <- vapply(candidates, `[[`, numeric(1L), "structural")
  costs <- vapply(candidates, `[[`, numeric(1L), "cost")
  best <- candidates[[order(structural, costs)[[1L]]]]
  changed <- best$changed
  list(
    record = best$record,
    changed_variables = changed,
    old_values = vapply(record[changed], function(x) {
      as.character(x[1L])
    }, character(1L)),
    new_values = vapply(best$record[changed], function(x) {
      as.character(x[1L])
    }, character(1L)),
    move_type = "vaux_endogenous_backward_coherence",
    proposal_source = "exact_donor_vaux_coherence",
    predecessors_used = "",
    strata_used = "",
    donor_count = nrow(donors),
    probability = 1
  )
}

.sspice_logical_edit_values <- function(records, logical_edit_metadata) {
  if (is.null(logical_edit_metadata) || !nrow(logical_edit_metadata)) {
    return(records)
  }
  edit_names <- unique(trimws(as.character(logical_edit_metadata[[1L]])))
  base <- records[, setdiff(names(records), edit_names), drop = FALSE]
  create_logical_edit_variables(
    inpdata = base,
    logical_edits_data = logical_edit_metadata
  )
}

.sspice_logical_repair_cost <- function(record, proposed, variables, control) {
  sum(vapply(variables, function(variable) {
    old <- record[[variable]][1L]
    new <- proposed[[variable]][1L]
    if (is.na(old) && !is.na(new)) return(control$penalty_fill)
    if (!is.na(old) && is.na(new)) return(control$penalty_blank)
    control$penalty_flip
  }, numeric(1L)))
}

.sspice_logical_candidate_values <- function(
    variable, record, dag, domains, include_missing = TRUE) {
  values <- domains[[variable]] %||% vector(mode(record[[variable]]), 0L)
  values <- unique(as.character(values[!is.na(values)]))
  current <- record[[variable]][1L]
  values <- values[values != as.character(current)]
  result <- lapply(values, .sspice_cast_mode, template = record[[variable]])
  if (isTRUE(include_missing) && !is.na(current) &&
      .sspice_node_allows_missing(variable, dag)) {
    result <- c(list(NA), result)
  }
  result
}

.sspice_find_logical_edit_repair <- function(
    record, edit_name, logical_edit_metadata, context, dag, domains,
    fixed_variables, control) {
  if (is.null(logical_edit_metadata) || !nrow(logical_edit_metadata) ||
      !length(edit_name)) return(NULL)
  edit_name <- as.character(edit_name[[1L]])
  all_edit_names <- unique(trimws(as.character(logical_edit_metadata[[1L]])))
  if (!edit_name %in% all_edit_names || !edit_name %in% names(record)) {
    return(NULL)
  }
  inputs <- context$logical_edit_inputs %||% character()
  inputs <- intersect(inputs, names(record))
  inputs <- setdiff(inputs, unique(c(
    fixed_variables,
    all_edit_names,
    names(context$locked_values %||% list())
  )))
  if (!length(inputs)) return(NULL)
  inputs <- inputs[order(
    vapply(inputs, function(variable) is.na(record[[variable]][1L]), logical(1L)),
    decreasing = FALSE
  )]

  baseline <- .sspice_logical_edit_values(record, logical_edit_metadata)
  protected_edits <- setdiff(all_edit_names, edit_name)
  protected_edits <- intersect(protected_edits, names(baseline))
  protected_edits <- protected_edits[!vapply(
    baseline[protected_edits],
    function(x) is.na(x[1L]),
    logical(1L)
  )]
  oracle <- context$routing_oracle %||% NULL
  maximum <- control$logical_repair_max_evaluations %||% 1000L
  evaluated <- 0L
  valid <- list()

  evaluate_batch <- function(records, changed_variables) {
    if (!nrow(records) || evaluated >= maximum) return(invisible(NULL))
    keep_count <- min(nrow(records), maximum - evaluated)
    records <- records[seq_len(keep_count), , drop = FALSE]
    evaluated <<- evaluated + nrow(records)
    refreshed <- .sspice_logical_edit_values(records, logical_edit_metadata)
    target_valid <- !is.na(refreshed[[edit_name]])
    protected_valid <- if (!length(protected_edits)) {
      rep(TRUE, nrow(refreshed))
    } else {
      rowSums(is.na(refreshed[, protected_edits, drop = FALSE])) == 0L
    }
    candidate_rows <- which(target_valid & protected_valid)
    if (!length(candidate_rows)) return(invisible(NULL))
    for (row_index in candidate_rows) {
      candidate <- refreshed[row_index, names(record), drop = FALSE]
      # Rank routing-compatible repairs before applying the change cost.  Do
      # not use a hard veto: the caller performs the authoritative complete
      # evaluation (routing + LEDIT + VAUX) immediately afterwards.
      structural <- 0L
      if (!is.null(oracle)) {
        fast <- .sspice_fast_routing_state(
          candidate, oracle, build_report = FALSE
        )
        structural <- fast$routing_violations + fast$vaux_violations
      }
      changed <- changed_variables(candidate, row_index)
      valid[[length(valid) + 1L]] <<- list(
        record = candidate,
        variables = changed,
        structural = structural,
        cost = .sspice_logical_repair_cost(
          record, candidate, changed, control
        )
      )
    }
    invisible(NULL)
  }

  # First search the complete one-variable neighbourhood.  This is general:
  # the canonical logical-edit evaluator, not a rule-specific inverse, decides
  # whether a value repairs the edit.
  for (variable in inputs) {
    values <- .sspice_logical_candidate_values(
      variable, record, dag, domains, include_missing = TRUE
    )
    if (!length(values) || evaluated >= maximum) next
    batch <- record[rep(1L, length(values)), , drop = FALSE]
    for (index in seq_along(values)) batch[[variable]][index] <- values[[index]]
    evaluate_batch(
      batch,
      function(candidate, row_index) variable
    )
  }

  # Only if no single change works, expand bounded multi-variable
  # neighbourhoods. The canonical evaluator remains completely edit-agnostic;
  # the controls cap both depth and total assignments examined.
  max_changes <- control$logical_repair_max_changes %||% 2L
  if (!length(valid) && max_changes >= 2L && length(inputs) >= 2L &&
      evaluated < maximum) {
    beam <- control$logical_repair_beam_width %||% 10L
    maximum_depth <- min(max_changes, length(inputs))
    for (depth in seq.int(2L, maximum_depth)) {
      groups <- utils::combn(inputs, depth, simplify = FALSE)
      for (group in groups) {
        if (evaluated >= maximum) break
        value_sets <- lapply(group, function(variable) {
          head(.sspice_logical_candidate_values(
            variable, record, dag, domains, include_missing = TRUE
          ), beam)
        })
        if (any(lengths(value_sets) == 0L)) next
        grid <- do.call(expand.grid, lapply(value_sets, seq_along))
        batch <- record[rep(1L, nrow(grid)), , drop = FALSE]
        for (index in seq_len(nrow(grid))) {
          for (variable_index in seq_along(group)) {
            batch[[group[[variable_index]]]][index] <-
              value_sets[[variable_index]][[grid[index, variable_index]]]
          }
        }
        evaluate_batch(
          batch,
          function(candidate, row_index) group
        )
      }
      if (length(valid) || evaluated >= maximum) break
    }
  }
  if (!length(valid)) return(NULL)
  structural <- vapply(valid, `[[`, numeric(1L), "structural")
  costs <- vapply(valid, `[[`, numeric(1L), "cost")
  best <- valid[[order(structural, costs)[[1L]]]]
  changed <- best$variables
  list(
    record = best$record,
    changed_variables = changed,
    old_values = vapply(record[changed], function(x) {
      as.character(x[1L])
    }, character(1L)),
    new_values = vapply(best$record[changed], function(x) {
      as.character(x[1L])
    }, character(1L)),
    move_type = "logical_edit_repair",
    proposal_source = "logical_edit_domain_search",
    predecessors_used = "",
    strata_used = "",
    donor_count = 0L,
    probability = 1
  )
}

.sspice_generate_one_correction_proposal <- function(
    record, context, dag, domains, donor_data, strata,
    imputation_method, priors, fixed_variables, control,
    after_variable = NULL, routing_oracle = NULL,
    vaux_metadata = NULL, logical_edit_metadata = NULL,
    locked_values = list(),
    donor_cache = NULL,
    excluded_modes = list()) {
  if (is.null(routing_oracle)) routing_oracle <- context$routing_oracle
  context$locked_values <- locked_values
  fixed_variables <- intersect(
    unique(as.character(fixed_variables)),
    names(record)
  )
  locked_names <- intersect(names(locked_values), names(record))
  derived_variables <- if (is.null(vaux_metadata) ||
                              !nrow(vaux_metadata)) {
    character()
  } else {
    intersect(
      unique(as.character(vaux_metadata[[1L]])),
      names(record)
    )
  }
  excluded_variables <- unique(c(
    fixed_variables,
    locked_names
  ))

  violated_edits <- context$violated_logical_edits %||% character()
  if (length(violated_edits)) {
    logical_repair <- .sspice_find_logical_edit_repair(
      record = record,
      edit_name = violated_edits[[1L]],
      logical_edit_metadata = logical_edit_metadata,
      context = context,
      dag = dag,
      domains = domains,
      fixed_variables = fixed_variables,
      control = control
    )
    if (!is.null(logical_repair)) return(logical_repair)
  }

  blankable_derived <- unique(c(
    setdiff(derived_variables, fixed_variables),
    locked_names
  ))
  direct_vaux_blank <- intersect(
    context$must_be_missing %||% character(),
    unique(c(
      context$vaux_nodes %||% character(),
      blankable_derived
    ))
  )
  direct_vaux_blank <- setdiff(
    direct_vaux_blank,
    setdiff(fixed_variables, locked_names)
  )
  direct_vaux_blank <- intersect(
    direct_vaux_blank,
    names(record)
  )
  if (length(direct_vaux_blank)) {
    positions <- match(direct_vaux_blank, dag$varnames)
    direct_vaux_blank <- direct_vaux_blank[
      order(positions, na.last = TRUE)
    ]
    for (variable in direct_vaux_blank) {
      blank <- .sspice_blank_proposal(record, variable, "blank")
      if (!is.null(blank)) return(blank)
    }
  }
  
  routing_nodes <- intersect(
    setdiff(context$violated_nodes %||% character(), excluded_variables),
    names(record)
  )
  logical_nodes <- intersect(
    setdiff(context$logical_edit_inputs %||% character(), excluded_variables),
    names(record)
  )
  logical_nodes <- setdiff(logical_nodes, routing_nodes)
  # For an ordinary routing violation the proposal remains localized on the
  # violated variable.  The preceding routing variable only conditions the
  # donor distribution; it is not an alternative correction target.
  candidate_nodes <- unique(c(
    intersect(
      unique(c(
        context$must_be_missing %||% character(),
        context$must_be_present %||% character()
      )),
      locked_names
    ),
    intersect(
      setdiff(
        context$vaux_nodes %||% character(),
        context$vaux_consistency_nodes %||% character()
      ),
      derived_variables
    ),
    routing_nodes,
    logical_nodes
  ))
  unrestorable_locked <- locked_names[vapply(
    locked_values[locked_names],
    function(value) is.na(value[1L]),
    logical(1)
  )]
  candidate_nodes <- setdiff(
    candidate_nodes,
    intersect(
      context$must_be_present %||% character(),
      unrestorable_locked
    )
  )
  if (!length(candidate_nodes)) {
    return(NULL)
  }
  proposal_for_variable <- function(variable) {
    current <- record[[variable]][1L]
    excluded_for_variable <- as.character(
      excluded_modes[[variable]] %||% character()
    )
    if (variable %in% (context$must_be_missing %||% character())) {
      return(.sspice_blank_proposal(record, variable, "blank"))
    }
    if (variable %in% locked_names) {
      locked_value <- locked_values[[variable]][1L]
      if (is.na(locked_value)) return(NULL)
      proposed <- record
      proposed[[variable]] <- .sspice_cast_mode(
        locked_value,
        record[[variable]]
      )
      return(list(
        record = proposed,
        changed_variables = variable,
        old_values = as.character(current),
        new_values = as.character(locked_value),
        move_type = "restore_mode_locked_vaux",
        proposal_source = "mode_locked_vaux_original_mode",
        predecessors_used = "",
        strata_used = "",
        donor_count = 0L,
        probability = 1
      ))
    }
    if (variable %in% logical_nodes && !is.na(current) &&
        .sspice_node_allows_missing(variable, dag)) {
      blank <- .sspice_blank_proposal(
        record = record,
        variables = variable,
        move_type = "logical_blank"
      )
      if (!is.null(blank)) return(blank)
    }
    distribution <- .sspice_candidate_node_modes(
      variable, record, dag, domains, donor_data, strata,
      imputation_method, priors, control, donor_cache = donor_cache
    )
    eligible <- seq_along(distribution$values)
    if (length(excluded_for_variable)) {
      eligible <- eligible[
        !as.character(distribution$values[eligible]) %in%
          excluded_for_variable
      ]
    }
    if (isTRUE(distribution$quantitative)) {
      arithmetic_constraints <- .sspice_arithmetic_constraints(
        variable = variable,
        record = record,
        vaux_metadata = vaux_metadata
      )
      eligible <- eligible[vapply(
        distribution$values[eligible],
        .sspice_numeric_value_satisfies,
        logical(1),
        constraints = arithmetic_constraints
      )]
    } else {
      arithmetic_constraints <- list()
    }
    if (!is.na(current)) {
      eligible <- eligible[
        as.character(distribution$values[eligible]) != as.character(current)
      ]
    }
    if (!length(eligible)) {
      fallback_values <- domains[[variable]]
      if (isTRUE(distribution$quantitative)) {
        fallback_values <- fallback_values[vapply(
          fallback_values,
          .sspice_numeric_value_satisfies,
          logical(1),
          constraints = arithmetic_constraints
        )]
      }
      if (!is.na(current)) {
        fallback_values <- fallback_values[
          as.character(fallback_values) != as.character(current)
        ]
      }
      if (length(excluded_for_variable)) {
        fallback_values <- fallback_values[
          !as.character(fallback_values) %in% excluded_for_variable
        ]
      }
      if (!length(fallback_values)) return(NULL)
      distribution <- list(
        values = fallback_values,
        probabilities = rep(
          1 / length(fallback_values),
          length(fallback_values)
        ),
        source = "routing_domain_fallback",
        predecessors_used = "",
        strata_used = "",
        donor_count = 0L
      )
      if (isTRUE(.sspice_is_quantitative_node(variable, record, dag))) {
        distribution$source <- "numeric_domain_fallback"
        distribution$quantitative <- TRUE
      }
      eligible <- seq_along(distribution$values)
    }
    try_distribution <- function(candidate_distribution, candidate_indices) {
      probabilities <- suppressWarnings(
        as.numeric(candidate_distribution$probabilities[candidate_indices])
      )
      probabilities[!is.finite(probabilities) | probabilities < 0] <- 0
      if (!length(probabilities) || sum(probabilities) <= 0) {
        probabilities <- rep(1, length(candidate_indices))
      }
      probabilities <- probabilities / sum(probabilities)
      remaining <- seq_along(candidate_indices)
      while (length(remaining)) {
        remaining_probabilities <- probabilities[remaining]
        if (!length(remaining_probabilities) ||
            sum(remaining_probabilities) <= 0) {
          remaining_probabilities <- rep(
            1 / length(remaining),
            length(remaining)
          )
        } else {
          remaining_probabilities <-
            remaining_probabilities / sum(remaining_probabilities)
        }
        sampled_position <- sample.int(
          length(remaining),
          size = 1L,
          replace = FALSE,
          prob = remaining_probabilities
        )
        distribution_position <- remaining[[sampled_position]]
        selected <- candidate_indices[[distribution_position]]
        draw_probability <- remaining_probabilities[[sampled_position]]
        value <- .sspice_cast_mode(
          candidate_distribution$values[selected],
          record[[variable]]
        )
        jump_profile <- .sspice_mode_jump_profile(
          variable = variable,
          value = value,
          record = record,
          dag = dag,
          fixed_variables = fixed_variables
        )
        jump_is_usable <-
          !isTRUE(jump_profile$jumps) ||
          (
            jump_profile$filled_share <= control$branch_fill_threshold &&
              !length(jump_profile$immutable_filled)
          )
        if (!jump_is_usable) {
          remaining <- remaining[-sampled_position]
          next
        }

        proposed <- record
        proposed[[variable]] <- value
        blanked_variables <- if (isTRUE(jump_profile$jumps)) {
          jump_profile$filled_variables
        } else {
          character()
        }
        blanked_variables <- setdiff(blanked_variables, variable)
        for (blanked_variable in blanked_variables) {
          proposed[[blanked_variable]] <- NA
        }
        if (variable %in% derived_variables) {
          coherent <- .sspice_vaux_donor_coherence_proposal(
            record = record,
            vaux_var = variable,
            target_mode = value,
            vaux_metadata = vaux_metadata,
            donor_data = donor_data,
            fixed_variables = fixed_variables,
            routing_oracle = routing_oracle,
            control = control
          )
          if (!is.null(coherent)) return(coherent)
        }
        changed_variables <- c(variable, blanked_variables)
        old_values <- vapply(
          record[changed_variables],
          function(column) as.character(column[1L]),
          character(1L)
        )
        new_values <- vapply(
          proposed[changed_variables],
          function(column) as.character(column[1L]),
          character(1L)
        )
        return(list(
          record = proposed,
          changed_variables = changed_variables,
          old_values = old_values,
          new_values = new_values,
          move_type = if (isTRUE(jump_profile$jumps)) {
            "jump_and_blank"
          } else if (is.na(current)) {
            "fill"
          } else {
            "flip"
          },
          proposal_source = candidate_distribution$source,
          predecessors_used = candidate_distribution$predecessors_used,
          strata_used = candidate_distribution$strata_used,
          donor_count = candidate_distribution$donor_count,
          probability = draw_probability
        ))
      }
      NULL
    }

    attempted_values <- as.character(distribution$values[eligible])
    proposal <- try_distribution(distribution, eligible)
    if (!is.null(proposal)) return(proposal)

    # All donor-supported modes may be rejected by their routing
    # consequences.  Stay on the violated variable and try the remaining
    # structural-domain modes before declaring that no proposal exists.
    fallback_values <- domains[[variable]]
    if (isTRUE(distribution$quantitative)) {
      fallback_values <- fallback_values[vapply(
        fallback_values,
        .sspice_numeric_value_satisfies,
        logical(1),
        constraints = arithmetic_constraints
      )]
    }
    if (!is.na(current)) {
      fallback_values <- fallback_values[
        as.character(fallback_values) != as.character(current)
      ]
    }
    if (length(excluded_for_variable)) {
      fallback_values <- fallback_values[
        !as.character(fallback_values) %in% excluded_for_variable
      ]
    }
    fallback_values <- fallback_values[
      !as.character(fallback_values) %in% attempted_values
    ]
    if (!length(fallback_values)) return(NULL)
    fallback_distribution <- list(
      values = fallback_values,
      probabilities = rep(1 / length(fallback_values), length(fallback_values)),
      source = if (isTRUE(.sspice_is_quantitative_node(variable, record, dag))) {
        "numeric_domain_fallback"
      } else {
        "routing_domain_fallback"
      },
      predecessors_used = "",
      strata_used = "",
      donor_count = 0L,
      quantitative = isTRUE(distribution$quantitative)
    )
    try_distribution(
      fallback_distribution,
      seq_along(fallback_distribution$values)
    )
  }

  for (variable in candidate_nodes) {
    proposal <- proposal_for_variable(variable)
    if (!is.null(proposal)) return(proposal)
  }
  NULL
}

.sspice_generate_correction_proposals <- function(
    record, context, dag, domains, donor_data, strata,
    imputation_method, priors, fixed_variables, control,
    donor_cache = NULL) {
  proposals <- vector("list", 0L)
  fixed_variables <- intersect(
    unique(as.character(fixed_variables)),
    names(record)
  )
  ordinary_nodes <- setdiff(
    context$nodes,
    c(context$gates, fixed_variables)
  )
  maximum_nodes <- control$max_nodes_per_iteration %||% 1L
  if (length(ordinary_nodes) > maximum_nodes) {
    ordinary_nodes <- sample(
      ordinary_nodes,
      size = maximum_nodes,
      replace = FALSE
    )
  }
  for (variable in ordinary_nodes) {
    current <- record[[variable]][1L]
    if (!is.na(current)) {
      blank <- .sspice_blank_proposal(record, variable, "blank")
      if (!is.null(blank)) proposals[[length(proposals) + 1L]] <- blank
    }
    proposals <- c(proposals, .sspice_node_mode_proposals(
      record, variable, "node", dag, domains, donor_data, strata,
      imputation_method, priors, control, donor_cache = donor_cache
    ))
  }
  candidate_gates <- setdiff(context$gates, fixed_variables)
  for (gate in candidate_gates) {
    gate_index <- match(gate, dag$varnames)
    segment <- .sspice_jump_segment(gate_index, record, dag)
    filled_share <- if (length(segment)) {
      mean(!vapply(record[segment], function(x) is.na(x[1L]), logical(1)))
    } else 0
    if (filled_share >= control$branch_fill_threshold) {
      proposals <- c(proposals, .sspice_node_mode_proposals(
        record, gate, "gate", dag, domains, donor_data, strata,
        imputation_method, priors, control, donor_cache = donor_cache
      ))
    }
  }
  branch_blank <- .sspice_blank_proposal(
    record,
    setdiff(
      intersect(context$segment, context$nodes),
      fixed_variables
    ),
    "repair_skipped_branch"
  )
  if (!is.null(branch_blank)) proposals[[length(proposals) + 1L]] <- branch_blank
  proposals
}

.sspice_accept_sa_proposal <- function(current_energy, proposed_energy,
                                        temperature) {
  difference <- proposed_energy - current_energy
  difference <= 0 ||
    log(stats::runif(1L)) < -difference / max(temperature, .Machine$double.eps)
}
