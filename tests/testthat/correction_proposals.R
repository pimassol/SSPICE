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

.sspice_generate_one_correction_proposal <- function(
    record, context, dag, domains, donor_data, strata,
    imputation_method, priors, fixed_variables, control,
    after_variable = NULL, routing_oracle = NULL,
    vaux_metadata = NULL, locked_values = list(),
    donor_cache = NULL) {
  if (is.null(routing_oracle)) routing_oracle <- context$routing_oracle
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
    locked_names,
    derived_variables
  ))

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
  
  violated_endogenous_vaux <- intersect(
    context$must_be_present %||% character(),
    derived_variables
  )
  if (length(violated_endogenous_vaux) && !is.null(vaux_metadata)) {
    for (vaux_var in violated_endogenous_vaux) {
      target_modes <- unique(as.character(
        vaux_metadata[
          as.character(vaux_metadata[[1L]]) == vaux_var,
          2L
        ]
      ))
      target_modes <- target_modes[
        !is.na(target_modes) & nzchar(trimws(target_modes))
      ]
      current_mode <- as.character(record[[vaux_var]][1L])
      alt_modes <- setdiff(target_modes, current_mode)
      for (selected_mode in alt_modes) {
        topdown_prop <- .sspice_vaux_topdown_proposal(
          record,
          vaux_var,
          selected_mode,
          vaux_metadata
        )
        if (!is.null(topdown_prop)) return(topdown_prop)
      }
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
  responsible_gate_nodes <- intersect(
    setdiff(
      unique(c(
        context$responsible_gates %||% character(),
        context$inferred_gates %||% character()
      )),
      excluded_variables
    ),
    names(record)
  )
  current_jump_profiles <- stats::setNames(
    lapply(responsible_gate_nodes, function(variable) {
      .sspice_mode_jump_profile(
        variable = variable,
        value = record[[variable]][1L],
        record = record,
        dag = dag,
        fixed_variables = fixed_variables
      )
    }),
    responsible_gate_nodes
  )
  must_be_missing <- context$must_be_missing %||% character()
  structural_gate_nodes <- responsible_gate_nodes[vapply(
    responsible_gate_nodes,
    function(variable) {
      profile <- current_jump_profiles[[variable]]
      isTRUE(profile$jumps) &&
        length(intersect(profile$segment_variables, must_be_missing)) > 0L
    },
    logical(1L)
  )]

  # If the current routing mode skips a segment that is at most half filled,
  # preserving the jump and blanking the filled part is the minimum-loss move.
  if (length(structural_gate_nodes)) {
    for (variable in structural_gate_nodes) {
      profile <- current_jump_profiles[[variable]]
      jump_is_usable <-
        profile$filled_share <= control$branch_fill_threshold &&
        !length(profile$immutable_filled)
      if (jump_is_usable) {
        blank <- .sspice_blank_proposal(
          record = record,
          variables = profile$filled_variables,
          move_type = "repair_skipped_branch"
        )
        if (!is.null(blank)) return(blank)
      }
    }
  }

  candidate_nodes <- unique(c(
    structural_gate_nodes,
    intersect(
      unique(c(
        context$must_be_missing %||% character(),
        context$must_be_present %||% character()
      )),
      locked_names
    ),
    routing_nodes,
    logical_nodes,
    setdiff(responsible_gate_nodes, structural_gate_nodes)
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
    probabilities <- suppressWarnings(
      as.numeric(distribution$probabilities[eligible])
    )
    probabilities[!is.finite(probabilities) | probabilities < 0] <- 0
    if (!length(probabilities) || sum(probabilities) <= 0) {
      probabilities <- rep(1, length(eligible))
    }
    probabilities <- probabilities / sum(probabilities)
    remaining <- seq_along(eligible)
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
      selected <- eligible[[distribution_position]]
      draw_probability <- remaining_probabilities[[sampled_position]]
      value <- .sspice_cast_mode(
        distribution$values[selected],
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
        proposal_source = distribution$source,
        predecessors_used = distribution$predecessors_used,
        strata_used = distribution$strata_used,
        donor_count = distribution$donor_count,
        probability = draw_probability
      ))
    }
    NULL
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
