.sspice_change_distance <- function(record, original, fixed_variables, control) {
  variables <- setdiff(names(record), fixed_variables)
  current <- record[1L, variables, drop = FALSE]
  initial <- original[1L, variables, drop = FALSE]
  current_na <- vapply(current, is.na, logical(1))
  initial_na <- vapply(initial, is.na, logical(1))
  changed <- vapply(variables, function(variable) {
    if (current_na[[variable]] || initial_na[[variable]]) return(FALSE)
    !identical(as.character(current[[variable]]), as.character(initial[[variable]]))
  }, logical(1))
  sum(!initial_na & current_na) * control$penalty_blank +
    sum(initial_na & !current_na) * control$penalty_fill +
    sum(changed) * control$penalty_flip
}

.sspice_build_routing_oracle <- function(
    dag, record_id, vaux_names = character(), items_mapping_file = NULL) {
  routes <- as.matrix(dag$routes_numeric)
  variable_names <- as.character(dag$varnames)
  declared_number_items <- suppressWarnings(as.integer(routes[, 2L]))
  number_items <- abs(declared_number_items)
  number_items[is.na(number_items) | number_items < 1L] <- 1L
  if (length(number_items) != length(variable_names)) {
    stop("Routing rows and DAG variables are not aligned.", call. = FALSE)
  }
  expected_rows <- sum(number_items) - number_items[length(number_items)]
  if (nrow(dag$Bt) != expected_rows) {
    stop("Bt rows are incompatible with the routing domains.", call. = FALSE)
  }
  terminal_rows <- matrix(
    0,
    nrow = number_items[length(number_items)],
    ncol = length(variable_names)
  )
  terminal_rows[, length(variable_names)] <- 1
  incidence <- rbind(dag$Bt, terminal_rows)
  edge_start <- cumsum(c(1L, head(number_items, -1L)))
  edge_end <- edge_start + number_items - 1L
  edge_sources <- vapply(
    seq_len(nrow(incidence)),
    function(edge) {
      indices <- which(incidence[edge, ] == 1)
      if (length(indices)) indices[[1L]] else NA_integer_
    },
    integer(1L)
  )
  edge_targets <- lapply(
    seq_len(nrow(incidence)),
    function(edge) which(incidence[edge, ] == -1)
  )
  edge_last_targets <- vapply(
    edge_targets,
    function(indices) {
      if (length(indices)) indices[[length(indices)]] else NA_integer_
    },
    integer(1L)
  )
  edge_spans <- Map(
    function(source, target) {
      if (is.na(source) || is.na(target) || source > target) {
        integer()
      } else {
        seq.int(source, target)
      }
    },
    edge_sources,
    edge_last_targets
  )
  variable_type <- suppressWarnings(as.numeric(routes[, 5L]))
  group_sizes <- vapply(
    seq_along(variable_names),
    function(index) as.integer(if_is_a_group(index, variable_type)),
    integer(1L)
  )
  minimum <- suppressWarnings(as.numeric(routes[, 3L]))
  item_mapping <- .sspice_read_item_mapping(items_mapping_file)
  .sspice_validate_item_mapping(
    item_mapping, variable_names, number_items, minimum,
    suppressWarnings(as.numeric(routes[, 4L]))
  )
  list(
    incidence = incidence,
    B = t(incidence),
    declared_number_items = declared_number_items,
    number_items = number_items,
    minimum = minimum,
    maximum = suppressWarnings(as.numeric(routes[, 4L])),
    variable_type = variable_type,
    edge_start = edge_start,
    edge_end = edge_end,
    edge_sources = edge_sources,
    edge_targets = edge_targets,
    edge_last_targets = edge_last_targets,
    edge_spans = edge_spans,
    group_sizes = group_sizes,
    string_indices = which(
      !is.na(declared_number_items) & declared_number_items == 0L
    ),
    required_variables = !is.na(minimum),
    variable_names = variable_names,
    record_id = record_id,
    record_id_index = match(record_id, variable_names),
    logical_edit_names = dag$logical_edit_names %||% character(),
    vaux_names = unique(as.character(vaux_names)),
    item_mapping = item_mapping
  )
}

.sspice_oracle_dummy <- function(record, oracle) {
  values <- as.list(record[1L, oracle$variable_names, drop = FALSE])
  # Oracles built before routing-aware string handling, including compact
  # test fixtures, retain the original package dummy encoding.
  if (is.null(oracle$declared_number_items) ||
      is.null(oracle$incidence)) {
    return(dummize_vector(values, oracle$number_items))
  }

  declared_number_items <- suppressWarnings(
    as.integer(oracle$declared_number_items)
  )
  number_items <- as.integer(oracle$number_items)
  number_variables <- length(oracle$variable_names)
  if (length(declared_number_items) != number_variables ||
      length(number_items) != number_variables) {
    stop("Routing oracle item metadata are not aligned.", call. = FALSE)
  }

  value_present <- !vapply(
    values,
    function(value) is.na(value[[1L]]),
    logical(1)
  )
  encoding_values <- .sspice_routing_positions(
    values, number_items, oracle$minimum, oracle$maximum, oracle$item_mapping
  )
  string_indices <- oracle$string_indices %||% which(
    !is.na(declared_number_items) & declared_number_items == 0L
  )
  for (variable_index in string_indices) {
    # String presence is encoded below without numeric coercion.
    encoding_values[[variable_index]] <- NA_real_
  }
  observed_dummy <- dummize_vector(
    encoding_values,
    oracle$number_items
  )

  edge_start <- oracle$edge_start %||%
    cumsum(c(1L, head(number_items, -1L)))
  edge_end <- oracle$edge_end %||%
    (edge_start + number_items - 1L)
  reached <- rep(FALSE, number_variables)
  reached[1L] <- TRUE
  routing_dummy <- observed_dummy

  for (variable_index in seq_len(number_variables)) {
    first_edge <- edge_start[[variable_index]]
    last_edge <- edge_end[[variable_index]]
    variable_edges <- seq.int(first_edge, last_edge)

    # A routing-reached string field (declared nitems == 0) has one neutral
    # outgoing edge even when its stored value is NA.  This changes only the
    # routing representation; the record value remains untouched.
    if (!is.na(declared_number_items[[variable_index]]) &&
        declared_number_items[[variable_index]] == 0L) {
      optional_string <- is.na(oracle$minimum[[variable_index]])
      routing_dummy[first_edge] <- as.integer(
        isTRUE(value_present[[variable_index]]) ||
          (
            isTRUE(reached[[variable_index]]) &&
              isTRUE(optional_string)
          )
      )
    }

    # Only edges emitted by a reached node propagate reachability.  A value
    # found in a skipped node remains encoded, so it can be reported as an
    # illegal filled value, but it cannot activate downstream nodes.
    if (!isTRUE(reached[[variable_index]])) next
    active_edges <- variable_edges[routing_dummy[variable_edges] == 1L]
    if (!length(active_edges)) next
    for (edge in active_edges) {
      target_nodes <- if (!is.null(oracle$edge_targets)) {
        oracle$edge_targets[[edge]]
      } else {
        which(oracle$incidence[edge, ] == -1)
      }
      if (length(target_nodes)) reached[target_nodes] <- TRUE
    }
  }

  routing_dummy
}

.sspice_fast_routing_state <- function(record, oracle, build_report = TRUE) {
  dummy <- .sspice_oracle_dummy(record, oracle)
  active_rows <- which(dummy == 1L)
  number_variables <- length(oracle$variable_names)
  variable_type <- oracle$variable_type %||% rep(0, number_variables)
  present <- !vapply(
    record[oracle$variable_names],
    function(value) is.na(value[1L]),
    logical(1)
  )
  activation <- numeric(number_variables)
  identifier <- record[[oracle$record_id]][1L]

  report_rows <- list()
  report_variables <- integer()
  report_sources <- character()
  add_report <- function(variable_index, route_index, error_code) {
    if (is.na(variable_index) || variable_index == oracle$record_id_index) {
      return(invisible(NULL))
    }
    activation[variable_index] <<- if (error_code > 0L) 1 else -1
    label <- oracle$variable_names[variable_index]
    source <- if (label %in% oracle$logical_edit_names) {
      "LOGICAL_EDIT"
    } else if (label %in% oracle$vaux_names) {
      "VAUX"
    } else {
      "ROUTING"
    }
    report_variables <<- c(report_variables, variable_index)
    report_sources <<- c(report_sources, source)
    if (!isTRUE(build_report)) {
      return(invisible(NULL))
    }
    report_rows[[length(report_rows) + 1L]] <<- data.frame(
      identifier = identifier,
      errcode = as.integer(error_code),
      varname = as.integer(variable_index),
      valname = as.character(record[[variable_index]][1L]),
      routevar = as.integer(route_index),
      routeval = if (is.na(route_index)) {
        NA_character_
      } else {
        as.character(record[[route_index]][1L])
      },
      varname_label = label,
      routevar_label = if (is.na(route_index)) {
        NA_character_
      } else {
        oracle$variable_names[route_index]
      },
      violation_source = source,
      stringsAsFactors = FALSE
    )
  }

  # check_all_paths() deliberately ignores the final active dummy edge,
  # which belongs to the terminal variable.  Reproduce the same rule here.
  path_edges <- if (length(active_rows) > 1L) {
    active_rows[seq_len(length(active_rows) - 1L)]
  } else {
    integer()
  }

  for (edge in path_edges) {
    source_node <- if (!is.null(oracle$edge_sources)) {
      oracle$edge_sources[[edge]]
    } else {
      source_nodes <- which(oracle$incidence[edge, ] == 1)
      if (length(source_nodes)) source_nodes[[1L]] else NA_integer_
    }
    target_node <- if (!is.null(oracle$edge_last_targets)) {
      oracle$edge_last_targets[[edge]]
    } else {
      target_nodes <- which(oracle$incidence[edge, ] == -1)
      if (length(target_nodes)) {
        target_nodes[[length(target_nodes)]]
      } else {
        NA_integer_
      }
    }
    if (is.na(source_node) || is.na(target_node) ||
        source_node > target_node) next
    incidence_row <- oracle$incidence[edge, , drop = TRUE]
    variables_on_edge <- if (!is.null(oracle$edge_spans)) {
      oracle$edge_spans[[edge]]
    } else {
      seq.int(source_node, target_node)
    }

    pending_group <- integer()
    for (variable_index in variables_on_edge) {
      difference <- as.integer(present[variable_index]) -
        abs(incidence_row[variable_index])
      if (difference > 0L) {
        add_report(variable_index, source_node, 1L)
      } else if (difference < 0L &&
                 !is.na(oracle$minimum[variable_index])) {
        group_size <- if (!is.null(oracle$group_sizes)) {
          oracle$group_sizes[[variable_index]]
        } else {
          if_is_a_group(variable_index, variable_type)
        }
        if (group_size > 1L) {
          pending_group <- c(pending_group, variable_index)
          if (length(pending_group) == group_size) {
            for (group_variable in pending_group) {
              add_report(group_variable, source_node, -2L)
            }
          }
        } else {
          add_report(variable_index, source_node, -1L)
        }
      }
    }
  }

  report <- if (isTRUE(build_report) && length(report_rows)) {
    do.call(rbind, report_rows)
  } else if (isTRUE(build_report)) {
    data.frame(
      identifier = record[[oracle$record_id]][FALSE],
      errcode = integer(),
      varname = integer(),
      valname = character(),
      routevar = integer(),
      routeval = character(),
      varname_label = character(),
      routevar_label = character(),
      violation_source = character(),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  # routing_check() performs an exhaustive logical-edit scan and gives that
  # result precedence over a path violation on the same derived variable.
  logical_missing <- intersect(
    oracle$logical_edit_names,
    oracle$variable_names[
      !present
    ]
  )
  if (length(logical_missing)) {
    logical_indices <- match(logical_missing, oracle$variable_names)
    keep_report <- !(report_variables %in% logical_indices)
    report_variables <- report_variables[keep_report]
    report_sources <- report_sources[keep_report]
    if (isTRUE(build_report)) {
      report <- report[
        !(report$varname_label %in% logical_missing),
        ,
        drop = FALSE
      ]
    }
    for (label in logical_missing) {
      variable_index <- match(label, oracle$variable_names)
      activation[variable_index] <- -1
      report_variables <- c(report_variables, variable_index)
      report_sources <- c(report_sources, "LOGICAL_EDIT")
      if (isTRUE(build_report)) {
        report <- rbind(
          report,
          data.frame(
            identifier = identifier,
            errcode = NA_integer_,
            varname = variable_index,
            valname = NA_character_,
            routevar = NA_integer_,
            routeval = NA_character_,
            varname_label = label,
            routevar_label = NA_character_,
            violation_source = "LOGICAL_EDIT",
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  if (!is.na(oracle$record_id_index)) {
    activation[oracle$record_id_index] <- 0
    keep_report <- report_variables != oracle$record_id_index
    report_variables <- report_variables[keep_report]
    report_sources <- report_sources[keep_report]
    if (isTRUE(build_report)) {
      report <- report[
        report$varname != oracle$record_id_index,
        ,
        drop = FALSE
      ]
    }
  }
  logical <- report_sources == "LOGICAL_EDIT"
  vaux <- report_sources == "VAUX"
  if (!isTRUE(build_report)) {
    return(list(
      routing_violations = sum(!logical & !vaux),
      logical_edit_violations = sum(logical),
      vaux_violations = sum(vaux),
      violation_variables = report_variables,
      violation_sources = report_sources,
      activation = activation
    ))
  }
  row.names(report) <- NULL
  names(report)[1L] <- oracle$record_id
  list(
    routing_violations = sum(!logical & !vaux),
    logical_edit_violations = sum(logical),
    vaux_violations = sum(vaux),
    violation_variables = report_variables,
    violation_sources = report_sources,
    dummy = dummy,
    activation = activation,
    full_report = report
  )
}

.sspice_fast_constraint_score <- function(record, oracle) {
  state <- .sspice_fast_routing_state(
    record = record,
    oracle = oracle,
    build_report = FALSE
  )
  as.numeric(
    state$routing_violations +
      state$logical_edit_violations +
      state$vaux_violations
  )
}

.sspice_refresh_derived_variables <- function(
    record, vaux_metadata, logical_edit_metadata, external_data_dir,
    routing_variable_names, endogenous_vaux, frozen_values,
    changed_variables = NULL, dependency_index = NULL,
    suppress_frozen = character(),
    defer_missing_endogenous_vaux = FALSE) {
  refreshed <- record
  if (is.null(dependency_index)) {
    dependency_index <- .sspice_build_derived_dependency_index(
      vaux_metadata,
      logical_edit_metadata,
      endogenous_vaux
    )
  }
  affected <- if (is.null(changed_variables)) {
    list(
      vaux = names(dependency_index$vaux),
      logical_edits = names(dependency_index$logical_edits)
    )
  } else {
    .sspice_affected_derived_variables(changed_variables, dependency_index)
  }
  # Recompute an endogenous VAUX when one of its base variables changes.
  # A VAUX changed directly by the current proposal is intentionally preserved:
  # its consistency with the bases is evaluated separately and can drive a
  # subsequent correction of either the VAUX or its inputs.
  directly_changed_vaux <- intersect(
    unique(as.character(changed_variables %||% character())),
    endogenous_vaux
  )
  vaux_to_refresh <- setdiff(
    intersect(affected$vaux, endogenous_vaux),
    directly_changed_vaux
  )
  # During local forward correction, an endogenous VAUX which is still
  # missing represents a future node that has not yet been proposed.  Do not
  # manufacture its value merely because one of its inputs has just changed:
  # the VAUX will be handled when localization reaches it.  Already-valued
  # VAUX are still refreshed, so accepted changes cannot leave them stale.
  if (isTRUE(defer_missing_endogenous_vaux) && length(vaux_to_refresh)) {
    vaux_to_refresh <- vaux_to_refresh[vapply(
      vaux_to_refresh,
      function(variable) {
        variable %in% names(refreshed) &&
          !is.na(refreshed[[variable]][1L])
      },
      logical(1L)
    )]
  }
  if (!is.null(vaux_metadata) && length(vaux_to_refresh)) {
    pending_vaux <- vaux_to_refresh

    while (length(pending_vaux)) {
      ready_vaux <- pending_vaux[vapply(
        pending_vaux,
        function(variable) {
          inputs <- dependency_index$vaux[[variable]] %||% character()
          !any(inputs %in% pending_vaux)
        },
        logical(1L)
      )]

      if (!length(ready_vaux)) {
        stop(
          "Cyclic dependency among endogenous VAUX variables: ",
          paste(pending_vaux, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      for (variable in ready_vaux) {
        rows <- as.character(vaux_metadata[[1L]]) == variable
        base <- refreshed[, setdiff(names(refreshed), variable), drop = FALSE]
        recomputed <- create_vaux_variables(
          inpdata = base,
          vauxs_data = vaux_metadata[rows, , drop = FALSE],
          extDFpath = external_data_dir,
          routing_vnames = unique(c(routing_variable_names, variable))
        )

        if (!(variable %in% names(recomputed))) {
          stop(
            "Unable to recompute VAUX variable: ",
            variable,
            ".",
            call. = FALSE
          )
        }

        refreshed[[variable]] <- recomputed[[variable]]
      }

      pending_vaux <- setdiff(pending_vaux, ready_vaux)
    }
  }

  edits_to_refresh <- affected$logical_edits
  if (!is.null(logical_edit_metadata) && length(edits_to_refresh)) {
    rows <- as.character(logical_edit_metadata[[1L]]) %in% edits_to_refresh
    base <- refreshed[, setdiff(names(refreshed), edits_to_refresh), drop = FALSE]
    recomputed_edits <- create_logical_edit_variables(
      inpdata = base,
      logical_edits_data = logical_edit_metadata[rows, , drop = FALSE]
    )
    missing_edits <- setdiff(edits_to_refresh, names(recomputed_edits))
    if (length(missing_edits)) {
      stop(
        "Unable to recompute logical-edit variables: ",
        paste(missing_edits, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    for (variable in edits_to_refresh) {
      refreshed[[variable]] <- recomputed_edits[[variable]]
    }
  }
  if (length(frozen_values)) {
    frozen_target <- setdiff(names(frozen_values), suppress_frozen)
    for (variable in intersect(frozen_target, names(refreshed))) {
      refreshed[[variable]] <- frozen_values[[variable]]
    }
  }
  refreshed
}

.sspice_vaux_consistency_state <- function(
    record, vaux_metadata, external_data_dir, routing_variable_names,
    endogenous_vaux, routing_state, build_report = TRUE,
    candidates = NULL) {
  empty <- list(count = 0L, variables = integer(), report = NULL)
  if (is.null(vaux_metadata) || !nrow(vaux_metadata) ||
      !length(endogenous_vaux)) return(empty)

  routing_variables <- routing_state$violation_variables %||% integer()
  routing_sources <- routing_state$violation_sources %||% character()
  routing_vaux <- routing_variables[routing_sources == "VAUX"]
  names_record <- names(record)
  candidates <- intersect(candidates %||% endogenous_vaux, names_record)
  candidates <- candidates[vapply(
    candidates,
    function(variable) {
      index <- match(variable, names_record)
      !is.na(record[[variable]][1L]) && !index %in% routing_vaux
    },
    logical(1L)
  )]
  if (!length(candidates)) return(empty)

  rows <- as.character(vaux_metadata[[1L]]) %in% candidates
  base <- record[, setdiff(names_record, candidates), drop = FALSE]
  expected <- create_vaux_variables(
    inpdata = base,
    vauxs_data = vaux_metadata[rows, , drop = FALSE],
    extDFpath = external_data_dir,
    routing_vnames = routing_variable_names
  )
  available <- intersect(candidates, names(expected))
  inconsistent <- available[vapply(
    available,
    function(variable) {
      observed <- record[[variable]][1L]
      target <- expected[[variable]][1L]
      is.na(target) || as.character(observed) != as.character(target)
    },
    logical(1L)
  )]
  missing_expected <- setdiff(candidates, available)
  inconsistent <- unique(c(inconsistent, missing_expected))
  if (!length(inconsistent)) return(empty)

  indices <- match(inconsistent, names_record)
  report <- NULL
  if (isTRUE(build_report)) {
    identifier <- record[[routing_state$record_id %||% names_record[[1L]]]][1L]
    report <- data.frame(
      identifier = rep(identifier, length(inconsistent)),
      errcode = rep(-3L, length(inconsistent)),
      varname = indices,
      valname = vapply(inconsistent, function(x) {
        as.character(record[[x]][1L])
      }, character(1L)),
      routevar = NA_integer_,
      routeval = NA_character_,
      varname_label = inconsistent,
      routevar_label = NA_character_,
      violation_source = rep("VAUX", length(inconsistent)),
      stringsAsFactors = FALSE
    )
    names(report)[1L] <- routing_state$record_id %||% names_record[[1L]]
  }
  list(count = length(inconsistent), variables = indices, report = report)
}

.sspice_evaluate_record_state <- function(record, original, dag, record_id,
                                          fixed_variables, control,
                                          vaux_inconsistencies = 0L,
                                          items_mapping_file = NULL,
                                          routing_oracle = NULL,
                                          vaux_names = character(),
                                          vaux_metadata = NULL,
                                          external_data_dir = ".",
                                          endogenous_vaux = character(),
                                          build_report = TRUE,
                                          previous_state = NULL,
                                          changed_variables = NULL) {
  if (!length(vaux_names)) {
    vaux_names <- if (!is.null(routing_oracle)) {
      routing_oracle$vaux_names %||% character()
    } else {
      dag$vaux_names %||% character()
    }
  }
  mapping_name <- if (is.null(items_mapping_file)) NULL else
    basename(items_mapping_file)
  mapping_path <- if (is.null(items_mapping_file)) NULL else
    dirname(items_mapping_file)
  if (!is.null(routing_oracle)) {
    fast <- .sspice_fast_routing_state(
      record, routing_oracle, build_report = build_report
    )
    fast$record_id <- record_id
    affected_vaux <- endogenous_vaux
    retained_consistency <- integer()
    if (!is.null(previous_state) && !is.null(changed_variables)) {
      affected <- .sspice_affected_derived_variables(
        changed_variables,
        routing_oracle$dependency_index
      )
      affected_vaux <- unique(c(
        intersect(changed_variables, endogenous_vaux),
        intersect(affected$vaux, endogenous_vaux)
      ))
      previous_consistency <-
        previous_state$routing_check$consistency_variables %||% integer()
      retained_consistency <- setdiff(
        previous_consistency,
        match(affected_vaux, names(record))
      )
      routing_vaux_variables <-
        fast$violation_variables[
          (fast$violation_sources %||% character()) == "VAUX"
        ]
      retained_consistency <- setdiff(
        retained_consistency,
        routing_vaux_variables
      )
    }
    consistency <- .sspice_vaux_consistency_state(
      record = record,
      vaux_metadata = vaux_metadata,
      external_data_dir = external_data_dir,
      routing_variable_names = names(record),
      endogenous_vaux = endogenous_vaux,
      routing_state = fast,
      build_report = build_report,
      candidates = affected_vaux
    )
    consistency_variables <- unique(c(
      retained_consistency,
      consistency$variables
    ))
    full_report <- fast$full_report %||% NULL
    if (isTRUE(build_report) && !is.null(consistency$report)) {
      full_report <- rbind(full_report, consistency$report)
    }
    violation_variables <- c(
      fast$violation_variables %||% integer(),
      consistency_variables
    )
    violation_sources <- c(
      fast$violation_sources %||% character(),
      rep("VAUX", length(consistency_variables))
    )
    checked <- list(
      full_report = full_report,
      activation = fast$activation,
      dummy = fast$dummy,
      dependency_index = routing_oracle$dependency_index,
      routing_oracle = routing_oracle,
      violation_variables = violation_variables,
      violation_sources = violation_sources,
      consistency_variables = consistency_variables
    )
    routing_count <- fast$routing_violations
    logical_count <- fast$logical_edit_violations
    vaux_inconsistencies <-
      fast$vaux_violations + length(consistency_variables)
  } else {
    checked <- routing_check(
      dset_data = record,
      routes = dag$routes_numeric,
      Bt = dag$Bt,
      varkey = record_id,
      items.maps = mapping_name,
      path.items.maps = mapping_path,
      logical_edit_names = dag$logical_edit_names
    )
    report <- checked$full_report
    if (is.null(report)) report <- data.frame()
    sources <- if (nrow(report) && "violation_source" %in% names(report)) {
      as.character(report$violation_source)
    } else rep("ROUTING", nrow(report))
    labels <- if (nrow(report) && "varname_label" %in% names(report)) {
      as.character(report$varname_label)
    } else character(nrow(report))
    vaux_rows <- labels %in% unique(as.character(vaux_names))
    routing_count <- sum(sources != "LOGICAL_EDIT" & !vaux_rows)
    logical_count <- sum(sources == "LOGICAL_EDIT")
    vaux_inconsistencies <- sum(vaux_rows & sources != "LOGICAL_EDIT")
  }
  change_cost <- .sspice_change_distance(record, original, fixed_variables, control)
  total <- control$routing_weight * routing_count +
    control$logical_edit_weight * logical_count +
    control$vaux_weight * vaux_inconsistencies +
    control$lambda_change * change_cost
  list(
    routing_violations = routing_count,
    logical_edit_violations = logical_count,
    vaux_violations = as.integer(vaux_inconsistencies),
    change_cost = change_cost,
    total_energy = total,
    routing_check = checked
  )
}

.sspice_future_cost <- function(current_state, proposed_state) {
  max(
    0,
    proposed_state$routing_violations +
      proposed_state$logical_edit_violations +
      (proposed_state$vaux_violations %||% 0L) -
      current_state$routing_violations -
      current_state$logical_edit_violations -
      (current_state$vaux_violations %||% 0L)
  )
}

.sspice_build_derived_dependency_index <- function(
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    endogenous_vaux = character()) {
  vaux_inputs <- list()
  if (!is.null(vaux_metadata) && nrow(vaux_metadata)) {
    vaux_names <- unique(as.character(vaux_metadata[[1L]]))
    vaux_names <- intersect(vaux_names, endogenous_vaux)
    vaux_inputs <- stats::setNames(
      lapply(vaux_names, function(variable) {
        rows <- as.character(vaux_metadata[[1L]]) == variable
        inputs <- trimws(as.character(vaux_metadata[[3L]][rows]))
        unique(inputs[!is.na(inputs) & nzchar(inputs) & inputs != "."])
      }),
      vaux_names
    )
  }
  edit_inputs <- list()
  if (!is.null(logical_edit_metadata) && nrow(logical_edit_metadata)) {
    edit_names <- unique(as.character(logical_edit_metadata[[1L]]))
    edit_inputs <- stats::setNames(
      lapply(edit_names, function(variable) {
        rows <- as.character(logical_edit_metadata[[1L]]) == variable
        inputs <- trimws(as.character(logical_edit_metadata[[3L]][rows]))
        unique(inputs[!is.na(inputs) & nzchar(inputs) & inputs != "."])
      }),
      edit_names
    )
  }
  list(vaux = vaux_inputs, logical_edits = edit_inputs)
}

.sspice_classify_vaux <- function(
    dependency_index, data_names, endogenous_vaux, external_vaux,
    logical_group_vaux = character()) {
  endogenous_vaux <- unique(as.character(endogenous_vaux))
  logical_group_vaux <- intersect(
    unique(as.character(logical_group_vaux)),
    endogenous_vaux
  )
  ordinary_endogenous_vaux <- setdiff(endogenous_vaux, logical_group_vaux)
  inputs <- dependency_index$vaux[endogenous_vaux]
  recomputable <- ordinary_endogenous_vaux[vapply(
    ordinary_endogenous_vaux,
    function(variable) {
      current_inputs <- inputs[[variable]] %||% character()
      length(current_inputs) > 0L && all(current_inputs %in% data_names)
    },
    logical(1)
  )]
  frozen_by_canonicalization <- setdiff(
    ordinary_endogenous_vaux,
    recomputable
  )
  missing_inputs <- stats::setNames(
    lapply(frozen_by_canonicalization, function(variable) {
      setdiff(inputs[[variable]] %||% character(), data_names)
    }),
    frozen_by_canonicalization
  )
  list(
    recomputable = recomputable,
    logical_groups = logical_group_vaux,
    external = unique(as.character(external_vaux)),
    frozen_by_canonicalization = frozen_by_canonicalization,
    frozen = unique(c(
      external_vaux,
      logical_group_vaux,
      frozen_by_canonicalization
    )),
    missing_inputs = missing_inputs
  )
}

.sspice_affected_derived_variables <- function(changed_variables, dependency_index) {
  changed <- unique(as.character(changed_variables))
  affected_vaux <- character()
  repeat {
    available <- setdiff(names(dependency_index$vaux), affected_vaux)
    newly_affected <- available[vapply(
      dependency_index$vaux[available],
      function(inputs) any(inputs %in% changed),
      logical(1)
    )]
    if (!length(newly_affected)) break
    affected_vaux <- c(affected_vaux, newly_affected)
    changed <- unique(c(changed, newly_affected))
  }
  affected_edits <- names(dependency_index$logical_edits)[vapply(
    dependency_index$logical_edits,
    function(inputs) any(inputs %in% changed),
    logical(1)
  )]
  list(vaux = affected_vaux, logical_edits = affected_edits)
}
