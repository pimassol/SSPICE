# Internal helpers for locating the correction neighbourhood in DAG+.

.sspice_bt_row_metadata <- function(Bt, routes, variable_names) {
  n_items <- abs(suppressWarnings(as.integer(routes[, 2L])))
  n_items[is.na(n_items) | n_items < 1L] <- 1L
  owner <- rep(seq_along(variable_names), n_items)
  if (length(owner) < nrow(Bt)) {
    owner <- c(owner, rep(length(variable_names), nrow(Bt) - length(owner)))
  }
  owner <- owner[seq_len(nrow(Bt))]
  item_index <- unlist(lapply(n_items, seq_len), use.names = FALSE)
  if (length(item_index) < nrow(Bt)) {
    item_index <- c(item_index, rep(1L, nrow(Bt) - length(item_index)))
  }
  data.frame(
    row = seq_len(nrow(Bt)),
    predecessor_index = owner,
    predecessor = variable_names[owner],
    item_index = item_index[seq_len(nrow(Bt))],
    n_items = n_items[owner],
    minimum = suppressWarnings(as.numeric(routes[owner, 3L])),
    maximum = suppressWarnings(as.numeric(routes[owner, 4L])),
    stringsAsFactors = FALSE
  )
}

.sspice_structural_predecessors <- function(variable, record, dag,
                                             active_only = TRUE) {
  target <- match(variable, dag$varnames)
  if (is.na(target)) return(character())
  rows <- which(dag$Bt[, target] == -1)
  if (!length(rows)) return(character())
  metadata <- .sspice_bt_row_metadata(
    dag$Bt, dag$routes_numeric, dag$varnames
  )[rows, , drop = FALSE]
  if (active_only) {
    active <- vapply(seq_len(nrow(metadata)), function(index) {
      predecessor <- metadata$predecessor[index]
      observed <- record[[predecessor]][1L]
      if (is.na(observed)) return(FALSE)
      observed <- suppressWarnings(
        as.numeric(
          as.character(observed)
        )
      )
      if (is.na(observed)) return(FALSE)
      scaled <- item_scaling(
        observed,
        metadata$n_items[index],
        metadata$minimum[index],
        metadata$maximum[index]
      )
      !is.na(scaled) && scaled == metadata$item_index[index]
    }, logical(1))
    metadata <- metadata[active, , drop = FALSE]
  }
  unique(metadata$predecessor)
}

.sspice_gate_target <- function(gate, value, routes) {
  if (length(gate) != 1L || is.na(gate) || gate < 1L) {
    return(NA_integer_)
  }
  gate <- as.integer(gate)
  default_target <- gate + 1L
  if (length(value) != 1L || is.na(value)) {
    return(default_target)
  }
  route_dimensions <- dim(routes)
  if (is.null(route_dimensions) ||
      length(route_dimensions) != 2L ||
      gate > route_dimensions[[1L]] ||
      route_dimensions[[2L]] < 6L) {
    return(default_target)
  }
  number_routes <- suppressWarnings(as.integer(routes[gate, 6L]))
  if (length(number_routes) != 1L ||
      is.na(number_routes) ||
      number_routes <= 0L) {
    return(default_target)
  }
  if (number_routes == 98L) number_routes <- 1L
  for (index in seq_len(number_routes)) {
    item_column <- 2L * index + 5L
    target_column <- 2L * index + 6L
    if (target_column > ncol(routes)) next
    item <- routes[gate, item_column]
    target <- routes[gate, target_column]
    if (isTRUE(
      !is.na(item) &&
        !is.na(target) &&
        identical(as.character(item), as.character(value))
    )) {
      return(as.integer(target))
    }
  }
  default_target
}

.sspice_find_responsible_gate <- function(node, record, dag) {
  variable <- dag$varnames[node]
  predecessors <- .sspice_structural_predecessors(
    variable, record, dag, active_only = FALSE
  )
  indices <- match(predecessors, dag$varnames)
  indices <- indices[!is.na(indices) & indices < node]
  if (!length(indices)) return(NA_integer_)
  for (candidate in rev(sort(indices))) {
    value <- record[[candidate]][1L]
    if (is.na(value)) next
    target <- .sspice_gate_target(candidate, value, dag$routes_numeric)
    if (!is.na(target) && target > node) return(candidate)
  }
  NA_integer_
}

.sspice_jump_segment <- function(gate, record, dag, value = NULL) {
  if (is.null(value)) value <- record[[gate]][1L]
  if (is.na(value)) return(integer())
  target <- .sspice_gate_target(gate, value, dag$routes_numeric)
  if (is.na(target) || target <= gate + 1L || target > ncol(record)) {
    return(integer())
  }
  seq.int(gate + 1L, target - 1L)
}

.sspice_mode_jump_profile <- function(
    variable, value, record, dag, fixed_variables = character()) {
  variable_index <- match(variable, dag$varnames)
  if (is.na(variable_index)) {
    return(list(
      target = NA_integer_,
      segment_indices = integer(),
      segment_variables = character(),
      filled_variables = character(),
      immutable_filled = character(),
      filled_share = 0,
      jumps = FALSE
    ))
  }
  target <- .sspice_gate_target(
    variable_index,
    value,
    dag$routes_numeric
  )
  segment_indices <- .sspice_jump_segment(
    variable_index,
    record,
    dag,
    value = value
  )
  segment_variables <- dag$varnames[segment_indices]
  filled_variables <- segment_variables[vapply(
    record[segment_variables],
    function(column) !is.na(column[1L]),
    logical(1L)
  )]
  immutable_filled <- intersect(
    filled_variables,
    fixed_variables
  )
  filled_share <- if (length(segment_variables)) {
    length(filled_variables) / length(segment_variables)
  } else {
    0
  }
  list(
    target = as.integer(target),
    segment_indices = segment_indices,
    segment_variables = segment_variables,
    filled_variables = filled_variables,
    immutable_filled = immutable_filled,
    filled_share = as.numeric(filled_share),
    jumps = length(segment_indices) > 0L
  )
}

.sspice_logical_edit_inputs <- function(edit_names, metadata) {
  if (is.null(metadata) || !length(edit_names)) return(character())
  rows <- as.character(metadata[[1L]]) %in% edit_names
  unique(as.character(metadata[[3L]][rows]))
}

.sspice_violation_context <- function(state, record, dag, record_id,
                                       fixed_variables,
                                       logical_edit_metadata = NULL,
                                       dependency_index = NULL,
                                       violation_row = NULL) {
  if (is.null(dependency_index)) {
    dependency_index <- state$routing_check$dependency_index
  }
  report <- state$routing_check$full_report
  if (is.null(report) || !nrow(report)) {
    return(list(
      nodes = character(),
      gates = character(),
      responsible_gates = character(),
      inferred_gates = character(),
      segment = character()
    ))
  }
  if (!is.null(violation_row)) {
    violation_row <- as.integer(violation_row[[1L]])
    if (is.na(violation_row) || violation_row < 1L ||
        violation_row > nrow(report)) {
      stop("Invalid violation-row index.", call. = FALSE)
    }
    report <- report[violation_row, , drop = FALSE]
  }
  labels <- if ("varname_label" %in% names(report)) {
    unique(as.character(report$varname_label))
  } else character()
  route_labels <- if ("routevar_label" %in% names(report)) {
    unique(as.character(report$routevar_label))
  } else character()
  edit_names <- intersect(labels, dag$logical_edit_names %||% character())
  edit_inputs <- .sspice_logical_edit_inputs(edit_names, logical_edit_metadata)
  if (!is.null(dependency_index) && length(edit_inputs)) {
    repeat {
      derived_inputs <- intersect(edit_inputs, names(dependency_index$vaux))
      if (!length(derived_inputs)) break
      expanded <- unique(unlist(
        dependency_index$vaux[derived_inputs],
        use.names = FALSE
      ))
      next_inputs <- unique(c(setdiff(edit_inputs, derived_inputs), expanded))
      if (identical(sort(next_inputs), sort(edit_inputs))) break
      edit_inputs <- next_inputs
    }
  }
  sources <- if ("violation_source" %in% names(report)) {
    as.character(report$violation_source)
  } else {
    rep("ROUTING", nrow(report))
  }
  known_derived_vaux <- names(dependency_index$vaux %||% list())
  vaux_labels <- unique(c(
    as.character(report$varname_label[sources == "VAUX"]),
    intersect(labels, known_derived_vaux)
  ))
  consistency_vaux <- unique(as.character(report$varname_label[
    sources == "VAUX" &
      !is.na(report$errcode) &
      as.integer(report$errcode) == -3L
  ]))
  vaux_inputs <- if (!is.null(dependency_index) && length(vaux_labels)) {
    unique(unlist(dependency_index$vaux[vaux_labels], use.names = FALSE))
  } else {
    character()
  }
  if (!is.null(dependency_index) && length(vaux_inputs)) {
    repeat {
      derived_inputs <- intersect(vaux_inputs, names(dependency_index$vaux))
      if (!length(derived_inputs)) break
      expanded <- unique(unlist(
        dependency_index$vaux[derived_inputs],
        use.names = FALSE
      ))
      next_inputs <- unique(c(setdiff(vaux_inputs, derived_inputs), expanded))
      if (identical(sort(next_inputs), sort(vaux_inputs))) break
      vaux_inputs <- next_inputs
    }
  }
  violated_nodes <- setdiff(labels, c(edit_names, vaux_labels))
  activation <- state$routing_check$activation %||% numeric()
  names(activation) <- if (length(activation) == length(dag$varnames)) {
    dag$varnames
  } else {
    names(activation)
  }
  activation_targets <- unique(c(violated_nodes, vaux_labels))
  must_be_missing <- activation_targets[
    activation_targets %in% names(activation) &
      activation[activation_targets] == 1
  ]
  must_be_present <- activation_targets[
    activation_targets %in% names(activation) &
      activation[activation_targets] == -1
  ]
  explicit_gates <- intersect(route_labels, dag$varnames)
  routing_targets <- unique(c(violated_nodes, vaux_labels))
  inferred_gate_indices <- unlist(lapply(routing_targets, function(variable) {
    node <- match(variable, dag$varnames)
    if (is.na(node)) return(integer())
    .sspice_find_responsible_gate(node, record, dag)
  }), use.names = FALSE)
  inferred_gates <- dag$varnames[inferred_gate_indices[!is.na(inferred_gate_indices)]]
  gates <- unique(c(explicit_gates, inferred_gates))
  segment_indices <- unique(unlist(lapply(match(gates, dag$varnames), function(gate) {
    if (is.na(gate)) integer() else .sspice_jump_segment(gate, record, dag)
  }), use.names = FALSE))
  segment <- dag$varnames[segment_indices]
  nodes <- unique(c(
    violated_nodes, vaux_labels, edit_inputs, vaux_inputs, gates, segment
  ))
  nodes <- intersect(nodes, dag$varnames)
  nodes <- setdiff(nodes, c(record_id, fixed_variables,
                            dag$logical_edit_names %||% character(),
                            names(dependency_index$vaux %||% list())))
  list(
    nodes = nodes,
    gates = setdiff(gates, fixed_variables),
    responsible_gates = setdiff(explicit_gates, fixed_variables),
    inferred_gates = setdiff(inferred_gates, fixed_variables),
    segment = setdiff(segment, fixed_variables),
    violated_nodes = violated_nodes,
    violated_logical_edits = edit_names,
    vaux_nodes = vaux_labels,
    vaux_consistency_nodes = consistency_vaux,
    must_be_missing = must_be_missing,
    must_be_present = must_be_present,
    logical_edit_inputs = unique(c(edit_inputs, vaux_inputs)),
    vaux_inputs = vaux_inputs,
    routing_oracle = state$routing_check$routing_oracle
  )
}
