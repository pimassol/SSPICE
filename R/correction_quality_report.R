.sspice_quality_empty <- function(columns) {
  result <- as.data.frame(
    stats::setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
  result
}

.sspice_quality_report_table <- function(check, record_id) {
  if (is.null(check)) return(data.frame())
  report <- if (is.list(check) && is.data.frame(check$full_report)) {
    check$full_report
  } else if (is.data.frame(check)) {
    check
  } else {
    data.frame()
  }
  if (!nrow(report)) return(report)
  if (!record_id %in% names(report)) {
    stop("A violation report does not contain record_id.", call. = FALSE)
  }
  report
}

.sspice_quality_violation_summaries <- function(report, record_id) {
  if (!nrow(report)) {
    return(list(
      by_variable = data.frame(
        variable = character(), violations = integer(),
        stringsAsFactors = FALSE
      ),
      by_record = data.frame(
        record_id = character(), violations = integer(),
        stringsAsFactors = FALSE
      )
    ))
  }
  variable_column <- if ("varname_label" %in% names(report)) {
    "varname_label"
  } else if ("variable" %in% names(report)) {
    "variable"
  } else {
    stop("A violation report does not contain a variable label.", call. = FALSE)
  }
  variables <- as.character(report[[variable_column]])
  identifiers <- as.character(report[[record_id]])
  variable_counts <- sort(table(variables), decreasing = TRUE)
  record_counts <- sort(table(identifiers), decreasing = TRUE)
  list(
    by_variable = data.frame(
      variable = names(variable_counts),
      violations = as.integer(variable_counts),
      row.names = NULL,
      stringsAsFactors = FALSE
    ),
    by_record = data.frame(
      record_id = names(record_counts),
      violations = as.integer(record_counts),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}

.sspice_quality_variable_profile <- function(
    data, dag, correction_result, record_id) {
  variables <- names(data)
  positions <- match(variables, dag$varnames)
  routes <- dag$routes_numeric
  nitems <- minval <- maxval <- rep(NA_real_, length(variables))
  valid <- !is.na(positions) & positions <= nrow(routes)
  if (ncol(routes) >= 4L && any(valid)) {
    nitems[valid] <- suppressWarnings(as.numeric(routes[positions[valid], 2L]))
    minval[valid] <- suppressWarnings(as.numeric(routes[positions[valid], 3L]))
    maxval[valid] <- suppressWarnings(as.numeric(routes[positions[valid], 4L]))
  }
  is_string <- vapply(data, is.character, logical(1L))
  quantitative <- !is_string & is.finite(nitems) & nitems == 1 &
    is.finite(minval) & is.finite(maxval) & minval < maxval
  measurement_type <- ifelse(
    is_string, "STRING", ifelse(quantitative, "QUANTITATIVE", "QUALITATIVE")
  )
  role <- rep("BASE", length(variables))
  names(role) <- variables
  edit_names <- intersect(dag$logical_edit_names %||% character(), variables)
  sampled <- intersect(correction_result$sampled_vaux %||% character(), variables)
  external <- intersect(
    correction_result$frozen_external_vaux %||% character(), variables
  )
  logical_groups <- intersect(
    unique(c(
      correction_result$vaux_classification$logical_groups %||% character(),
      .sspice_quality_canonical_logical_nodes(dag)
    )),
    variables
  )
  mode_locked <- setdiff(intersect(
    correction_result$mode_locked_vaux %||% character(), variables
  ), c(external, logical_groups))
  role[sampled] <- "ENDOGENOUS_VAUX"
  role[mode_locked] <- "MODE_LOCKED_VAUX"
  role[external] <- "EXOGENOUS_VAUX"
  role[logical_groups] <- "LOGICAL_GROUP"
  role[edit_names] <- "LOGICAL_EDIT"
  role[intersect(correction_result$strata %||% character(), variables)] <-
    "STRATUM"
  role[record_id] <- "RECORD_ID"
  immutable <- variables %in% (correction_result$immutable_variables %||%
    character())
  data.frame(
    variable = variables,
    storage_class = vapply(data, function(x) class(x)[1L], character(1L)),
    measurement_type = unname(measurement_type),
    role = unname(role),
    nitems = nitems,
    minval = minval,
    maxval = maxval,
    immutable = immutable,
    classification_rule = ifelse(
      is_string,
      "textual input column",
      ifelse(
        quantitative,
        "routing: nitems == 1 and minval < maxval",
        "routing: all other cases"
      )
    ),
    stringsAsFactors = FALSE
  )
}

.sspice_quality_canonical_logical_nodes <- function(dag) {
  groups <- .sspice_quality_logical_groups(dag)
  if (!nrow(groups)) return(character())
  group_id <- suppressWarnings(as.numeric(groups$group_id))
  valid <- is.finite(group_id) & group_id != 0
  if (!any(valid)) return(character())
  nodes <- ifelse(
    group_id[valid] > 0,
    paste0("VOR", abs(group_id[valid])),
    paste0("VXOR", abs(group_id[valid]))
  )
  intersect(unique(nodes), dag$varnames %||% character())
}

.sspice_quality_logical_groups <- function(dag) {
  routes <- dag$routes_original
  if (is.null(routes) || !is.data.frame(routes) || ncol(routes) < 5L) {
    return(data.frame(
      group_id = character(), group_type = character(),
      member_count = integer(), variables = character(),
      stringsAsFactors = FALSE
    ))
  }
  group_id <- suppressWarnings(as.numeric(routes[[5L]]))
  groups <- sort(unique(group_id[is.finite(group_id) & group_id != 0]))
  if (!length(groups)) {
    return(data.frame(
      group_id = character(), group_type = character(),
      member_count = integer(), variables = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(groups, function(group) {
    members <- as.character(routes[[1L]][group_id == group])
    data.frame(
      group_id = as.character(group),
      group_type = if (group > 0) "OR" else "XOR",
      member_count = length(members),
      variables = paste(members, collapse = " | "),
      stringsAsFactors = FALSE
    )
  }))
}

.sspice_quality_changes <- function(
    original_data, corrected_data, processed_ids, record_id) {
  original_match <- match(processed_ids, as.character(original_data[[record_id]]))
  corrected_match <- match(processed_ids, as.character(corrected_data[[record_id]]))
  keep_ids <- !is.na(original_match) & !is.na(corrected_match)
  processed_ids <- processed_ids[keep_ids]
  original_match <- original_match[keep_ids]
  corrected_match <- corrected_match[keep_ids]
  variables <- setdiff(intersect(names(original_data), names(corrected_data)), record_id)
  pieces <- vector("list", length(variables))
  for (index in seq_along(variables)) {
    variable <- variables[index]
    old <- original_data[[variable]][original_match]
    new <- corrected_data[[variable]][corrected_match]
    changed <- xor(is.na(old), is.na(new)) |
      (!is.na(old) & !is.na(new) & as.character(old) != as.character(new))
    if (!any(changed)) next
    old_character <- as.character(old[changed])
    new_character <- as.character(new[changed])
    pieces[[index]] <- data.frame(
      record_id = processed_ids[changed],
      variable = variable,
      old_value = old_character,
      new_value = new_character,
      change_type = ifelse(
        is.na(old[changed]), "FILLED",
        ifelse(is.na(new[changed]), "BLANKED", "MODE_CHANGED")
      ),
      stringsAsFactors = FALSE
    )
  }
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) {
    return(data.frame(
      record_id = character(), variable = character(),
      old_value = character(), new_value = character(),
      change_type = character(), stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, pieces)
  row.names(result) <- NULL
  result
}

.sspice_quality_change_summaries <- function(changes) {
  if (!nrow(changes)) {
    return(list(
      by_variable = data.frame(
        variable = character(), changes = integer(),
        stringsAsFactors = FALSE
      ),
      by_record = data.frame(
        record_id = character(), changes = integer(),
        stringsAsFactors = FALSE
      )
    ))
  }
  by_variable <- sort(table(changes$variable), decreasing = TRUE)
  by_record <- sort(table(changes$record_id), decreasing = TRUE)
  list(
    by_variable = data.frame(
      variable = names(by_variable), changes = as.integer(by_variable),
      row.names = NULL, stringsAsFactors = FALSE
    ),
    by_record = data.frame(
      record_id = names(by_record), changes = as.integer(by_record),
      row.names = NULL, stringsAsFactors = FALSE
    )
  )
}

.sspice_quality_quantitative_structural_changes <- function(
    changes, variable_profile) {
  empty <- data.frame(
    variable = character(), filled = integer(), blanked = integer(),
    numeric_value_changed = integer(), structural_changes = integer(),
    stringsAsFactors = FALSE
  )
  if (!nrow(changes)) return(empty)
  quantitative <- variable_profile$variable[
    variable_profile$measurement_type == "QUANTITATIVE"
  ]
  selected <- changes[changes$variable %in% quantitative, , drop = FALSE]
  if (!nrow(selected)) return(empty)
  variables <- sort(unique(selected$variable))
  result <- do.call(rbind, lapply(variables, function(variable) {
    rows <- selected[selected$variable == variable, , drop = FALSE]
    filled <- sum(rows$change_type == "FILLED")
    blanked <- sum(rows$change_type == "BLANKED")
    data.frame(
      variable = variable,
      filled = filled,
      blanked = blanked,
      numeric_value_changed = sum(rows$change_type == "MODE_CHANGED"),
      structural_changes = filled + blanked,
      stringsAsFactors = FALSE
    )
  }))
  result[order(
    result$structural_changes, result$filled, result$blanked,
    decreasing = TRUE
  ), , drop = FALSE]
}

.sspice_quality_uncertainty <- function(
    changes, original_data, corrected_data, variable_profile, correction_result,
    dag, donor_data, record_id) {
  empty <- data.frame(
    record_id = character(), variable = character(),
    measurement_type = character(), distribution_source = character(),
    donor_count = integer(), selected_probability = numeric(),
    entropy = numeric(), normalized_entropy = numeric(),
    support_size = integer(), domain_size = integer(),
    structural_action = character(), status = character(),
    stringsAsFactors = FALSE
  )
  if (!nrow(changes)) return(empty)
  result <- empty[rep(1L, nrow(changes)), , drop = FALSE]
  result$record_id <- changes$record_id
  result$variable <- changes$variable
  result$status <- "NOT_APPLICABLE"
  result$structural_action <- ifelse(
    changes$change_type == "FILLED", "FILLED",
    ifelse(changes$change_type == "BLANKED", "BLANKED", "VALUE_CHANGED")
  )
  profile_match <- match(changes$variable, variable_profile$variable)
  result$measurement_type <- variable_profile$measurement_type[profile_match]
  quantitative <- result$measurement_type == "QUANTITATIVE"
  result$status[quantitative] <- ifelse(
    changes$change_type[quantitative] == "FILLED",
    "QUANTITATIVE_FILLED_STRUCTURALLY",
    ifelse(
      changes$change_type[quantitative] == "BLANKED",
      "QUANTITATIVE_BLANKED_STRUCTURALLY",
      "QUANTITATIVE_VALUE_CHANGE_OUT_OF_SCOPE"
    )
  )
  eligible <- result$measurement_type == "QUALITATIVE" &
    changes$change_type != "BLANKED" &
    variable_profile$role[profile_match] == "BASE"
  if (is.null(donor_data) || !nrow(donor_data) || !any(eligible)) return(result)
  control <- correction_result$control
  domains <- .sspice_domains_from_routes(
    routes = dag$routes_numeric,
    variable_names = dag$varnames,
    data = original_data,
    donor_data = donor_data
  )
  cache <- .sspice_build_donor_cache(
    donor_data = donor_data,
    dag = dag,
    record_id = record_id,
    strata = correction_result$strata %||% character(),
    immutable = TRUE
  )
  for (index in which(eligible)) {
    row_index <- match(
      changes$record_id[index], as.character(corrected_data[[record_id]])
    )
    if (is.na(row_index)) {
      result$status[index] <- "RECORD_NOT_FOUND"
      next
    }
    distribution <- tryCatch(
      .sspice_node_distribution(
        variable = changes$variable[index],
        record = corrected_data[row_index, , drop = FALSE],
        dag = dag,
        domain = domains[[changes$variable[index]]],
        donor_data = donor_data,
        strata = correction_result$strata %||% character(),
        method = correction_result$imputation_method %||% "donor",
        priors = list(),
        control = control,
        donor_cache = cache
      ),
      error = function(condition) NULL
    )
    if (is.null(distribution)) {
      result$status[index] <- "DISTRIBUTION_UNAVAILABLE"
      next
    }
    probability <- distribution$probabilities / sum(distribution$probabilities)
    result$distribution_source[index] <- distribution$source
    result$donor_count[index] <- distribution$donor_count %||% 0L
    result$support_size[index] <- sum(is.finite(probability) & probability > 0)
    result$domain_size[index] <- length(domains[[changes$variable[index]]])
    selected <- match(changes$new_value[index], as.character(distribution$values))
    result$selected_probability[index] <- if (is.na(selected)) NA_real_ else
      probability[selected]
    selected_outside_support <- is.na(selected) ||
      !is.finite(probability[selected]) || probability[selected] <= 0
    positive_probability <- probability[
      is.finite(probability) & probability > 0
    ]
    result$entropy[index] <- -sum(
      positive_probability * log(positive_probability)
    )
    result$normalized_entropy[index] <- if (
      length(positive_probability) <= 1L
    ) {
      0
    } else {
      result$entropy[index] / log(length(positive_probability))
    }
    result$status[index] <- if (selected_outside_support) {
      "SELECTED_VALUE_OUTSIDE_POSTHOC_SUPPORT"
    } else {
      "AVAILABLE"
    }
  }
  row.names(result) <- NULL
  result
}

.sspice_quality_write_html <- function(report, path) {
  esc <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  format_value <- function(x) {
    if (is.numeric(x)) {
      return(ifelse(
        is.na(x), "", format(x, digits = 6L, big.mark = ",", trim = TRUE)
      ))
    }
    esc(x)
  }

  html_table <- function(data, max_rows = 20L, empty_text = "No data available.") {
    if (is.null(data) || !is.data.frame(data) || !nrow(data)) {
      return(paste0("<p class='empty'>", esc(empty_text), "</p>"))
    }
    shown <- utils::head(data, max_rows)
    header <- paste0(
      "<tr>", paste0("<th>", esc(names(shown)), "</th>", collapse = ""),
      "</tr>"
    )
    rows <- apply(shown, 1L, function(row) {
      paste0(
        "<tr>", paste0("<td>", esc(row), "</td>", collapse = ""), "</tr>"
      )
    })
    note <- if (nrow(data) > max_rows) paste0(
      "<p class='table-note'>Showing the first ", max_rows,
      " of ", nrow(data), " rows. The complete table is available in CSV.</p>"
    ) else ""
    paste0(
      "<div class='table-wrap'><table>", header,
      paste(rows, collapse = "\n"), "</table></div>", note
    )
  }

  metric_table <- function(metric_names) {
    summary <- report$summary
    selected <- summary[match(metric_names, summary$metric), , drop = FALSE]
    selected <- selected[!is.na(selected$metric), , drop = FALSE]
    selected$value <- format_value(selected$value)
    html_table(selected, max_rows = nrow(selected))
  }

  numeric_description <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (!length(x)) {
      return(data.frame(
        statistic = character(), value = numeric(), stringsAsFactors = FALSE
      ))
    }
    data.frame(
      statistic = c("N", "Minimum", "25th percentile", "Median", "Mean",
                    "75th percentile", "Maximum"),
      value = c(
        length(x), min(x), unname(stats::quantile(x, 0.25)),
        stats::median(x), mean(x), unname(stats::quantile(x, 0.75)), max(x)
      ),
      stringsAsFactors = FALSE
    )
  }

  uncertainty <- report$uncertainty
  if (is.null(uncertainty)) uncertainty <- data.frame()
  available <- if (nrow(uncertainty) && "status" %in% names(uncertainty)) {
    uncertainty[uncertainty$status == "AVAILABLE", , drop = FALSE]
  } else data.frame()
  qualitative <- if (nrow(available) &&
      all(c("measurement_type", "entropy") %in% names(available))) {
    available[available$measurement_type == "QUALITATIVE" &
                is.finite(available$entropy), , drop = FALSE]
  } else data.frame()
  qualitative_all <- if (nrow(uncertainty) &&
      "measurement_type" %in% names(uncertainty)) {
    uncertainty[uncertainty$measurement_type == "QUALITATIVE", , drop = FALSE]
  } else data.frame()

  qualitative_status_table <- if (nrow(qualitative_all) &&
      "status" %in% names(qualitative_all)) {
    counts <- sort(table(qualitative_all$status), decreasing = TRUE)
    data.frame(
      status = names(counts),
      modifications = as.integer(counts),
      percentage = round(100 * as.numeric(counts) / sum(counts), 3L),
      stringsAsFactors = FALSE
    )
  } else data.frame()

  change_type_table <- if (nrow(report$changes)) {
    counts <- sort(table(report$changes$change_type), decreasing = TRUE)
    data.frame(
      change_type = names(counts),
      modifications = as.integer(counts),
      percentage = round(100 * as.numeric(counts) / sum(counts), 3L),
      stringsAsFactors = FALSE
    )
  } else data.frame()

  mean_by_variable <- function(data, value_name) {
    if (!nrow(data) || !all(c("variable", value_name) %in% names(data))) {
      return(data.frame())
    }
    result <- stats::aggregate(
      data[[value_name]], list(variable = data$variable),
      function(x) mean(x, na.rm = TRUE)
    )
    names(result)[2L] <- paste0("mean_", value_name)
    result$n <- as.integer(table(data$variable)[result$variable])
    result[order(result[[2L]], decreasing = TRUE), , drop = FALSE]
  }

  high_entropy <- mean_by_variable(qualitative, "entropy")
  high_normalized_entropy <- mean_by_variable(
    qualitative, "normalized_entropy"
  )

  unresolved <- report$unresolved_records
  if (is.null(unresolved)) unresolved <- data.frame()

  html <- c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>SSPICE correction quality report</title>",
    "<style>",
    "body{font-family:Arial,sans-serif;max-width:1180px;margin:2em auto;padding:0 1em;color:#202124;line-height:1.45}",
    "h1{color:#17365d}h2{margin-top:2.2em;color:#17365d;border-bottom:2px solid #8faadc;padding-bottom:.25em}",
    "h3{color:#2f5597;margin-top:1.6em}nav{background:#f3f6fa;border:1px solid #c8d3e1;padding:1em 1.4em}",
    "nav ol{columns:2;margin:.4em 0}nav a{color:#174a7e;text-decoration:none}",
    ".lead{font-size:1.08em}.note{background:#fff4ce;border-left:5px solid #e0a800;padding:.8em 1em}",
    ".good{background:#eaf5ea;border-left:5px solid #3c8c40;padding:.8em 1em}",
    ".table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%;margin:.7em 0 1em}",
    "th,td{border:1px solid #ccd2d8;padding:.45em;text-align:left;vertical-align:top}th{background:#e9eff7}",
    ".table-note,.empty{color:#666;font-size:.92em}.definition{color:#444}",
    "footer{margin-top:3em;border-top:1px solid #ccc;padding-top:1em;color:#666;font-size:.9em}",
    "</style></head><body>",
    "<h1>SSPICE correction quality report</h1>",
    paste0("<p class='lead'>Generated: ", esc(report$generated_at), "</p>"),
    "<p>This report evaluates SSPICE as a generalized structural control and correction system for categorical data. Quantitative variables are assessed only through presence or absence; their numerical values are outside the inferential scope of SSPICE.</p>",
    "<nav><strong>Contents</strong><ol>",
    "<li><a href='#dataset'>Dataset and correction setup</a></li>",
    "<li><a href='#variables'>Variable structure</a></li>",
    "<li><a href='#outcome'>Correction outcome</a></li>",
    "<li><a href='#invasiveness'>Correction invasiveness</a></li>",
    "<li><a href='#violations'>Violations before and after</a></li>",
    "<li><a href='#changes'>Changes by variable and record</a></li>",
    "<li><a href='#uncertainty'>Imputation uncertainty</a></li>",
    "<li><a href='#unresolved'>Unresolved records</a></li>",
    "<li><a href='#execution'>Execution details</a></li>",
    "</ol></nav>",

    "<h2 id='dataset'>1. Dataset and correction setup</h2>",
    metric_table(c(
      "records_input", "variables_input", "records_exact_initial",
      "records_erroneous_initial", "donors_supplied", "donors_retained"
    )),
    "<h3>Run metadata</h3>", html_table(report$run_metadata, 20L),

    "<h2 id='variables'>2. Variable structure</h2>",
    metric_table(c(
      "base_variables", "quantitative_variables", "qualitative_variables",
      "string_variables", "endogenous_vaux", "exogenous_vaux",
      "logical_groups", "logical_edits", "immutable_variables"
    )),
    "<p class='definition'><strong>Classification rule.</strong> A textual input column is STRING. A non-textual routing variable is QUANTITATIVE only when nitems = 1 and minval &lt; maxval; every other case is QUALITATIVE.</p>",
    "<h3>Canonical OR/XOR groups</h3>", html_table(report$logical_groups, 40L),

    "<h2 id='outcome'>3. Correction outcome</h2>",
    metric_table(c(
      "records_processed", "records_erroneous_not_processed",
      "records_resolved", "records_unresolved",
      "violations_before", "violations_after"
    )),
    "<p class='good'>A resolved record has no remaining routing, logical-edit, or VAUX consistency violation. Residual cases are listed in Chapter 8.</p>",

    "<h2 id='invasiveness'>4. Correction invasiveness</h2>",
    metric_table(c(
      "records_changed", "cells_changed", "fills", "blanks",
      "mode_changes", "editable_cells", "changed_cell_percentage"
    )),
    "<h3>Composition of changes</h3>", html_table(change_type_table, 10L),
    "<p class='definition'><strong>FILLED</strong> means NA to a value; <strong>BLANKED</strong> means a value to NA; <strong>MODE_CHANGED</strong> means replacement of one non-missing modality by another.</p>",

    "<h2 id='violations'>5. Violations before and after correction</h2>",
    "<h3>Variables most involved before correction</h3>",
    html_table(report$violations_by_variable_before, 20L),
    "<h3>Records with the most violations before correction</h3>",
    html_table(report$violations_by_record_before, 20L),
    "<h3>Residual violations by variable</h3>",
    html_table(report$violations_by_variable_after, 20L, "No residual violation."),
    "<h3>Residual violations by record</h3>",
    html_table(report$violations_by_record_after, 20L, "No residual violation."),

    "<h2 id='changes'>6. Changes by variable and record</h2>",
    "<h3>Variables most frequently changed</h3>",
    html_table(report$changes_by_variable, 20L),
    "<h3>Records with the most changed cells</h3>",
    html_table(report$changes_by_record, 20L),

    "<h2 id='uncertainty'>7. Categorical imputation uncertainty and quantitative structural actions</h2>",
    "<p class='note'>Entropy and selected-modality probability are reported only for qualitative variables. They are descriptive post-hoc measures from the immutable donor distribution and are not multiple-imputation variance estimates. For quantitative variables SSPICE reports only whether a value was filled or blanked.</p>",
    "<h3>Qualitative uncertainty status</h3>",
    html_table(qualitative_status_table, 20L),
    "<h3>Qualitative imputations: entropy</h3>",
    "<p class='definition'>Lower entropy indicates a more concentrated donor distribution. Higher entropy indicates several plausible modalities and therefore greater uncertainty.</p>",
    html_table(numeric_description(qualitative$entropy), 20L),
    "<h3>Qualitative imputations: selected-modality probability</h3>",
    "<p class='definition'>A higher selected probability means that the imputed modality received stronger support from the donor distribution.</p>",
    html_table(numeric_description(qualitative$selected_probability), 20L),
    "<h3>Variables with the highest mean entropy</h3>",
    html_table(high_entropy, 20L),
    "<h3>Qualitative imputations: normalized entropy</h3>",
    "<p class='definition'>Normalized entropy ranges from 0 to 1 and accounts for the number of supported modalities. Values near 0 indicate concentration; values near 1 indicate a distribution spread across its support.</p>",
    html_table(numeric_description(qualitative$normalized_entropy), 20L),
    "<h3>Variables with the highest mean normalized entropy</h3>",
    html_table(high_normalized_entropy, 20L),
    "<h3>Quantitative variables: presence/absence actions</h3>",
    "<p class='note'><strong>FILLED</strong> means that SSPICE established that a quantitative value had to be present and inserted a value. <strong>BLANKED</strong> means that routing required its absence. The numerical magnitude inserted or changed is not evaluated as a quantitative imputation.</p>",
    html_table(
      report$quantitative_structural_changes, 40L,
      "No quantitative variable was structurally filled or blanked."
    ),

    "<h2 id='unresolved'>8. Unresolved records</h2>",
    "<p>These records retain at least one final structural violation and require targeted inspection.</p>",
    html_table(unresolved, 30L, "All processed records were resolved."),

    "<h2 id='execution'>9. Execution details</h2>",
    metric_table(c(
      "elapsed_seconds", "workers_used", "batches_total", "batches_reused"
    )),
    "<footer>SSPICE correction quality report. Complete analytical tables are stored as CSV files in the same directory.</footer>",
    "</body></html>"
  )
  writeLines(html, path, useBytes = TRUE)
}

#' Build a quality report for an SSPICE correction run
#'
#' Produces read-only diagnostics describing the input data, violations before
#' and after correction, changed cells, correction invasiveness, execution
#' characteristics, categorical post-hoc uncertainty, and presence/absence
#' actions on quantitative variables. It never changes the correction result
#' or any input record.
#'
#' Variable measurement type is determined without heuristic interpretation:
#' textual columns are `STRING`; non-textual routing nodes with `nitems == 1`
#' and `minval < maxval` are `QUANTITATIVE`; all other variables are
#' `QUALITATIVE`.
#'
#' @param original_data Data frame before correction.
#' @param correction_result Object returned by [correct_data()].
#' @param dag SSPICE DAG used for correction.
#' @param corrected_data Optional corrected data frame. By default,
#'   `correction_result$corrected_data` is used.
#' @param initial_check Optional routing-check object for the input data.
#' @param final_check Optional routing-check object for the corrected data.
#'   By default, `correction_result$final_routing_check` is used.
#' @param donor_data Optional immutable donor data used to estimate post-hoc
#'   uncertainty for filled or changed base variables.
#' @param record_id Record identifier; defaults to `dag$record_id` or the first
#'   column of `correction_result$record_summary`.
#' @param output_dir Optional directory in which CSV, RDS, and HTML report
#'   files are written.
#' @param elapsed_seconds Optional total execution time when it is not already
#'   represented in external run metadata.
#' @param donor_origin Optional label describing whether donors were obtained
#'   from input records passing the initial check or supplied separately.
#'
#' @return An object of class `sspice_correction_quality_report`.
#' @export
correction_quality_report <- function(
    original_data,
    correction_result,
    dag,
    corrected_data = NULL,
    initial_check = NULL,
    final_check = NULL,
    donor_data = NULL,
    record_id = NULL,
    output_dir = NULL,
    elapsed_seconds = NA_real_,
    donor_origin = NA_character_) {
  if (!is.data.frame(original_data) || !nrow(original_data)) {
    stop("original_data must be a non-empty data frame.", call. = FALSE)
  }
  if (!inherits(correction_result, "sspice_correction_result")) {
    stop("correction_result must be an sspice_correction_result.", call. = FALSE)
  }
  corrected_data <- corrected_data %||% correction_result$corrected_data
  if (!is.data.frame(corrected_data)) {
    stop("corrected_data must be a data frame.", call. = FALSE)
  }
  record_id <- record_id %||% dag$record_id %||%
    names(correction_result$record_summary)[1L]
  if (!record_id %in% names(original_data) ||
      !record_id %in% names(corrected_data)) {
    stop("record_id must occur in original and corrected data.", call. = FALSE)
  }
  summary_records <- correction_result$record_summary
  processed_ids <- as.character(summary_records[[record_id]])
  variable_profile <- .sspice_quality_variable_profile(
    original_data, dag, correction_result, record_id
  )
  changes <- .sspice_quality_changes(
    original_data, corrected_data, processed_ids, record_id
  )
  change_summaries <- .sspice_quality_change_summaries(changes)
  initial_report <- .sspice_quality_report_table(initial_check, record_id)
  final_check <- final_check %||% correction_result$final_routing_check
  final_report <- .sspice_quality_report_table(final_check, record_id)
  before <- .sspice_quality_violation_summaries(initial_report, record_id)
  after <- .sspice_quality_violation_summaries(final_report, record_id)
  uncertainty <- .sspice_quality_uncertainty(
    changes, original_data, corrected_data, variable_profile, correction_result,
    dag, donor_data, record_id
  )
  quantitative_structural_changes <-
    .sspice_quality_quantitative_structural_changes(
      changes, variable_profile
    )
  resolved <- as.logical(summary_records$resolved)
  erroneous_initial_ids <- if (nrow(initial_report)) {
    unique(as.character(initial_report[[record_id]]))
  } else {
    character()
  }
  initial_check_available <- !is.null(initial_check)
  records_erroneous_initial <- if (initial_check_available) {
    length(erroneous_initial_ids)
  } else {
    NA_integer_
  }
  records_exact_initial <- if (initial_check_available) {
    nrow(original_data) - records_erroneous_initial
  } else {
    NA_integer_
  }
  number_changed_records <- length(unique(changes$record_id))
  editable_cells <- length(processed_ids) * sum(
    variable_profile$role != "RECORD_ID" & !variable_profile$immutable
  )
  donor_supplied <- correction_result$donor_diagnostics$supplied %||% 0L
  donor_retained <- correction_result$donor_diagnostics$retained %||% 0L
  metrics <- c(
    records_input = nrow(original_data),
    variables_input = ncol(original_data),
    base_variables = sum(variable_profile$role == "BASE"),
    quantitative_variables = sum(variable_profile$measurement_type == "QUANTITATIVE"),
    qualitative_variables = sum(variable_profile$measurement_type == "QUALITATIVE"),
    string_variables = sum(variable_profile$measurement_type == "STRING"),
    endogenous_vaux = sum(variable_profile$role == "ENDOGENOUS_VAUX"),
    exogenous_vaux = sum(variable_profile$role == "EXOGENOUS_VAUX"),
    logical_groups = sum(variable_profile$role == "LOGICAL_GROUP"),
    logical_edits = sum(variable_profile$role == "LOGICAL_EDIT"),
    immutable_variables = sum(variable_profile$immutable),
    records_processed = length(processed_ids),
    records_exact_initial = records_exact_initial,
    records_erroneous_initial = records_erroneous_initial,
    records_erroneous_not_processed = if (is.na(records_erroneous_initial)) {
      NA_real_
    } else {
      max(0, records_erroneous_initial - length(processed_ids))
    },
    records_resolved = sum(resolved, na.rm = TRUE),
    records_unresolved = sum(!resolved, na.rm = TRUE),
    donors_supplied = donor_supplied,
    donors_retained = donor_retained,
    violations_before = if (initial_check_available) nrow(initial_report) else NA_real_,
    violations_after = nrow(final_report),
    records_changed = number_changed_records,
    cells_changed = nrow(changes),
    fills = sum(changes$change_type == "FILLED"),
    blanks = sum(changes$change_type == "BLANKED"),
    mode_changes = sum(changes$change_type == "MODE_CHANGED"),
    editable_cells = editable_cells,
    changed_cell_percentage = if (editable_cells) 100 * nrow(changes) / editable_cells else NA_real_,
    elapsed_seconds = as.numeric(elapsed_seconds),
    workers_used = correction_result$parallel$workers_used %||% 1L,
    batches_total = correction_result$parallel$batches_total %||% 1L,
    batches_reused = correction_result$parallel$batches_reused %||% 0L
  )
  report <- structure(
    list(
      generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      summary = data.frame(
        metric = names(metrics), value = unname(metrics),
        row.names = NULL, stringsAsFactors = FALSE
      ),
      run_metadata = data.frame(
        field = c("record_id", "strata", "imputation_method", "donor_origin"),
        value = c(
          record_id,
          paste(correction_result$strata %||% character(), collapse = " | "),
          paste(correction_result$imputation_method %||% "", collapse = " | "),
          as.character(donor_origin)[1L]
        ),
        stringsAsFactors = FALSE
      ),
      variable_profile = variable_profile,
      logical_groups = .sspice_quality_logical_groups(dag),
      violations_before = initial_report,
      violations_after = final_report,
      violations_by_variable_before = before$by_variable,
      violations_by_variable_after = after$by_variable,
      violations_by_record_before = before$by_record,
      violations_by_record_after = after$by_record,
      changes = changes,
      changes_by_variable = change_summaries$by_variable,
      changes_by_record = change_summaries$by_record,
      uncertainty = uncertainty,
      quantitative_structural_changes = quantitative_structural_changes,
      unresolved_records = correction_result$unresolved_records,
      record_summary = summary_records
    ),
    class = "sspice_correction_quality_report"
  )
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    tables <- report[vapply(report, is.data.frame, logical(1L))]
    for (name in names(tables)) {
      utils::write.csv(
        tables[[name]], file.path(output_dir, paste0(name, ".csv")),
        row.names = FALSE, na = ""
      )
    }
    saveRDS(report, file.path(output_dir, "correction_quality_report.rds"))
    .sspice_quality_write_html(
      report, file.path(output_dir, "correction_quality_report.html")
    )
  }
  report
}

#' @export
print.sspice_correction_quality_report <- function(x, ...) {
  cat("SSPICE correction quality report\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}
