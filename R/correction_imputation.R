.sspice_method_for_variable <- function(method, variable) {
  if (length(method) == 1L && is.null(names(method))) return(method)
  if (variable %in% names(method)) return(unname(method[[variable]]))
  "uniform"
}

.sspice_node_distribution <- function(
    variable,
    record,
    dag,
    domain,
    donor_data,
    strata,
    method,
    priors,
    control,
    donor_cache = NULL) {
  selected_method <- .sspice_method_for_variable(method, variable)
  predecessor <- .sspice_previous_variable(
    variable = variable,
    dag = dag,
    record_id = dag$record_id %||% NULL
  )
  if (selected_method == "donor") {
    if (is.null(donor_cache) && !is.null(donor_data)) {
      donor_cache <- .sspice_build_donor_cache(
        donor_data = donor_data,
        dag = dag,
        record_id = dag$record_id %||% NULL,
        strata = strata,
        immutable = TRUE
      )
    }
    empirical <- .sspice_cached_node_distribution(
      cache = donor_cache,
      variable = variable,
      record = record,
      domain = domain,
      minimum_donors = control$minimum_donors,
      allow_unconditional_donor = control$allow_unconditional_donor
    )
    if (!is.null(empirical)) return(empirical)
    selected_method <- if (variable %in% names(priors)) {
      "user_prior"
    } else {
      "uniform"
    }
  }
  if (selected_method == "user_prior") {
    prior <- priors[[variable]]
    if (is.null(prior)) {
      stop("No user prior was supplied for ", variable, ".", call. = FALSE)
    }
    return(list(
      values = names(prior),
      probabilities = as.numeric(prior),
      source = "user_prior",
      predecessors_used = paste(predecessor, collapse = " | "),
      strata_used = "",
      donor_count = 0L
    ))
  }
  if (selected_method != "uniform") {
    stop(
      "Unknown imputation method for ", variable, ": ", selected_method, ".",
      call. = FALSE
    )
  }
  if (!length(domain)) {
    stop("No admissible domain is available for ", variable, ".",
         call. = FALSE)
  }
  list(
    values = as.character(domain),
    probabilities = rep(1 / length(domain), length(domain)),
    source = "uniform",
    predecessors_used = paste(predecessor, collapse = " | "),
    strata_used = "",
    donor_count = 0L
  )
}

.sspice_candidate_node_modes <- function(
    variable,
    record,
    dag,
    domains,
    donor_data,
    strata,
    method,
    priors,
    control,
    donor_cache = NULL) {
  distribution <- .sspice_node_distribution(
    variable = variable,
    record = record,
    dag = dag,
    domain = domains[[variable]],
    donor_data = donor_data,
    strata = strata,
    method = method,
    priors = priors,
    control = control,
    donor_cache = donor_cache
  )
  quantitative <- .sspice_is_quantitative_node(variable, record, dag)
  ordering <- order(distribution$probabilities, decreasing = TRUE)
  if (quantitative && length(ordering) > control$top_k) {
    keep <- sample(
      ordering,
      size = control$top_k,
      replace = FALSE,
      prob = distribution$probabilities[ordering]
    )
  } else {
    keep <- head(ordering, control$top_k)
  }
  distribution$values <- distribution$values[keep]
  distribution$probabilities <- distribution$probabilities[keep]
  distribution$probabilities <- distribution$probabilities /
    sum(distribution$probabilities)
  if (quantitative && identical(distribution$source, "conditional_donor")) {
    distribution$source <- "conditional_numeric_donor"
  } else if (quantitative &&
             identical(distribution$source, "marginal_donor")) {
    distribution$source <- "marginal_numeric_donor"
  }
  distribution$quantitative <- quantitative
  distribution
}
