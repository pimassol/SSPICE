utils::globalVariables(c(
  ".sspice_batch_context",
  ".sspice_batch_correct_one",
  "rng_state"
))

.sspice_validated_donors <- function(donor_data, dag, record_id,
                                      items_mapping_file,
                                      already_validated = FALSE) {
  if (is.null(donor_data)) {
    return(list(data = NULL, supplied = 0L, retained = 0L))
  }
  if (!is.data.frame(donor_data)) {
    stop("donor_data must be a data frame.", call. = FALSE)
  }
  if (!identical(names(donor_data), dag$varnames)) {
    stop("donor_data columns must match DAG variables and order.", call. = FALSE)
  }
  if (isTRUE(already_validated)) {
    return(list(
      data = donor_data,
      supplied = nrow(donor_data),
      retained = nrow(donor_data)
    ))
  }
  mapping_name <- if (is.null(items_mapping_file)) NULL else
    basename(items_mapping_file)
  mapping_path <- if (is.null(items_mapping_file)) NULL else
    dirname(items_mapping_file)
  result <- routing_check(
    dset_data = donor_data,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = record_id,
    items.maps = mapping_name,
    path.items.maps = mapping_path,
    logical_edit_names = dag$logical_edit_names
  )
  report <- result$full_report
  invalid_ids <- if (is.null(report) || !nrow(report)) character() else
    unique(as.character(report[[record_id]]))
  keep <- !as.character(donor_data[[record_id]]) %in% invalid_ids
  list(
    data = donor_data[keep, , drop = FALSE],
    supplied = nrow(donor_data),
    retained = sum(keep)
  )
}

.sspice_parallel_worker_count <- function(n_records, requested = NULL) {
  if (!is.null(requested)) {
    return(min(as.integer(requested), as.integer(n_records)))
  }
  detected <- parallel::detectCores(logical = TRUE)
  if (length(detected) != 1L || is.na(detected) || detected < 1L) {
    detected <- 1L
  }
  min(max(1L, as.integer(detected) - 2L), as.integer(n_records))
}

.sspice_record_rng_streams <- function(seed, n_records) {
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed))
  streams <- vector("list", n_records)
  streams[[1L]] <- .Random.seed
  if (n_records > 1L) {
    for (index in 2:n_records) {
      streams[[index]] <- parallel::nextRNGStream(streams[[index - 1L]])
    }
  }
  streams
}

.sspice_checkpoint_fingerprint <- function(object) {
  path <- tempfile("sspice-checkpoint-fingerprint-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, compress = FALSE)
  unname(tools::md5sum(path)[[1L]])
}

.sspice_atomic_save_rds <- function(object, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(basename(path), "-"),
    tmpdir = directory,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary)
  if (file.exists(path)) {
    unlink_status <- unlink(path)
    if (!identical(unlink_status, 0L)) {
      stop("Unable to replace checkpoint: ", path, ".", call. = FALSE)
    }
  }
  if (!file.rename(temporary, path)) {
    stop("Unable to commit checkpoint: ", path, ".", call. = FALSE)
  }
  invisible(path)
}

.sspice_correct_one_record <- function(index, context, rng_state = NULL) {
  if (!is.null(rng_state)) {
    assign(".Random.seed", rng_state, envir = .GlobalEnv)
  }
  original <- context$data[index, , drop = FALSE]
  frozen_values <- as.list(
    original[1L, context$external_frozen_vaux, drop = FALSE]
  )
  mode_locked_values <- as.list(
    original[1L, context$mode_locked_vaux, drop = FALSE]
  )
  for (variable in intersect(
    context$vaux_classification$logical_groups,
    context$mode_locked_vaux
  )) {
    rows <- as.character(context$vaux_metadata[[1L]]) == variable
    defined_modes <- unique(as.character(context$vaux_metadata[[2L]][rows]))
    defined_modes <- defined_modes[
      !is.na(defined_modes) & nzchar(trimws(defined_modes))
    ]
    if (length(defined_modes) != 1L) {
      stop(
        "Logical-group VAUX ", variable,
        " must define exactly one VOR mode.",
        call. = FALSE
      )
    }
    mode_locked_values[[variable]] <- .sspice_cast_mode(
      defined_modes[[1L]],
      original[[variable]]
    )
  }
  result <- .sspice_correct_record(
    record = original,
    original = original,
    dag = context$dag,
    record_id = context$record_id,
    domains = context$domains,
    donor_data = context$donor_data,
    donor_cache = context$donor_cache,
    strata = context$strata,
    imputation_method = context$imputation_method,
    priors = context$priors,
    fixed_variables = context$proposal_excluded_variables,
    control = context$control,
    vaux_metadata = context$vaux_metadata,
    logical_edit_metadata = context$logical_edit_metadata,
    external_data_dir = context$external_data_dir,
    endogenous_vaux = context$sampled_vaux,
    frozen_values = frozen_values,
    mode_locked_values = mode_locked_values,
    items_mapping_file = context$items_mapping_file,
    routing_oracle = context$routing_oracle
  )
  list(
    index = index,
    identifier = original[[context$record_id]][1L],
    corrected_record = result$corrected_record,
    final_routing_violations = result$final_routing_violations,
    final_logical_edit_violations = result$final_logical_edit_violations,
    final_vaux_violations = result$final_vaux_violations,
    final_energy = result$final_energy,
    resolved = result$resolved
  )
}

#' Correct questionnaire records using MCMC simulated annealing
#'
#' Correction is restricted to the local neighbourhood of reported routing or
#' logical-edit violations. Every fill or flip samples a mode from the node
#' domain. Immutable donor matrices estimate the conditional distribution of
#' each variable given its immediate predecessor and strata, including when
#' the predecessor is missing, with predecessor-only and marginal fallbacks.
#' Complete donor records are never copied or updated with corrected records.
#'
#' @section Territorial strata in the example:
#' In the supplied example, STRATUM identifies five geographical
#' divisions of Italy: North-West (1), North-East (2), Centre (3),
#' South (4), and Islands (5). It is used for stratification during
#' correction. With freeze_strata = TRUE, stratum membership remains
#' unchanged.
#'
#' @section Preparation before the first batch:
#' Before record correction begins, SSPICE prepares or reuses
#' conditional distributions, donor information, variable domains
#' and other supporting structures. Parallel execution also requires
#' worker initialization. Step 10 performs an initial routing check
#' unless a valid cached result is available.
#' 
#' Preparation time depends on the number of variables and records,
#' the number of categories and strata, and the routing structure.
#' Computing conditional distributions can take appreciable time
#' before the first batch starts.
#' 
#' The first batch-completion message appears only after preparation
#' and processing of that batch. A delay before this message does not
#' by itself indicate that execution has stalled.
#' 
#' Distinguish total script elapsed time from the correction time
#' reported in the output. Their difference is not a complete measure
#' of initialization time, because some preparation may occur within
#' the timed correction call. Measuring individual preparation stages
#' requires separate timing.
#' SSPICE startup documentation end.
#'
#' @param data Data frame to correct, already aligned with `dag`.
#' @param dag An SSPICE DAG or extended DAG.
#' @param record_id Record identifier. If omitted, `dag$record_id` is used.
#' @param donor_data Optional data frame containing validated records.
#' @param donor_data_validated Whether `donor_data` has already passed the
#'   complete DAG+ routing and logical-edit check.
#' @param donor_cache Optional immutable cache built from the original valid
#'   donor records. Corrected records are never added to this cache.
#' @param vaux_metadata Optional nine-column VAUX metadata.
#' @param logical_edit_metadata Optional nine-column LEDIT metadata.
#' @param immutable_variables Base variables that must never be changed.
#'   Sampled VAUX, mode-locked VAUX, and logical edits are classified
#'   automatically and must not be supplied here.
#' @param strata Variables used in addition to active DAG predecessors.
#' @param imputation_method `donor`, `user_prior`, `uniform`, or a named vector.
#' @param imputation_priors Optional named list of named probability vectors.
#' @param domains Optional named list overriding domains derived from routing.
#' @param external_data_dir Directory containing external VAUX sources.
#' @param items_mapping_file Optional complete path to `items_mapping.csv`.
#' @param control An object produced by [correction_control()].
#' @param batch_output_dir Optional directory for atomic per-batch checkpoints.
#'   Domains, donor distributions, caches, and the routing oracle are prepared
#'   once before batching and are never rebuilt between batches.
#' @return An object of class `sspice_correction_result`.
#' @export
correct_data <- function(
    data,
    dag,
    record_id = NULL,
    donor_data = NULL,
    donor_data_validated = FALSE,
    donor_cache = NULL,
    vaux_metadata = NULL,
    logical_edit_metadata = NULL,
    immutable_variables = NULL,
    strata = NULL,
    imputation_method = c("donor", "user_prior", "uniform"),
    imputation_priors = NULL,
    domains = NULL,
    external_data_dir = ".",
    items_mapping_file = NULL,
    control = correction_control(),
    batch_output_dir = NULL) {
  imputation_method <- if (length(imputation_method) == 1L ||
                              !is.null(names(imputation_method))) {
    imputation_method
  } else {
    match.arg(imputation_method)
  }
  if (!is.data.frame(data) || !nrow(data)) {
    stop("data must be a non-empty data frame.", call. = FALSE)
  }
  required_dag <- c("Bt", "routes_numeric", "varnames")
  if (!all(required_dag %in% names(dag))) {
    stop("The DAG object is incomplete.", call. = FALSE)
  }
  if (!identical(names(data), dag$varnames)) {
    stop("Data columns must match DAG variables and order.", call. = FALSE)
  }
  record_id <- record_id %||% dag$record_id
  if (is.null(record_id) || length(record_id) != 1L ||
      !record_id %in% names(data)) {
    stop("A valid record_id must be supplied.", call. = FALSE)
  }
  if (anyDuplicated(data[[record_id]])) {
    stop("record_id values must be unique.", call. = FALSE)
  }
  dag$record_id <- record_id
  strata <- unique(as.character(strata %||% character()))
  strata <- strata[!is.na(strata) & nzchar(strata)]
  unknown_strata <- setdiff(strata, names(data))
  if (length(unknown_strata)) {
    stop("Unknown strata variables: ",
         paste(unknown_strata, collapse = ", "), ".", call. = FALSE)
  }
  user_immutable_variables <- .sspice_normalize_fixed_variables(
    immutable_variables,
    names(data)
  )
  edit_names <- dag$logical_edit_names %||% character()
  vaux_names <- if (is.null(vaux_metadata)) character() else
    unique(as.character(vaux_metadata[[1L]]))
  external_vaux <- character()
  if (!is.null(vaux_metadata) && ncol(vaux_metadata) >= 5L) {
    source_file <- trimws(as.character(vaux_metadata[[4L]]))
    source_key <- trimws(as.character(vaux_metadata[[5L]]))
    external_rows <- (!is.na(source_file) & nzchar(source_file)) |
      (!is.na(source_key) & nzchar(source_key))
    external_vaux <- unique(as.character(vaux_metadata[[1L]][external_rows]))
  }
  endogenous_vaux <- setdiff(vaux_names, external_vaux)
  logical_group_vaux <- character()
  if (!is.null(vaux_metadata) && ncol(vaux_metadata) >= 9L) {
    logical_tokens <- trimws(as.character(vaux_metadata[[9L]]))
    logical_group_rows <- grepl(
      "(^|[^[:alnum:]_])OR([^[:alnum:]_]|$)",
      logical_tokens,
      ignore.case = TRUE
    )
    logical_group_candidates <- unique(as.character(
      vaux_metadata[[1L]][logical_group_rows]
    ))
    logical_group_candidates <- intersect(
      logical_group_candidates,
      endogenous_vaux
    )
    # OR can also join conditions belonging to an ordinary multi-modal
    # endogenous VAUX.  A genuine logical-group VAUX (VOR) is identified
    # by the combination of OR and exactly one defined output mode.
    logical_group_vaux <- logical_group_candidates[vapply(
      logical_group_candidates,
      function(variable) {
        rows <- as.character(vaux_metadata[[1L]]) == variable
        modes <- trimws(as.character(vaux_metadata[[2L]][rows]))
        modes <- unique(modes[!is.na(modes) & nzchar(modes)])
        length(modes) == 1L
      },
      logical(1)
    )]
  }
  auxiliary_marked_immutable <- intersect(
    user_immutable_variables,
    unique(c(vaux_names, edit_names))
  )
  if (length(auxiliary_marked_immutable)) {
    stop(
      "Recalculable or mode-locked auxiliary variables cannot be listed in ",
      "immutable_variables: ",
      paste(auxiliary_marked_immutable, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  immutable_variables <- unique(c(
    user_immutable_variables,
    record_id,
    if (control$freeze_strata) strata else character()
  ))
  derived_domains <- .sspice_domains_from_routes(
    routes = dag$routes_numeric,
    variable_names = dag$varnames,
    data = data,
    donor_data = donor_data
  )
  if (!is.null(domains)) {
    if (!is.list(domains) || is.null(names(domains))) {
      stop("domains must be a named list.", call. = FALSE)
    }
    derived_domains[names(domains)] <- domains
  }
  priors <- .sspice_validate_priors(imputation_priors, derived_domains)
  donor_validation <- .sspice_validated_donors(
    donor_data, dag, record_id, items_mapping_file,
    already_validated = donor_data_validated
  )
  donor_data <- donor_validation$data
  if (is.null(donor_cache)) {
    donor_cache <- .sspice_build_donor_cache(
      donor_data = donor_data,
      dag = dag,
      record_id = record_id,
      strata = strata,
      immutable = TRUE
    )
  } else if (!inherits(donor_cache, "sspice_donor_cache")) {
    stop("donor_cache must be an sspice_donor_cache object.", call. = FALSE)
  } else if (!identical(donor_cache$strata, strata)) {
    stop("donor_cache strata do not match the requested strata.",
         call. = FALSE)
  }
  routing_oracle <- .sspice_build_routing_oracle(
    dag = dag,
    record_id = record_id,
    vaux_names = vaux_names,
    items_mapping_file = items_mapping_file
  )
  dependency_index <- .sspice_build_derived_dependency_index(
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    endogenous_vaux = endogenous_vaux
  )
  vaux_classification <- .sspice_classify_vaux(
    dependency_index = dependency_index,
    data_names = names(data),
    endogenous_vaux = endogenous_vaux,
    external_vaux = external_vaux,
    logical_group_vaux = logical_group_vaux
  )
  recomputable_vaux <- vaux_classification$recomputable
  mode_locked_vaux <- unique(c(
    vaux_classification$logical_groups,
    vaux_classification$frozen_by_canonicalization,
    vaux_classification$external
  ))
  external_frozen_vaux <- vaux_classification$external
  sampled_vaux <- recomputable_vaux
  recalculable_variables <- unique(edit_names)
  proposal_excluded_variables <- unique(c(
    immutable_variables,
    recalculable_variables,
    mode_locked_vaux
  ))
  if (length(intersect(immutable_variables, recalculable_variables)) ||
      length(intersect(immutable_variables, mode_locked_vaux)) ||
      length(intersect(recalculable_variables, mode_locked_vaux))) {
    stop("Correction variable classes must be disjoint.", call. = FALSE)
  }
  dependency_index <- .sspice_build_derived_dependency_index(
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    endogenous_vaux = recomputable_vaux
  )
  routing_oracle$dependency_index <- dependency_index
  dag$vaux_names <- vaux_names
  corrected <- data
  summaries <- vector("list", nrow(data))

  worker_context <- list(
    data = data,
    dag = dag,
    record_id = record_id,
    domains = derived_domains,
    donor_data = donor_data,
    donor_cache = donor_cache,
    strata = strata,
    imputation_method = imputation_method,
    priors = priors,
    proposal_excluded_variables = proposal_excluded_variables,
    control = control,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    external_data_dir = external_data_dir,
    recomputable_vaux = recomputable_vaux,
    sampled_vaux = sampled_vaux,
    external_frozen_vaux = external_frozen_vaux,
    mode_locked_vaux = mode_locked_vaux,
    vaux_classification = vaux_classification,
    items_mapping_file = items_mapping_file,
    routing_oracle = routing_oracle
  )

  use_parallel <- isTRUE(control$use_parallel %||% FALSE)
  batch_size <- control$batch_size %||% nrow(data)
  batch_size <- min(as.integer(batch_size), nrow(data))
  batches <- split(
    seq_len(nrow(data)),
    ceiling(seq_len(nrow(data)) / batch_size)
  )
  resume_batches <- isTRUE(control$resume_batches %||% FALSE)
  if (resume_batches && is.null(batch_output_dir)) {
    stop(
      "resume_batches = TRUE requires batch_output_dir.",
      call. = FALSE
    )
  }
  if (!is.null(batch_output_dir)) {
    if (length(batch_output_dir) != 1L || is.na(batch_output_dir) ||
        !nzchar(batch_output_dir)) {
      stop("batch_output_dir must be NULL or one non-empty path.",
           call. = FALSE)
    }
    dir.create(batch_output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  fingerprint_control <- control
  fingerprint_control[c(
    "use_parallel", "parallel_workers", "batch_size", "resume_batches"
  )] <- NULL
  checkpoint_fingerprint <- .sspice_checkpoint_fingerprint(list(
    data = data,
    dag = dag,
    donor_data = donor_data,
    donor_cache = donor_cache,
    vaux_metadata = vaux_metadata,
    logical_edit_metadata = logical_edit_metadata,
    strata = strata,
    imputation_method = imputation_method,
    priors = priors,
    domains = derived_domains,
    immutable_variables = immutable_variables,
    control = fingerprint_control
  ))
  manifest_rows <- vector("list", length(batches))
  record_rng_streams <- .sspice_record_rng_streams(
    seed = control$seed,
    n_records = nrow(data)
  )
  workers_used <- 1L
  cluster <- NULL
  cluster_open <- FALSE
  worker_packages <- NULL
  if (use_parallel) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Parallel correction requires package 'doParallel'.",
           call. = FALSE)
    }
    if (!requireNamespace("foreach", quietly = TRUE)) {
      stop("Parallel correction requires package 'foreach'.",
           call. = FALSE)
    }
    workers_used <- .sspice_parallel_worker_count(
      n_records = nrow(data),
      requested = control$parallel_workers %||% NULL
    )
    cluster <- parallel::makePSOCKcluster(workers_used)
    cluster_open <- TRUE
    on.exit({
      if (isTRUE(cluster_open)) {
        try(parallel::stopCluster(cluster), silent = TRUE)
      }
      foreach::registerDoSEQ()
    }, add = TRUE)
    doParallel::registerDoParallel(cluster)

    development_path <- NULL
    namespace_path <- tryCatch(
      getNamespaceInfo(asNamespace("SSPICE"), "path"),
      error = function(...) NULL
    )
    if (!is.null(namespace_path)) {
      source_file <- file.path(namespace_path, "R", "correct_data.R")
      description_file <- file.path(namespace_path, "DESCRIPTION")
      if (file.exists(source_file) && file.exists(description_file)) {
        package_name <- tryCatch(
          read.dcf(description_file, fields = "Package")[[1L]],
          error = function(...) NA_character_
        )
        if (identical(package_name, "SSPICE")) {
          development_path <- normalizePath(namespace_path, winslash = "/")
        }
      }
    }
    worker_packages <- "SSPICE"
    if (!is.null(development_path)) {
      parallel::clusterCall(
        cluster,
        function(path) {
          if (!requireNamespace("pkgload", quietly = TRUE)) {
            stop("Package 'pkgload' is required on development workers.")
          }
          pkgload::load_all(
            path = path,
            reset = TRUE,
            export_all = FALSE,
            helpers = FALSE,
            quiet = TRUE
          )
          NULL
        },
        development_path
      )
      worker_packages <- NULL
    }

    worker_exports <- new.env(parent = emptyenv())
    worker_exports$.sspice_batch_context <- worker_context
    worker_exports$.sspice_batch_correct_one <- .sspice_correct_one_record
    parallel::clusterExport(
      cluster,
      c(
        ".sspice_batch_context",
        ".sspice_batch_correct_one"
      ),
      envir = worker_exports
    )
  }

  for (batch_number in seq_along(batches)) {
    batch_started_at <- Sys.time()
    batch_indices <- batches[[batch_number]]
    checkpoint_file <- if (is.null(batch_output_dir)) NULL else file.path(
      batch_output_dir,
      sprintf("batch_%06d.rds", batch_number)
    )
    checkpoint <- NULL
    checkpoint_reused <- FALSE
    if (resume_batches && !is.null(checkpoint_file) &&
        file.exists(checkpoint_file)) {
      checkpoint <- tryCatch(readRDS(checkpoint_file), error = identity)
      if (inherits(checkpoint, "error") || !is.list(checkpoint) ||
          !identical(checkpoint$fingerprint, checkpoint_fingerprint) ||
          !identical(as.integer(checkpoint$record_indices),
                     as.integer(batch_indices)) ||
          !identical(
            as.character(checkpoint$record_identifiers),
            as.character(data[[record_id]][batch_indices])
          ) || !is.list(checkpoint$record_results) ||
          length(checkpoint$record_results) != length(batch_indices)) {
        stop(
          "Incompatible or incomplete checkpoint: ", checkpoint_file, ".",
          call. = FALSE
        )
      }
      record_results <- checkpoint$record_results
      checkpoint_reused <- TRUE
    } else if (use_parallel) {
      iterator <- foreach::foreach(
        index = batch_indices,
        rng_state = record_rng_streams[batch_indices],
        .inorder = TRUE,
        .packages = worker_packages,
        .noexport = c(
          ".sspice_batch_context",
          ".sspice_batch_correct_one"
        ),
        .options.snow = list(preschedule = FALSE)
      )
      record_results <- foreach::`%dopar%`(
        iterator,
        .sspice_batch_correct_one(
          index,
          .sspice_batch_context,
          rng_state
        )
      )
    } else {
      record_results <- lapply(
        batch_indices,
        function(index) {
          .sspice_correct_one_record(
            index,
            worker_context,
            record_rng_streams[[index]]
          )
        }
      )
    }

    if (!checkpoint_reused && !is.null(checkpoint_file)) {
      checkpoint <- list(
        batch_number = as.integer(batch_number),
        fingerprint = checkpoint_fingerprint,
        record_indices = as.integer(batch_indices),
        record_identifiers = data[[record_id]][batch_indices],
        record_results = record_results,
        completed_at = Sys.time()
      )
      .sspice_atomic_save_rds(checkpoint, checkpoint_file)
    }

    for (record_result in record_results) {
      index <- record_result$index
      identifier <- record_result$identifier
      corrected[index, ] <- record_result$corrected_record
      summaries[[index]] <- data.frame(
        record_identifier = identifier,
        final_routing_violations = record_result$final_routing_violations,
        final_logical_edit_violations =
          record_result$final_logical_edit_violations,
        final_vaux_violations = record_result$final_vaux_violations,
        final_energy = record_result$final_energy,
        resolved = record_result$resolved,
        stringsAsFactors = FALSE
      )
    }
    manifest_rows[[batch_number]] <- data.frame(
      batch_number = as.integer(batch_number),
      first_record = min(batch_indices),
      last_record = max(batch_indices),
      records = length(batch_indices),
      checkpoint = if (is.null(checkpoint_file)) NA_character_ else
        basename(checkpoint_file),
      reused = checkpoint_reused,
      fingerprint = checkpoint_fingerprint,
      elapsed_seconds = as.numeric(difftime(
        Sys.time(), batch_started_at, units = "secs"
      )),
      stringsAsFactors = FALSE
    )
    if (!is.null(batch_output_dir)) {
      .sspice_atomic_save_rds(
        do.call(rbind, manifest_rows[seq_len(batch_number)]),
        file.path(batch_output_dir, "batch_manifest.rds")
      )
    }
    message(
      "SSPICE correction batch ", batch_number, "/", length(batches),
      " completed (", length(batch_indices), " records",
      if (checkpoint_reused) ", checkpoint reused" else "",
      ")."
    )
    rm(record_results, checkpoint)
    gc(verbose = FALSE)
  }

  if (use_parallel) {
    parallel::stopCluster(cluster)
    cluster_open <- FALSE
    foreach::registerDoSEQ()
  }
  summary <- do.call(rbind, summaries)
  names(summary)[1L] <- record_id
  localized_routing_violations <- summary$final_routing_violations
  localized_logical_violations <- summary$final_logical_edit_violations
  localized_vaux_violations <- summary$final_vaux_violations

  # One canonical check, after record-level results have been recombined.
  # No canonical routing check is performed inside the MCMC-SA workers.
  mapping_name <- if (is.null(items_mapping_file)) NULL else
    basename(items_mapping_file)
  mapping_path <- if (is.null(items_mapping_file)) NULL else
    dirname(items_mapping_file)
  final_routing_check <- routing_check(
    dset_data = corrected,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = record_id,
    items.maps = mapping_name,
    path.items.maps = mapping_path,
    logical_edit_names = dag$logical_edit_names
  )
  final_report <- final_routing_check$full_report
  if (is.null(final_report)) final_report <- data.frame()
  if (nrow(final_report)) {
    report_ids <- as.character(final_report[[record_id]])
    report_sources <- if ("violation_source" %in% names(final_report)) {
      as.character(final_report$violation_source)
    } else {
      rep("ROUTING", nrow(final_report))
    }
    report_labels <- if ("varname_label" %in% names(final_report)) {
      as.character(final_report$varname_label)
    } else {
      rep("", nrow(final_report))
    }
    summary_ids <- as.character(summary[[record_id]])
    summary$final_routing_violations <- vapply(
      summary_ids,
      function(identifier) sum(
        report_ids == identifier &
          report_sources != "LOGICAL_EDIT" &
          !report_labels %in% vaux_names
      ),
      integer(1L)
    )
    summary$final_logical_edit_violations <- vapply(
      summary_ids,
      function(identifier) sum(
        report_ids == identifier & report_sources == "LOGICAL_EDIT"
      ),
      integer(1L)
    )
    canonical_vaux_violations <- vapply(
      summary_ids,
      function(identifier) sum(
        report_ids == identifier &
          report_sources != "LOGICAL_EDIT" &
          report_labels %in% vaux_names
      ),
      integer(1L)
    )
    summary$final_vaux_violations <- pmax(
      localized_vaux_violations,
      canonical_vaux_violations
    )
  } else {
    summary$final_routing_violations <- 0L
    summary$final_logical_edit_violations <- 0L
    summary$final_vaux_violations <- localized_vaux_violations
  }
  summary$resolved <-
    summary$final_routing_violations == 0L &
    summary$final_logical_edit_violations == 0L &
    summary$final_vaux_violations == 0L
  summary$final_energy <- summary$final_energy +
    control$routing_weight * (
      summary$final_routing_violations - localized_routing_violations
    ) +
    control$logical_edit_weight * (
      summary$final_logical_edit_violations - localized_logical_violations
    ) +
    control$vaux_weight * (
      summary$final_vaux_violations - localized_vaux_violations
    )
  structure(
    list(
      corrected_data = corrected,
      final_routing_check = final_routing_check,
      record_summary = summary,
      unresolved_records = summary[!summary$resolved, , drop = FALSE],
      donor_diagnostics = donor_validation[c("supplied", "retained")],
      control = control,
      immutable_variables = immutable_variables,
      recalculable_variables = recalculable_variables,
      sampled_vaux = sampled_vaux,
      mode_locked_vaux = mode_locked_vaux,
      frozen_external_vaux = external_frozen_vaux,
      strata = strata,
      imputation_method = imputation_method,
      vaux_classification = vaux_classification,
      batch_manifest = do.call(rbind, manifest_rows),
      parallel = list(
        requested = use_parallel,
        workers_used = workers_used,
        batch_size = batch_size,
        batches_total = length(batches),
        batches_reused = sum(vapply(
          manifest_rows,
          function(row) isTRUE(row$reused[[1L]]),
          logical(1L)
        )),
        checkpoint_directory = batch_output_dir,
        checkpoint_fingerprint = checkpoint_fingerprint
      )
    ),
    class = "sspice_correction_result"
  )
}
