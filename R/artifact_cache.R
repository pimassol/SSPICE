#' Inspect or build a cached routing-check artifact
#'
#' The cache is reused only when its manifest matches the current input files,
#' package version, cache schema and routing-check parameters. Input files are
#' fingerprinted with MD5 checksums.
#'
#' @param data Record-level data passed to [routing_check()].
#' @param dag Extended SSPICE DAG.
#' @param record_id Name of the record identifier.
#' @param logical_edit_names Optional logical-edit node names.
#' @param items_mapping_file Optional items-mapping file.
#' @param cache_file RDS file containing the cached routing-check result.
#' @param input_files Named character vector of files on which the artifact
#'   depends.
#' @param cache_policy One of `"auto"`, `"refresh"`, `"readonly"`, or
#'   `"off"`.
#' @param check_function Optional routing-check implementation. Intended for
#'   package testing; by default SSPICE's `routing_check()` is used.
#' @return A list containing `result`, `manifest`, and cache diagnostics.
#' @export
cached_routing_check <- function(
    data,
    dag,
    record_id,
    logical_edit_names = NULL,
    items_mapping_file = NULL,
    cache_file,
    input_files,
    cache_policy = c("auto", "refresh", "readonly", "off"),
    check_function = NULL) {
  cache_policy <- match.arg(cache_policy)
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (!is.list(dag) || is.null(dag$routes_numeric) || is.null(dag$Bt)) {
    stop("dag must contain routes_numeric and Bt.", call. = FALSE)
  }
  if (!is.character(record_id) || length(record_id) != 1L ||
      is.na(record_id) || !nzchar(record_id) ||
      !record_id %in% names(data)) {
    stop("record_id must identify one data column.", call. = FALSE)
  }
  if (!is.character(cache_file) || length(cache_file) != 1L ||
      is.na(cache_file) || !nzchar(cache_file)) {
    stop("cache_file must be one non-empty path.", call. = FALSE)
  }
  input_files <- .sspice_normalize_artifact_inputs(input_files)
  fingerprints <- .sspice_artifact_fingerprints(input_files)
  parameters <- list(
    record_id = record_id,
    logical_edit_names = sort(unique(as.character(
      if (is.null(logical_edit_names)) {
        character()
      } else {
        logical_edit_names
      }
    ))),
    items_mapping_file = if (is.null(items_mapping_file)) "" else
      normalizePath(items_mapping_file, winslash = "/", mustWork = TRUE)
  )
  manifest_file <- .sspice_artifact_manifest_file(cache_file)
  status <- .sspice_routing_cache_status(
    cache_file = cache_file,
    manifest_file = manifest_file,
    fingerprints = fingerprints,
    parameters = parameters
  )
  if (cache_policy %in% c("auto", "readonly") && isTRUE(status$valid)) {
    cached_result <- tryCatch(readRDS(cache_file), error = identity)
    if (!inherits(cached_result, "error")) {
      return(structure(
        list(
          result = cached_result,
          manifest = status$manifest,
          cache = list(
            policy = cache_policy,
            used = TRUE,
            valid = TRUE,
            refreshed = FALSE,
            reason = "valid_manifest",
            cache_file = cache_file,
            manifest_file = manifest_file
          )
        ),
        class = "sspice_cached_artifact"
      ))
    }
    status <- list(
      valid = FALSE,
      reason = paste0("cache_read_error: ", conditionMessage(cached_result)),
      manifest = status$manifest
    )
  }
  if (identical(cache_policy, "readonly")) {
    stop(
      "No valid read-only routing-check cache is available: ",
      status$reason,
      call. = FALSE
    )
  }
  if (is.null(check_function)) {
    check_function <- get("routing_check", envir = asNamespace("SSPICE"))
  }
  if (!is.function(check_function)) {
    stop("check_function must be a function.", call. = FALSE)
  }
  mapping_name <- if (is.null(items_mapping_file)) NULL else
    basename(items_mapping_file)
  mapping_path <- if (is.null(items_mapping_file)) NULL else
    dirname(items_mapping_file)
  started_at <- Sys.time()
  result <- check_function(
    dset_data = data,
    routes = dag$routes_numeric,
    Bt = dag$Bt,
    varkey = record_id,
    items.maps = mapping_name,
    path.items.maps = mapping_path,
    logical_edit_names = logical_edit_names
  )
  completed_at <- Sys.time()
  manifest <- .sspice_new_routing_cache_manifest(
    fingerprints = fingerprints,
    parameters = parameters,
    result = result,
    data = data,
    created_at = completed_at,
    elapsed_seconds = as.numeric(difftime(
      completed_at, started_at, units = "secs"
    ))
  )
  save_cache <- cache_policy %in% c("auto", "refresh")
  if (save_cache) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    .sspice_atomic_save_rds(result, cache_file)
    .sspice_atomic_save_rds(manifest, manifest_file)
  }
  reason <- if (identical(cache_policy, "off")) {
    "cache_disabled"
  } else if (identical(cache_policy, "refresh")) {
    "refresh_requested"
  } else {
    paste0("cache_rebuilt: ", status$reason)
  }
  structure(
    list(
      result = result,
      manifest = manifest,
      cache = list(
        policy = cache_policy,
        used = FALSE,
        valid = TRUE,
        refreshed = save_cache,
        reason = reason,
        cache_file = cache_file,
        manifest_file = manifest_file
      )
    ),
    class = "sspice_cached_artifact"
  )
}

.sspice_cache_schema_version <- 1L

.sspice_artifact_manifest_file <- function(cache_file) {
  if (grepl("[.]rds$", cache_file, ignore.case = TRUE)) {
    sub("[.]rds$", ".manifest.rds", cache_file, ignore.case = TRUE)
  } else {
    paste0(cache_file, ".manifest.rds")
  }
}

.sspice_normalize_artifact_inputs <- function(input_files) {
  if (is.list(input_files)) input_files <- unlist(input_files, use.names = TRUE)
  input_names <- names(input_files)
  input_files <- as.character(input_files)
  names(input_files) <- input_names
  keep <- !is.na(input_files) & nzchar(input_files)
  input_files <- input_files[keep]
  if (!length(input_files)) {
    stop("input_files must contain at least one file.", call. = FALSE)
  }
  if (is.null(names(input_files)) || any(!nzchar(names(input_files)))) {
    stop("input_files must be a named character vector.", call. = FALSE)
  }
  if (anyDuplicated(names(input_files))) {
    stop("input_files names must be unique.", call. = FALSE)
  }
  missing <- input_files[!file.exists(input_files)]
  if (length(missing)) {
    stop(
      "Artifact input files do not exist: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  input_files
}

.sspice_artifact_fingerprints <- function(input_files) {
  information <- file.info(input_files)
  data.frame(
    role = names(input_files),
    path = normalizePath(input_files, winslash = "/", mustWork = TRUE),
    size = as.numeric(information$size),
    modified = format(
      information$mtime,
      "%Y-%m-%dT%H:%M:%OS6%z"
    ),
    md5 = unname(tools::md5sum(input_files)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.sspice_comparable_fingerprints <- function(fingerprints) {
  fingerprints[c("role", "path", "size", "md5")]
}

.sspice_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("SSPICE")),
    error = function(error) "development"
  )
}

.sspice_new_routing_cache_manifest <- function(
    fingerprints,
    parameters,
    result,
    data,
    created_at,
    elapsed_seconds) {
  report <- result$full_report
  invalid_ids <- character()
  if (is.data.frame(report) && nrow(report) &&
      parameters$record_id %in% names(report)) {
    invalid_ids <- unique(as.character(report[[parameters$record_id]]))
    invalid_ids <- invalid_ids[!is.na(invalid_ids)]
  }
  list(
    artifact_type = "routing_check",
    schema_version = .sspice_cache_schema_version,
    package = "SSPICE",
    package_version = .sspice_package_version(),
    created_at = created_at,
    inputs = fingerprints,
    parameters = parameters,
    result = list(
      records_available = nrow(data),
      records_exact = nrow(data) - length(invalid_ids),
      records_erroneous = length(invalid_ids),
      violations = if (is.data.frame(report)) nrow(report) else 0L,
      elapsed_seconds = elapsed_seconds
    )
  )
}

.sspice_routing_cache_status <- function(
    cache_file,
    manifest_file,
    fingerprints,
    parameters) {
  if (!file.exists(cache_file)) {
    return(list(valid = FALSE, reason = "cache_file_missing", manifest = NULL))
  }
  if (!file.exists(manifest_file)) {
    return(list(valid = FALSE, reason = "manifest_file_missing", manifest = NULL))
  }
  manifest <- tryCatch(readRDS(manifest_file), error = identity)
  if (inherits(manifest, "error")) {
    return(list(
      valid = FALSE,
      reason = paste0("manifest_read_error: ", conditionMessage(manifest)),
      manifest = NULL
    ))
  }
  if (!identical(manifest$artifact_type, "routing_check")) {
    return(list(valid = FALSE, reason = "artifact_type_mismatch", manifest = manifest))
  }
  if (!identical(manifest$schema_version, .sspice_cache_schema_version)) {
    return(list(valid = FALSE, reason = "schema_version_mismatch", manifest = manifest))
  }
  if (!identical(manifest$package_version, .sspice_package_version())) {
    return(list(valid = FALSE, reason = "package_version_mismatch", manifest = manifest))
  }
  if (!identical(manifest$parameters, parameters)) {
    return(list(valid = FALSE, reason = "parameter_mismatch", manifest = manifest))
  }
  if (!identical(
    .sspice_comparable_fingerprints(manifest$inputs),
    .sspice_comparable_fingerprints(fingerprints)
  )) {
    return(list(valid = FALSE, reason = "input_fingerprint_mismatch", manifest = manifest))
  }
  list(valid = TRUE, reason = "valid_manifest", manifest = manifest)
}

.sspice_atomic_save_rds <- function(object, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(basename(path), "-"),
    tmpdir = directory,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary)
  copied <- file.copy(temporary, path, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Unable to write cached artifact: ", path, ".", call. = FALSE)
  }
  invisible(path)
}
