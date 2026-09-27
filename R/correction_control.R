#' Configure SSPICE data correction
#'
#' @param routing_weight,logical_edit_weight,vaux_weight Non-negative energy weights.
#' @param lambda_change Weight assigned to minimum-change distance.
#' @param future_weight Weight assigned to the local look-ahead cost.
#' @param penalty_fill,penalty_blank,penalty_flip Costs of elementary changes.
#' @param initial_temperature Optional initial SA temperature.
#' @param final_temperature Positive final SA temperature.
#' @param cooling_fraction Fraction of the chain used for linear cooling.
#'   The remaining iterations use the constant final temperature.
#' @param max_iterations Optional maximum iterations per record.
#' @param top_k Maximum number of proposed modes per node.
#' @param beta_fill Strength of the original "lucid" routing-improvement
#'   tilt applied to empirical mode frequencies. Zero gives pure empirical
#'   sampling.
#' @param max_nodes_per_iteration Maximum local nodes explored at each MCMC
#'   iteration. The default reproduces the original node-by-node strategy.
#' @param logical_repair_max_evaluations Maximum candidate assignments checked
#'   by the generic local repair search for one violated logical edit.
#' @param logical_repair_max_changes Maximum number of base variables changed
#'   by one generic logical-edit repair proposal.
#' @param logical_repair_beam_width Maximum retained values per variable when
#'   the repair search must consider more than one simultaneous change.
#' @param minimum_donors Minimum stratum observations before backing off.
#' @param allow_unconditional_donor Whether the original marginal donor
#'   fallback may be used after conditional pools fail.
#' @param branch_fill_threshold Filled share above which a gate-flip proposal
#'   is generated for a skipped segment.
#' @param freeze_strata Whether stratum variables are fixed.
#' @param seed Random seed.
#' @param use_parallel Whether records are corrected in parallel with a PSOCK
#'   cluster registered through `doParallel`. The default preserves the
#'   sequential correction path.
#' @param parallel_workers Optional positive integer number of workers. When
#'   `NULL` and `use_parallel = TRUE`, SSPICE uses
#'   `max(1, parallel::detectCores(logical = TRUE) - 2)`, capped at the number
#'   of records being corrected. This argument is mainly useful for testing or
#'   for explicitly limiting memory use.
#' @param batch_size Optional positive integer number of records collected per
#'   parallel batch. `NULL` preserves the previous all-at-once behaviour.
#' @param resume_batches Whether compatible completed batch checkpoints may be
#'   reused when `correct_data()` receives a `batch_output_dir`.
#' @return A list of class `sspice_correction_control`.
#' @export
correction_control <- function(
    routing_weight = 1,
    logical_edit_weight = 1,
    vaux_weight = 1,
    lambda_change = 2,
    future_weight = 0.5,
    penalty_fill = 1,
    penalty_blank = 2,
    penalty_flip = 3,
    initial_temperature = NULL,
    final_temperature = 1e-4,
    cooling_fraction = 0.90,
    max_iterations = NULL,
    top_k = 10L,
    beta_fill = 1,
    max_nodes_per_iteration = 1L,
    logical_repair_max_evaluations = 1000L,
    logical_repair_max_changes = 2L,
    logical_repair_beam_width = 10L,
    minimum_donors = 1L,
    allow_unconditional_donor = TRUE,
    branch_fill_threshold = 0.5,
    freeze_strata = TRUE,
    seed = 1234L,
    use_parallel = FALSE,
    parallel_workers = NULL,
    batch_size = NULL,
    resume_batches = FALSE) {
  numeric_values <- c(
    routing_weight, logical_edit_weight, vaux_weight, lambda_change,
    future_weight, penalty_fill, penalty_blank, penalty_flip,
    final_temperature, beta_fill
  )
  if (any(!is.finite(numeric_values)) || any(numeric_values < 0) ||
      final_temperature <= 0) {
    stop("Correction weights and temperatures must be finite and non-negative.",
         call. = FALSE)
  }
  if (!is.null(initial_temperature) &&
      (!is.numeric(initial_temperature) || length(initial_temperature) != 1L ||
       !is.finite(initial_temperature) || initial_temperature <= 0)) {
    stop("initial_temperature must be a finite positive number.",
         call. = FALSE)
  }
  if (!is.numeric(cooling_fraction) || length(cooling_fraction) != 1L ||
      !is.finite(cooling_fraction) || cooling_fraction <= 0 ||
      cooling_fraction >= 1) {
    stop("cooling_fraction must be strictly between zero and one.",
         call. = FALSE)
  }
  if (!is.null(max_iterations) &&
      (!is.numeric(max_iterations) || length(max_iterations) != 1L ||
       !is.finite(max_iterations) || max_iterations < 1 ||
       max_iterations != as.integer(max_iterations))) {
    stop("max_iterations must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(max_nodes_per_iteration) ||
      length(max_nodes_per_iteration) != 1L ||
      !is.finite(max_nodes_per_iteration) ||
      max_nodes_per_iteration < 1L ||
      max_nodes_per_iteration != as.integer(max_nodes_per_iteration)) {
    stop("max_nodes_per_iteration must be a positive integer.", call. = FALSE)
  }
  integer_controls <- list(
    logical_repair_max_evaluations = logical_repair_max_evaluations,
    logical_repair_max_changes = logical_repair_max_changes,
    logical_repair_beam_width = logical_repair_beam_width
  )
  invalid_integer_controls <- names(integer_controls)[vapply(
    integer_controls,
    function(value) {
      !is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1L || value != as.integer(value)
    },
    logical(1L)
  )]
  if (length(invalid_integer_controls)) {
    stop(
      paste(invalid_integer_controls, collapse = ", "),
      " must be positive integers.",
      call. = FALSE
    )
  }
  if (!is.finite(branch_fill_threshold) || branch_fill_threshold < 0 ||
      branch_fill_threshold > 1) {
    stop("branch_fill_threshold must be between zero and one.", call. = FALSE)
  }
  if (!is.logical(use_parallel) || length(use_parallel) != 1L ||
      is.na(use_parallel)) {
    stop("use_parallel must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(parallel_workers) &&
      (!is.numeric(parallel_workers) || length(parallel_workers) != 1L ||
       !is.finite(parallel_workers) || parallel_workers < 1L ||
       parallel_workers != as.integer(parallel_workers))) {
    stop("parallel_workers must be NULL or a positive integer.",
         call. = FALSE)
  }
  if (!is.null(batch_size) &&
      (!is.numeric(batch_size) || length(batch_size) != 1L ||
       !is.finite(batch_size) || batch_size < 1L ||
       batch_size != as.integer(batch_size))) {
    stop("batch_size must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.logical(resume_batches) || length(resume_batches) != 1L ||
      is.na(resume_batches)) {
    stop("resume_batches must be TRUE or FALSE.", call. = FALSE)
  }
  structure(
    list(
      routing_weight = routing_weight,
      logical_edit_weight = logical_edit_weight,
      vaux_weight = vaux_weight,
      lambda_change = lambda_change,
      future_weight = future_weight,
      penalty_fill = penalty_fill,
      penalty_blank = penalty_blank,
      penalty_flip = penalty_flip,
      initial_temperature = initial_temperature,
      final_temperature = final_temperature,
      cooling_fraction = cooling_fraction,
      max_iterations = if (is.null(max_iterations)) NULL else
        as.integer(max_iterations),
      top_k = as.integer(top_k),
      beta_fill = as.numeric(beta_fill),
      max_nodes_per_iteration = as.integer(max_nodes_per_iteration),
      logical_repair_max_evaluations =
        as.integer(logical_repair_max_evaluations),
      logical_repair_max_changes = as.integer(logical_repair_max_changes),
      logical_repair_beam_width = as.integer(logical_repair_beam_width),
      minimum_donors = as.integer(minimum_donors),
      allow_unconditional_donor = isTRUE(allow_unconditional_donor),
      branch_fill_threshold = branch_fill_threshold,
      freeze_strata = isTRUE(freeze_strata),
      seed = as.integer(seed),
      use_parallel = use_parallel,
      parallel_workers = if (is.null(parallel_workers)) NULL else
        as.integer(parallel_workers),
      batch_size = if (is.null(batch_size)) NULL else as.integer(batch_size),
      resume_batches = resume_batches
    ),
    class = "sspice_correction_control"
  )
}
