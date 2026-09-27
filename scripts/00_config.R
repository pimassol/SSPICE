# ============================================================
# SSPICE PACKAGE CONFIGURATION
# ============================================================

config <- list(
  routing_file = file.path("data-raw", "metadata", "routing_file.csv"),
  vaux_file = file.path("data-raw", "metadata", "vaux_rules.csv"),
  logical_edit_rules_file = file.path(
    "data-raw", "metadata", "logical_edit_rules.csv"
  ),
  items_mapping_file = file.path(
    "data-raw", "metadata", "items_mapping.csv"
  ),
  questionnaire_data_file = file.path(
    "data-raw", "input", "questionnaire_data.csv"
  ),
  violated_data_file = file.path(
    "data-raw", "input", "synthetic_data_violations.csv"
  ),
  violation_report_file = file.path(
    "data-raw", "input", "report_violations_by_ID_RECORD.csv"
  ),
  external_data_dir = file.path("data-raw", "external"),
  dag_output_dir = file.path("output", "DAG"),
  prepared_data_output_dir = file.path("output", "prepared-data"),
  routing_check_output_dir = file.path("output", "routing-check"),
  extended_dag_output_dir = file.path("output", "extended-DAG"),
  rules_output_dir = file.path("output", "rules"),
  horn_validation_output_dir = file.path("output", "horn-validation"),
  localization_validation_output_dir = file.path(
    "output", "localization-validation"
  ),
  metadata_separator = ";",
  data_separator = ",",
  missing_codes = c("", ".", "NA", "NULL"),
  record_id = "DB030",
  validation_record_id = "ID_RECORD",
  random_seed = 123L,
  n_random_closure_tests = 5000L,
  max_seed_size = 8L,
  add_value_to_filled_bridges = TRUE,
  max_memo_states = 5000000L,
  root_variables = NULL,
  max_paths_per_partition = 1e6,
  correction_strata = "FPROV",
  correction_immutable_variables = character(),
  correction_record_limit = 100L,
  correction_donor_data_file = NULL,
  correction_donor_data_validated = FALSE,
  correction_initial_check_file = file.path(
    "output",
    "extended-routing-check",
    "extended_routing_check_results.rds"
  ),
  correction_cache_policy = "auto",
  correction_priors_file = NULL,
  correction_domains_file = NULL,
  correction_imputation_method = "donor",
  correction_output_dir = file.path(
    "output",
    "corrected-data-pilot-100"
  ),
  correction_control_args = list(
    lambda_change = 2,
    future_weight = 0.50,
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
    minimum_donors = 1L,
    allow_unconditional_donor = TRUE,
    branch_fill_threshold = 0.5,
    freeze_strata = TRUE,
    seed = 123L,
    use_parallel = FALSE,
    parallel_workers = NULL
  )
)
