make_extended_workflow_routing <- function() {
  data.frame(
    V1 = c("CASE_KEY", "ROOT", "MIDDLE", "END"),
    V2 = c(1L, 2L, 2L, 2L),
    V3 = c(1000L, 1L, 1L, 1L),
    V4 = c(9999L, 2L, 2L, 2L),
    V5 = c(0L, 0L, 0L, 0L),
    V6 = c(0L, 0L, 0L, 0L),
    V7 = c(NA, NA, NA, NA),
    V8 = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

make_extended_workflow_edits <- function() {
  data.frame(
    V1 = c(
      "LEDIT_A",
      "LEDIT_A",
      "LEDIT_B",
      "LEDIT_B"
    ),
    V2 = c("1", "2", "1", "2"),
    V3 = c("MIDDLE", "MIDDLE", "END", "END"),
    V4 = c("", "", "", ""),
    V5 = c("", "", "", ""),
    V6 = c("1", "2", "1", "2"),
    V7 = c("", "", "", ""),
    V8 = c("", "", "", ""),
    V9 = c("NULL", "NULL", "NULL", "NULL"),
    stringsAsFactors = FALSE
  )
}

make_empty_workflow_mapping <- function(directory) {
  mapping_file <- file.path(directory, "items_mapping.csv")

  utils::write.csv(
    data.frame(
      varname = character(),
      route = numeric(),
      dset = numeric()
    ),
    file = mapping_file,
    row.names = FALSE
  )

  mapping_file
}

build_test_extended_workflow <- function(n_records = 20L) {
  base_routing <- make_extended_workflow_routing()
  edit_metadata <- make_extended_workflow_edits()

  base_dag <- build_DAG(
    routing = base_routing,
    analyze = FALSE,
    stop_on_cycle = TRUE
  )

  base_data <- SSPICE:::generate_admissible_records(
    dag = base_dag,
    n_records = n_records,
    id_col = "CASE_KEY",
    seed = 2026L
  )

  extended_routing <-
    SSPICE:::extend_routing_with_logical_edits(
      routing = base_routing,
      logical_edits = edit_metadata
    )

  extended_dag <- build_DAG(
    routing = extended_routing,
    analyze = FALSE,
    stop_on_cycle = TRUE
  )

  extended_dag$logical_edit_names <- attr(
    extended_routing,
    "logical_edit_names"
  )

  extended_data <- SSPICE:::create_logical_edit_variables(
    inpdata = base_data,
    logical_edits_data = edit_metadata
  )

  list(
    base_routing = base_routing,
    base_dag = base_dag,
    base_data = base_data,
    edit_metadata = edit_metadata,
    extended_routing = extended_routing,
    extended_dag = extended_dag,
    extended_data = extended_data
  )
}

test_that("the complete workflow builds a canonical extended DAG", {
  workflow <- build_test_extended_workflow()

  expect_s3_class(workflow$base_dag, "sspice_dag")
  expect_s3_class(workflow$extended_dag, "sspice_dag")
  expect_true(workflow$base_dag$graph_diagnostics$is_DAG)
  expect_true(workflow$extended_dag$graph_diagnostics$is_DAG)
  expect_equal(
    length(workflow$extended_dag$varnames),
    length(workflow$base_dag$varnames) + 2L
  )
  expect_identical(
    tail(workflow$extended_dag$varnames, 2L),
    c("LEDIT_A", "LEDIT_B")
  )
  expect_identical(
    workflow$extended_dag$logical_edit_names,
    c("LEDIT_A", "LEDIT_B")
  )
  expect_true(
    nrow(workflow$extended_dag$Bt) >
      nrow(workflow$base_dag$Bt)
  )
})

test_that("logical edits are appended and aligned with the extended DAG", {
  workflow <- build_test_extended_workflow()

  expect_identical(
    names(workflow$extended_data),
    workflow$extended_dag$varnames
  )
  expect_equal(
    nrow(workflow$extended_data),
    nrow(workflow$base_data)
  )
  expect_identical(
    workflow$extended_data[, workflow$base_dag$varnames, drop = FALSE],
    workflow$base_data
  )
  expect_true(
    all(workflow$extended_data$LEDIT_A %in% c(1, 2))
  )
  expect_true(
    all(workflow$extended_data$LEDIT_B %in% c(1, 2))
  )
  expect_false(anyNA(workflow$extended_data$LEDIT_A))
  expect_false(anyNA(workflow$extended_data$LEDIT_B))
})

test_that("routing_check reports every missing LEDIT independently", {
  workflow <- build_test_extended_workflow(n_records = 20L)
  test_directory <- tempfile(
    pattern = "sspice-extended-workflow-"
  )
  dir.create(test_directory)
  on.exit(
    unlink(test_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- make_empty_workflow_mapping(test_directory)

  valid_result <- routing_check(
    dset_data = workflow$extended_data,
    routes = workflow$extended_dag$routes_numeric,
    Bt = workflow$extended_dag$Bt,
    varkey = "CASE_KEY",
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    logical_edit_names =
      workflow$extended_dag$logical_edit_names
  )

  expect_equal(nrow(valid_result$full_report), 0L)

  violated_data <- workflow$extended_data
  violated_data$LEDIT_A[c(3L, 7L)] <- NA
  violated_data$LEDIT_B[c(3L, 7L, 11L)] <- NA

  violated_result <- routing_check(
    dset_data = violated_data,
    routes = workflow$extended_dag$routes_numeric,
    Bt = workflow$extended_dag$Bt,
    varkey = "CASE_KEY",
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    logical_edit_names =
      workflow$extended_dag$logical_edit_names
  )

  logical_report <- violated_result$full_report[
    violated_result$full_report$violation_source ==
      "LOGICAL_EDIT",
    ,
    drop = FALSE
  ]

  expected <- data.frame(
    CASE_KEY = c(
      violated_data$CASE_KEY[c(3L, 7L)],
      violated_data$CASE_KEY[c(3L, 7L, 11L)]
    ),
    varname_label = c(
      rep("LEDIT_A", 2L),
      rep("LEDIT_B", 3L)
    ),
    stringsAsFactors = FALSE
  )

  observed_keys <- sort(
    paste(
      logical_report$CASE_KEY,
      logical_report$varname_label,
      sep = "|"
    )
  )
  expected_keys <- sort(
    paste(
      expected$CASE_KEY,
      expected$varname_label,
      sep = "|"
    )
  )

  expect_identical(observed_keys, expected_keys)
  expect_equal(nrow(logical_report), 5L)
  expect_false(anyDuplicated(observed_keys) > 0L)
  expect_equal(
    sum(logical_report$varname_label == "LEDIT_A"),
    2L
  )
  expect_equal(
    sum(logical_report$varname_label == "LEDIT_B"),
    3L
  )
})
