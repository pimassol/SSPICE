make_formal_test_routes <- function() {
  data.frame(
    V1 = 1:3,
    V2 = c(2, 2, 2),
    V3 = c(1, 1, 1),
    V4 = c(2, 2, 2),
    V5 = c(0, 0, 0),
    V6 = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
}

make_formal_test_bt <- function() {
  bt <- matrix(
    0,
    nrow = 6,
    ncol = 3,
    dimnames = list(
      NULL,
      c("ROOT", "MIDDLE", "END")
    )
  )

  bt[1:2, "ROOT"] <- 1
  bt[1:2, "MIDDLE"] <- -1
  bt[3:4, "MIDDLE"] <- 1
  bt[3:4, "END"] <- -1
  bt[5:6, "END"] <- 1
  bt
}

write_test_item_mapping <- function(directory, mapped = FALSE) {
  mapping_file <- file.path(
    directory,
    "items_mapping.csv"
  )

  if (mapped) {
    mapping <- data.frame(
      varname = c("ROOT", "ROOT"),
      route = c(1, 2),
      old = c(10, 20),
      stringsAsFactors = FALSE
    )
  } else {
    mapping <- data.frame(
      varname = character(),
      route = numeric(),
      old = numeric(),
      stringsAsFactors = FALSE
    )
  }

  utils::write.csv(
    mapping,
    mapping_file,
    row.names = FALSE,
    na = ""
  )

  mapping_file
}

test_that("create_vardom returns domains and a fixed-width layout", {
  output_directory <- tempfile(
    pattern = "sspice-vardom-test-"
  )
  dir.create(output_directory)
  on.exit(
    unlink(output_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- write_test_item_mapping(
    output_directory
  )

  testthat::local_mocked_bindings(
    generate_stree = function(Bt, routes_df) {
      list(
        stree = NULL,
        list = NULL
      )
    },
    calculate_flows = function(stree, list, routes_df, Bt) {
      data.frame(
        node = 1:3,
        flow = c(1, 0.5, 1)
      )
    },
    .package = "SSPICE"
  )

  result <- SSPICE:::create_vardom(
    routes_df = make_formal_test_routes(),
    Bt = make_formal_test_bt(),
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    output.path = output_directory
  )

  expect_type(result, "list")
  expect_named(result, c("vardom", "record_layout"))
  expect_equal(nrow(result$vardom), 3L)
  expect_equal(nrow(result$record_layout), 3L)
  expect_identical(
    result$record_layout$varname,
    c("ROOT", "MIDDLE", "END")
  )
  expect_equal(result$record_layout$position, c(1L, 2L, 3L))
  expect_equal(result$record_layout$length, c(1L, 1L, 1L))
  expect_equal(result$vardom$blank_allowed, c(0, 1, 0))

  expect_true(
    file.exists(
      file.path(output_directory, "VARDOM.dat")
    )
  )
  expect_true(
    file.exists(
      file.path(output_directory, "record_layout.csv")
    )
  )
  expect_false(
    file.exists(
      file.path(output_directory, "STRUTT.dat")
    )
  )
  expect_false(
    file.exists(
      file.path(output_directory, "LISTE.dat")
    )
  )
})

test_that("create_vardom writes one record for every DAG variable", {
  output_directory <- tempfile(
    pattern = "sspice-vardom-records-"
  )
  dir.create(output_directory)
  on.exit(
    unlink(output_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- write_test_item_mapping(
    output_directory
  )

  testthat::local_mocked_bindings(
    generate_stree = function(Bt, routes_df) {
      list(
        stree = NULL,
        list = NULL
      )
    },
    calculate_flows = function(stree, list, routes_df, Bt) {
      data.frame(
        node = 1:3,
        flow = rep(1, 3)
      )
    },
    .package = "SSPICE"
  )

  result <- SSPICE:::create_vardom(
    routes_df = make_formal_test_routes(),
    Bt = make_formal_test_bt(),
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    output.path = output_directory
  )

  stored_records <- readLines(
    file.path(output_directory, "VARDOM.dat"),
    warn = FALSE
  )

  expect_equal(length(stored_records), 3L)
  expect_identical(stored_records, result$vardom$record)
  expect_true(startsWith(stored_records[1L], "ROOT"))
  expect_true(startsWith(stored_records[2L], "MIDDLE"))
  expect_true(startsWith(stored_records[3L], "END"))
})

test_that("create_rules generates the expected atomic rules", {
  output_directory <- tempfile(
    pattern = "sspice-rules-test-"
  )
  dir.create(output_directory)
  on.exit(
    unlink(output_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- write_test_item_mapping(
    output_directory
  )

  rules <- SSPICE:::create_rules(
    Bt = make_formal_test_bt(),
    routes_df = make_formal_test_routes(),
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    path_output = output_directory
  )

  expected_rules <- c(
    "ROOT(1) MIDDLE< )",
    "ROOT(2) MIDDLE< )",
    "MIDDLE(1) END< )",
    "MIDDLE(2) END< )"
  )

  expect_identical(rules, expected_rules)
  expect_true(
    file.exists(
      file.path(output_directory, "RULES.dat")
    )
  )
  expect_identical(
    readLines(
      file.path(output_directory, "RULES.dat"),
      warn = FALSE
    ),
    expected_rules
  )
  expect_false(
    file.exists(
      file.path(output_directory, "REGOLE.dat")
    )
  )
  expect_false(
    file.exists(
      file.path(output_directory, "STRUTT.dat")
    )
  )
  expect_false(
    file.exists(
      file.path(output_directory, "LISTE.dat")
    )
  )
})

test_that("create_rules applies item mappings to rule antecedents", {
  output_directory <- tempfile(
    pattern = "sspice-rules-mapping-"
  )
  dir.create(output_directory)
  on.exit(
    unlink(output_directory, recursive = TRUE),
    add = TRUE
  )

  mapping_file <- write_test_item_mapping(
    output_directory,
    mapped = TRUE
  )

  rules <- SSPICE:::create_rules(
    Bt = make_formal_test_bt(),
    routes_df = make_formal_test_routes(),
    items.maps = basename(mapping_file),
    path.items.maps = dirname(mapping_file),
    path_output = output_directory
  )

  expect_true("ROOT(10) MIDDLE< )" %in% rules)
  expect_true("ROOT(20) MIDDLE< )" %in% rules)
  expect_false("ROOT(1) MIDDLE< )" %in% rules)
  expect_false("ROOT(2) MIDDLE< )" %in% rules)
})

test_that("formal artifact generators reject missing mapping files", {
  missing_directory <- tempfile(
    pattern = "sspice-missing-mapping-"
  )
  dir.create(missing_directory)
  on.exit(
    unlink(missing_directory, recursive = TRUE),
    add = TRUE
  )

  expect_error(
    SSPICE:::create_vardom(
      routes_df = make_formal_test_routes(),
      Bt = make_formal_test_bt(),
      items.maps = "missing.csv",
      path.items.maps = missing_directory
    ),
    "Item-mapping file not found"
  )

  expect_error(
    SSPICE:::create_rules(
      Bt = make_formal_test_bt(),
      routes_df = make_formal_test_routes(),
      items.maps = "missing.csv",
      path.items.maps = missing_directory
    ),
    "Item-mapping file not found"
  )
})
