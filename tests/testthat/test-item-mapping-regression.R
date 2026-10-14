# Independent expected paths: category 2 skips DETAIL; categories 1/3 visit it.
testthat::test_that("mapping preserves path semantics, domains and correction", {
  internal <- function(name) getFromNamespace(name, "SSPICE")
  cases <- list(
    BASE = list(route = 1:3, observed = 1:3, mapped = FALSE),
    EQUIDISTANT = list(route = c(10,20,30), observed = c(10,20,30), mapped = FALSE),
    MAPPED_123 = list(route = 1:3, observed = c(10,21,99), mapped = TRUE),
    MAPPED_SPACED = list(route = c(10,20,30), observed = c(10,21,99), mapped = TRUE),
    OVERLAP = list(route = 1:3, observed = c(2,3,4), mapped = TRUE)
  )
  keys <- function(report) {
    if (!nrow(report)) return(character())
    sort(paste(report$ID, report$varname_label, report$errcode, sep = "|"))
  }
  expected_keys <- sort(c("4|DETAIL|-1", "5|DETAIL|1", "6|DETAIL|-1"))
  for (label in names(cases)) {
    z <- cases[[label]]
    file <- tempfile(fileext = ".csv")
    on.exit(unlink(file), add = TRUE)
    mapping <- data.frame(varname = character(), item_route = numeric(),
                          item_dataset = numeric())
    if (z$mapped) mapping <- data.frame(varname = "CHOICE",
      item_route = z$route, item_dataset = z$observed)
    utils::write.csv(mapping, file, row.names = FALSE)
    mapping_file_before <- readLines(file, warn = FALSE)
    routes <- data.frame(V1 = c("ID","CHOICE","DETAIL","END"),
      V2 = c(1,3,1,1), V3 = c(1,min(z$route),1,1),
      V4 = c(9999,max(z$route),1,1), V5 = 0, V6 = c(0,1,0,0),
      V7 = c(NA,z$route[2],NA,NA), V8 = c(NA,"END",NA,NA))
    dag <- SSPICE::build_DAG(routes, analyze = FALSE)
    original <- data.frame(ID = 1:6, CHOICE = rep(z$observed,2),
                           DETAIL = c(1,NA,1,NA,1,NA), END = 1)
    data <- SSPICE::prepare_data(original, dag)$data
    saved <- serialize(list(data, dag), NULL)
    map <- internal(".sspice_read_item_mapping")(file)
    oracle <- internal(".sspice_build_routing_oracle")(
      dag, "ID", items_mapping_file = file)
    checker <- internal("check_all_paths")
    probe <- new.env(parent = environment(checker))
    probe$positions <- integer()
    original_dummy <- internal("dummize_vector")
    probe$dummize_vector <- function(x_row, y) {
      dummy <- original_dummy(x_row, y)
      probe$positions <- c(probe$positions, which(dummy[2:4] == 1))
      dummy
    }
    environment(checker) <- probe
    invisible(checker(data, dag$routes_numeric, dag$Bt, "ID", basename(file), dirname(file)))
    testthat::expect_equal(probe$positions, rep(1:3,2), info = label)
    canonical <- SSPICE::routing_check(data, dag$routes_numeric, dag$Bt,
      "ID", basename(file), dirname(file))$full_report
    testthat::expect_identical(keys(canonical), expected_keys, info = label)
    testthat::expect_equal(canonical$routeval, z$observed, info = label)
    fast <- lapply(seq_len(6L), function(i) {
      dummy <- internal(".sspice_oracle_dummy")(data[i,,drop = FALSE], oracle)
      testthat::expect_equal(which(dummy[2:4] == 1), ((i-1L) %% 3L)+1L, info = label)
      internal(".sspice_fast_routing_state")(data[i,,drop = FALSE], oracle)$full_report
    })
    testthat::expect_identical(keys(do.call(rbind, fast)), expected_keys, info = label)
    absent <- data[1,,drop = FALSE]; absent$CHOICE <- NA_real_
    testthat::expect_equal(internal(".sspice_oracle_dummy")(absent,oracle)[2:4], c(0,0,0))
    domains <- internal(".sspice_domains_from_routes")(
      dag$routes_numeric, dag$varnames, item_mapping = map)
    testthat::expect_equal(domains$CHOICE, z$observed, info = label)
    # Gate helpers receive real data codes, including mode changes during proposals.
    mapped_dag <- dag; mapped_dag$item_mapping <- map
    for (i in 1:3) {
      profile <- internal(".sspice_mode_jump_profile")(
        "CHOICE", z$observed[i], data[i,,drop = FALSE], mapped_dag)
      testthat::expect_equal(profile$target, if (i == 2L) 4L else 3L, info = label)
      predecessors <- internal(".sspice_structural_predecessors")(
        "DETAIL", data[i,,drop = FALSE], mapped_dag)
      testthat::expect_identical("CHOICE" %in% predecessors, i != 2L, info = label)
    }
    rules <- internal("create_rules")(dag$Bt, dag$routes_numeric, basename(file), dirname(file))
    expected_rules <- c(paste0("CHOICE(",z$observed[1],") DETAIL< )"),
      paste0("CHOICE(",z$observed[2],") DETAIL() END< )"),
      paste0("CHOICE(",z$observed[3],") DETAIL< )"))
    testthat::expect_setequal(rules[startsWith(rules,"CHOICE(")], expected_rules)
    v <- internal("create_vardom")(dag$routes_numeric,dag$Bt,basename(file),dirname(file))$vardom
    v <- v[v$varname == "CHOICE",,drop = FALSE]
    if (z$mapped || label == "EQUIDISTANT") {
      testthat::expect_identical(as.character(v$intervalli),
        paste0(sprintf("%04d%04d",z$observed,z$observed),collapse = ""))
    } else {
      testthat::expect_equal(as.numeric(v$minitem),1)
      testthat::expect_equal(as.numeric(v$maxitem),3)
    }
    corrected <- SSPICE::correct_data(data[4:6,,drop = FALSE],dag,record_id = "ID",
      donor_data = data[1:3,,drop = FALSE],immutable_variables = c("CHOICE","END"),
      items_mapping_file = file,control = SSPICE::correction_control(
        seed = 123L,max_iterations = 12L,use_parallel = FALSE))
    testthat::expect_equal(corrected$corrected_data$DETAIL,c(1,NA,1), info = label)
    testthat::expect_equal(corrected$corrected_data$CHOICE,z$observed, info = label)
    testthat::expect_true(all(corrected$record_summary$resolved), info = label)
    # An editable mapped gate must be filled with an original code, never a position.
    corrected_gate <- SSPICE::correct_data(absent,dag,record_id = "ID",
      donor_data = data[1:3,,drop = FALSE],immutable_variables = c("DETAIL","END"),
      items_mapping_file = file,control = SSPICE::correction_control(
        seed = 123L,max_iterations = 30L,use_parallel = FALSE))
    testthat::expect_true(corrected_gate$corrected_data$CHOICE %in% z$observed[c(1,3)], info = label)
    testthat::expect_true(all(corrected_gate$record_summary$resolved), info = label)
    testthat::expect_identical(serialize(list(data,dag),NULL),saved, info = label)
    testthat::expect_identical(readLines(file, warn = FALSE), mapping_file_before, info = label)
    unlink(file)
  }
})

testthat::test_that("mapping rejects ambiguous metadata and unknown observed codes", {
  read_map <- getFromNamespace(".sspice_read_item_mapping","SSPICE")
  map_value <- getFromNamespace(".sspice_map_route_values","SSPICE")
  validate <- getFromNamespace(".sspice_validate_item_mapping","SSPICE")
  file <- tempfile(fileext = ".csv"); on.exit(unlink(file))
  m <- data.frame(varname = "Q", item_route = 1:3, item_dataset = c(2,3,4))
  utils::write.csv(m,file,row.names = FALSE)
  mapping <- read_map(file)
  testthat::expect_equal(map_value(c(2,3,4,NA),"Q",mapping),c(1,2,3,NA))
  testthat::expect_error(map_value(9,"Q",mapping),"absent from item mapping")
  testthat::expect_error(validate(mapping,"Q",3L,10,30),"disagree")
  m$item_dataset <- c(2,2,4); utils::write.csv(m,file,row.names = FALSE)
  testthat::expect_error(read_map(file),"one-to-one")
})

testthat::test_that("VAUX and LEDIT continue to use original data codes", {
  internal <- function(name) getFromNamespace(name,"SSPICE")
  meta <- function(name,codes,values) data.frame(V1 = name,V2 = as.character(values),
    V3 = "CHOICE",V4 = "",V5 = "",V6 = as.character(codes),V7 = "",V8 = "",V9 = "NULL")
  for (codes in list(c(10,21,99),1:3)) {
    data <- data.frame(ID = 1:4,CHOICE = c(codes,NA))
    vaux <- internal("create_vaux_variables")(data,meta("VAUX_TEST",codes,c(2,1,2)),
      tempdir(),c("ID","CHOICE","VAUX_TEST"))$VAUX_TEST
    testthat::expect_identical(as.numeric(vaux),c(2,1,2,NA_real_))
    ledit <- internal("create_logical_edit_variables")(data,
      meta("LEDIT_TEST",codes[1:2],c(2,1)))$LEDIT_TEST
    testthat::expect_identical(as.numeric(ledit),c(2,1,NA_real_,NA_real_))
  }
})
