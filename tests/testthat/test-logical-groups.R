make_or_test_routing <- function() {
  data.frame(
    V1 = c("UNIT_KEY", "A", "B", "NEXT"),
    V2 = c(1, 2, 2, 1),
    V3 = c(1, 1, 1, 1),
    V4 = c(999, 2, 2, 1),
    V5 = c(0, 1, 1, 0),
    V6 = c(0, 0, 0, 0),
    V7 = NA,
    V8 = NA,
    stringsAsFactors = FALSE
  )
}

make_xor_test_routing <- function() {
  data.frame(
    V1 = c("UNIT_KEY", "CATEGORY", "AMOUNT", "NEXT"),
    V2 = c(1, 1, 1, 1),
    V3 = c(1, 1, 0, 1),
    V4 = c(999, 1, 100, 1),
    V5 = c(0, -1, -1, 0),
    V6 = c(0, 0, 0, 0),
    V7 = NA,
    V8 = NA,
    stringsAsFactors = FALSE
  )
}

test_that("coarse_groups_OR replaces each OR group with one VOR node", {
  routing <- make_or_test_routing()

  grouped <- SSPICE:::coarse_groups_OR(routing)

  expect_equal(nrow(grouped), 3L)
  expect_identical(
    as.character(grouped$V1),
    c("UNIT_KEY", "VOR1", "NEXT")
  )
  expect_equal(as.numeric(grouped$V2), c(1, 1, 1))
  expect_equal(as.numeric(grouped$V3), c(1, 1, 1))
  expect_equal(as.numeric(grouped$V4), c(999, 1, 1))
  expect_equal(as.numeric(grouped$V5), c(0, 0, 0))
  expect_equal(as.numeric(grouped$V6), c(0, 0, 0))
  expect_false("cn" %in% names(grouped))
})

test_that("generate_groups_OR reproduces OR activation in record data", {
  routing <- make_or_test_routing()
  grouped_routing <- SSPICE:::coarse_groups_OR(routing)

  test_data <- data.frame(
    UNIT_KEY = 1:6,
    A = c(1, NA, 0, NA, 2, 0),
    B = c(NA, 2, 0, NA, 1, NA),
    NEXT = c(1, 1, 1, 1, 1, 1)
  )

  result <- SSPICE:::generate_groups_OR(
    data = test_data,
    routing_file_ungrouped = routing,
    routing_file_grouped = grouped_routing$V1
  )

  expect_identical(
    names(result),
    c("UNIT_KEY", "VOR1", "NEXT")
  )
  expect_equal(result$VOR1, c(1, 1, NA, NA, 1, NA))
  expect_identical(result$UNIT_KEY, test_data$UNIT_KEY)
  expect_equal(nrow(result), nrow(test_data))
  expect_false(any(c("A", "B") %in% names(result)))
})

test_that("coarse_groups_XOR creates a two-modal VXOR node", {
  routing <- make_xor_test_routing()

  grouped <- SSPICE:::coarse_groups_XOR(routing)

  expect_equal(nrow(grouped), 3L)
  expect_identical(
    as.character(grouped$V1),
    c("UNIT_KEY", "VXOR1", "NEXT")
  )
  expect_equal(as.numeric(grouped$V2), c(1, 2, 1))
  expect_equal(as.numeric(grouped$V3), c(1, 1, 1))
  expect_equal(as.numeric(grouped$V4), c(999, 2, 1))
  expect_equal(as.numeric(grouped$V5), c(0, 0, 0))
  expect_equal(as.numeric(grouped$V6), c(0, 0, 0))
})

test_that("coarse_groups_XOR redirects incoming jumps to the VXOR node", {
  routing <- make_xor_test_routing()
  routing$V6[1L] <- 98
  routing$V8[1L] <- "CATEGORY"

  grouped <- SSPICE:::coarse_groups_XOR(routing)

  expect_equal(as.character(grouped$V8[1L]), "VXOR1")
  expect_false(any(grouped$V1 %in% c("CATEGORY", "AMOUNT")))
})

test_that("generate_groups_XOR encodes valid and invalid XOR states", {
  routing <- make_xor_test_routing()
  grouped_routing <- SSPICE:::coarse_groups_XOR(routing)

  test_data <- data.frame(
    UNIT_KEY = 1:4,
    CATEGORY = c(1, NA, 1, NA),
    AMOUNT = c(NA, 25, 30, NA),
    NEXT = c(1, 1, 1, 1)
  )

  result <- SSPICE:::generate_groups_XOR(
    data = test_data,
    routing_file_ungrouped = routing,
    routing_file_grouped = grouped_routing,
    strict = FALSE
  )

  expect_identical(
    names(result),
    c("UNIT_KEY", "VXOR1", "NEXT")
  )
  expect_equal(
    as.integer(result$VXOR1),
    c(1L, 2L, NA, NA)
  )
  expect_equal(
    attr(result$VXOR1, "neither_active"),
    4L
  )
  expect_equal(nrow(result), 4L)
  expect_identical(result$UNIT_KEY, test_data$UNIT_KEY)

  violations <- attr(result, "xor_violations")

  expect_type(violations, "list")
  expect_true("VXOR1" %in% names(violations))
  expect_equal(violations$VXOR1$row, 3L)
  expect_equal(
    violations$VXOR1$violation,
    "both_variables_active"
  )
})

test_that("generate_groups_XOR rejects simultaneous activation in strict mode", {
  routing <- make_xor_test_routing()
  grouped_routing <- SSPICE:::coarse_groups_XOR(routing)

  test_data <- data.frame(
    UNIT_KEY = 1,
    CATEGORY = 1,
    AMOUNT = 25,
    NEXT = 1
  )

  expect_error(
    SSPICE:::generate_groups_XOR(
      data = test_data,
      routing_file_ungrouped = routing,
      routing_file_grouped = grouped_routing,
      strict = TRUE
    ),
    "Violazione del gruppo XOR"
  )
})

test_that("invalid XOR group definitions are rejected", {
  routing <- make_xor_test_routing()
  routing$V5[4L] <- -1

  expect_error(
    SSPICE:::coarse_groups_XOR(routing),
    "contiene 3 variabili anziche 2"
  )

  expect_error(
    SSPICE:::generate_groups_XOR(
      data = data.frame(
        UNIT_KEY = 1,
        CATEGORY = 1,
        AMOUNT = NA_real_,
        NEXT = NA_real_
      ),
      routing_file_ungrouped = routing,
      routing_file_grouped = c("UNIT_KEY", "VXOR1")
    ),
    "contiene 3 variabili"
  )
})
