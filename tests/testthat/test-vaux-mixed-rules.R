testthat::test_that("mixed comparison and grouped logic yield expected modality", {
  d <- data.frame(
    A = c(9.5, 10, 3, 2, NA_real_),
    D = c(1, 1, 2, 2, 1),
    E = c(0, 3, 3, 0, 3)
  )
  rules <- data.frame(
    V1 = rep("VAUX_MIX", 3), V2 = rep(1, 3),
    V3 = c("A", "D", "E"),
    V4 = "", V5 = "", V6 = c("", "1", "3"),
    V7 = "", V8 = "",
    V9 = c("<10 ) AND (", "OR", "NULL")
  )
  actual <- SSPICE:::create_vaux_variables(
    d, rules, tempdir(), c(names(d), "VAUX_MIX")
  )
  expected <- d$A < 10 & (d$D == 1 | d$E == 3)
  testthat::expect_equal(actual$VAUX_MIX,
                         ifelse(!is.na(expected) & expected, 1, NA_real_))
})

testthat::test_that("arithmetic parentheses, multiplication, division and logic combine", {
  d <- data.frame(
    A = c(2, 4, 1, 5), B = c(3, 1, 2, 2),
    C = c(5, 3, 4, 4), D = c(3, 1, 2, 3),
    E = c(0, 1, 0, 0), F = c(2, 2, 2, 2)
  )
  rules <- data.frame(
    V1 = rep("VAUX_MIX", 7), V2 = rep(1, 7),
    V3 = c("A", "B", "C", "D", "F", "E", "A"),
    V4 = "", V5 = "", V6 = c("", "", "", "", "", "1", "2"),
    V7 = "", V8 = "",
    V9 = c("+", ") * (", "-", ") / (", ">=5 ) OR (", "AND", "NULL")
  )
  actual <- SSPICE:::create_vaux_variables(
    d, rules, tempdir(), c(names(d), "VAUX_MIX")
  )
  expected <- ((d$A + d$B) * (d$C - d$D) / d$F >= 5) |
    (d$E == 1 & d$A == 2)
  testthat::expect_equal(actual$VAUX_MIX,
                         ifelse(expected, 1, NA_real_))
})

testthat::test_that("mixed expressions reject unmatched parentheses", {
  d <- data.frame(A = 1, D = 1)
  rules <- data.frame(
    V1 = rep("VAUX_MIX", 2), V2 = 1,
    V3 = c("A", "D"), V4 = "", V5 = "",
    V6 = c("", "1"), V7 = "", V8 = "",
    V9 = c("<10 ) AND (", "NULL")
  )
  testthat::expect_equal(
    SSPICE:::create_vaux_variables(d, rules, tempdir(),
                                   c("A", "D", "VAUX_MIX"))$VAUX_MIX,
    1
  )
  rules$V9[2] <- "AND"
  testthat::expect_error(
    SSPICE:::create_vaux_variables(d, rules, tempdir(),
                                   c("A", "D", "VAUX_MIX")),
    "Unfinished mixed VAUX rule"
  )
})
