testthat::test_that("original SA defaults are preserved", {
  control <- correction_control()

  testthat::expect_identical(control$final_temperature, 1e-4)
  testthat::expect_identical(control$cooling_fraction, 0.90)
  testthat::expect_identical(control$top_k, 10L)
  testthat::expect_identical(control$beta_fill, 1)
  testthat::expect_identical(control$lambda_change, 2)
  testthat::expect_identical(control$minimum_donors, 1L)
  testthat::expect_true(control$allow_unconditional_donor)
})

testthat::test_that("linear cooling reaches Tf at ninety percent of L", {
  temperature <- getFromNamespace(".sspice_sa_temperature", "SSPICE")
  L <- 100L
  T0 <- 7
  Tf <- 1e-4

  testthat::expect_equal(temperature(1L, L, T0, Tf, 0.90), T0)
  testthat::expect_equal(temperature(90L, L, T0, Tf, 0.90), Tf)
  testthat::expect_equal(temperature(100L, L, T0, Tf, 0.90), Tf)
})

testthat::test_that("conditioning hierarchy backs off to marginal donors", {
  hierarchy <- getFromNamespace(".sspice_conditioning_hierarchy", "SSPICE")
  result <- hierarchy("GATE", "STRATUM")

  testthat::expect_identical(result[[1L]], c("GATE", "STRATUM"))
  testthat::expect_identical(result[[2L]], "GATE")
  testthat::expect_identical(result[[3L]], character())
})

testthat::test_that("lucid empirical tilt favours violation improvement", {
  set.seed(91L)
  base_probability <- c(0.5, 0.5)
  improvement <- c(0, 2)
  beta <- 1
  log_weight <- log(base_probability) + beta * improvement
  probability <- exp(log_weight - max(log_weight))
  probability <- probability / sum(probability)

  testthat::expect_gt(probability[2L], probability[1L])
  testthat::expect_equal(sum(probability), 1)
})
