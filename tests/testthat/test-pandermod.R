library(testthat)

# Tests for R/pander_mod.R: pander_mod generic + .lm / .list / .simex methods.

testthat::test_that("pander_mod.lm returns a one-row model summary data.frame", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(pander_mod(m, return_df = TRUE))

  testthat::expect_true(is.data.frame(df))
  testthat::expect_equal(
    names(df),
    c("Resp.", "n", "Resid. Std. Error", "$R^2$",
      "Adjusted $R^2$", "$DE^{-1}$", "Predictors")
  )
  testthat::expect_equal(df$n, 5L)
  testthat::expect_equal(df[["$R^2$"]], 1)
  testthat::expect_equal(df$Predictors, "x")
  testthat::expect_equal(df$Resp., "y")
})

testthat::test_that("pander_mod.lm with df0 adds an n0 validation column", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(
    pander_mod(m, df0 = data.frame(y = c(3, 5), x = c(2, 3)), return_df = TRUE)
  )
  testthat::expect_true("n0" %in% names(df))
  testthat::expect_equal(df$n0, 2L)
})

testthat::test_that("pander_mod.lm return_df=FALSE prints via pander and returns NULL", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  out <- suppressWarnings(
    utils::capture.output(res <- pander_mod(m, return_df = FALSE))
  )
  testthat::expect_null(res)
})

testthat::test_that("pander_mod.list row-binds summaries for each model", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(pander_mod(list(m, m), return_df = TRUE))
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_equal(df$Resp., c("y", "y"))
  testthat::expect_equal(df$n, c(5L, 5L))
})

testthat::test_that("pander_mod is a generic function", {
  testthat::expect_true(is.function(pander_mod))
})

testthat::test_that("pander_mod.simex summarizes a simex-corrected model", {
  skip_if_not_installed("simex")

  set.seed(1)
  n <- 200
  x_true <- rnorm(n)
  x_obs <- x_true + rnorm(n, sd = 0.3)
  y <- 2 + 3 * x_true + rnorm(n, sd = 0.5)
  d <- data.frame(y = y, x = x_obs)
  m <- lm(y ~ x, data = d, x = TRUE)
  sm <- simex::simex(m, SIMEXvariable = "x", measurement.error = 0.3, B = 20)

  df <- suppressWarnings(pander_mod(sm, df0 = d, return_df = TRUE))

  testthat::expect_true(is.data.frame(df))
  testthat::expect_true("n0" %in% names(df))
  testthat::expect_equal(df$n0, 200)
})

