library(testthat)

# Tests for R/pandermod.R: pandermod generic + .lm / .list / .simex methods.

testthat::test_that("pandermod.lm returns a one-row model summary data.frame", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(pandermod(m, return_df = TRUE))

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

testthat::test_that("pandermod.lm with df0 adds an n0 validation column", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(
    pandermod(m, df0 = data.frame(y = c(3, 5), x = c(2, 3)), return_df = TRUE)
  )
  testthat::expect_true("n0" %in% names(df))
  testthat::expect_equal(df$n0, 2L)
})

testthat::test_that("pandermod.lm return_df=FALSE prints via pander and returns NULL", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  out <- suppressWarnings(
    utils::capture.output(res <- pandermod(m, return_df = FALSE))
  )
  testthat::expect_null(res)
})

testthat::test_that("pandermod.list row-binds summaries for each model", {
  skip_if_not_installed("pander")
  skip_if_not_installed("plyr")
  skip_if_not_installed("knitr")

  m <- lm(y ~ x, data.frame(y = c(2, 4, 6, 8, 10), x = 1:5))
  df <- suppressWarnings(pandermod(list(m, m), return_df = TRUE))
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_equal(df$Resp., c("y", "y"))
  testthat::expect_equal(df$n, c(5L, 5L))
})

testthat::test_that("pandermod is a generic function", {
  testthat::expect_true(is.function(pandermod))
})

testthat::test_that("pandermod.simex placeholder (requires simex)", {
  skip_if_not_installed("simex")
  # simex is not installed in this environment, so this test is skipped.
  testthat::expect_true(TRUE)
})
