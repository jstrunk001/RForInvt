library(testthat)

# Regression tests for R/ols_modeling.R: models fitted by lm_multi() must be
# re-fittable via update() so that lm_summary() (leave-one-out) and lm_boot()
# (.632+ bootstrap) work on them. Previously lm_multi stored the formula as a
# local symbol `form_i` in the model call, so update() failed in a fresh frame
# with "object 'form_i' not found". The fix embeds the formula object in the call.

testthat::test_that("lm_summary works on models fitted by lm_multi (update-able call)", {
  df <- make_model_df()

  mods <- lm_multi(y_vars = "y", data = df, form_y = "y~.", verbose = FALSE)

  # would previously error: object 'form_i' not found (via update() in fn_e)
  s <- lm_summary(mods, data = df)

  testthat::expect_true(is.data.frame(s))
  testthat::expect_equal(s$notes, "successful validation")
  testthat::expect_true(is.finite(s$RMSE_cv))
})

testthat::test_that("lm_boot works on a model fitted by lm_multi", {
  testthat::skip_if_not_installed("bootstrap")

  df <- make_model_df()
  mods <- lm_multi(y_vars = "y", data = df, form_y = "y~.", verbose = FALSE)

  set.seed(1)
  b <- lm_boot(mods[["y"]], n_boot = 20)

  testthat::expect_true(is.list(b))
  testthat::expect_gt(b$err_632, 0)
})
