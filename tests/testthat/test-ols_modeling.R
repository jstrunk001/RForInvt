library(testthat)

# Tests for R/ols_modeling.R
# make_model_df() is provided by helper-rforinvt.R:
#   y = 2 + 3*x1 - 1.5*x2 + small noise ; x3 is noise.

testthat::test_that("reg_model returns an lm with a summary element", {
  testthat::skip_if_not_installed("leaps")

  df <- make_model_df()
  rg <- leaps::regsubsets(y ~ x1 + x2 + x3, df, nvmax = 3)
  m  <- reg_model(df[, c("y", "x1", "x2", "x3")], rg, rank_by = "bic")

  testthat::expect_true(inherits(m, "lm"))
  testthat::expect_true("summary" %in% names(m))
})

testthat::test_that("lm_boot returns a list with a positive .632 error", {
  testthat::skip_if_not_installed("bootstrap")

  m <- lm(y ~ x1 + x2, make_model_df())
  set.seed(1)
  b <- lm_boot(m, n_boot = 20)

  testthat::expect_true(is.list(b))
  testthat::expect_true("err_632" %in% names(b))
  testthat::expect_gt(b$err_632, 0)
})

testthat::test_that("many_boots summarises bootstrap error over a range of sample sizes", {
  testthat::skip_if_not_installed("bootstrap")

  # FAILS at baseline: internal lm_boot_in() (L286) references `modk`, but the
  # fitted model is named `mod_k`, so evaluation errors with
  # "object 'modk' not found". Intended behavior asserted below.
  m <- lm(y ~ x1 + x2, make_model_df())
  res <- many_boots(m, n_range = c(20, 30), r_boots = 2, n_clus = 1, lm_boot = lm_boot)

  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(nrow(res), 2)
  testthat::expect_true("rsq_err_632" %in% names(res))
})

testthat::test_that("reg_multi returns one named regsubsets object per response", {
  testthat::skip_if_not_installed("leaps")

  rm_obj <- reg_multi(y_vars = "y", data = make_model_df(), form_y = "y~.")

  testthat::expect_true(is.list(rm_obj))
  testthat::expect_equal(length(rm_obj), 1)
  testthat::expect_true(inherits(rm_obj$y, "regsubsets"))
  testthat::expect_equal(rm_obj$y$response, "y")
})

testthat::test_that("lm_multi returns one named lm object per response", {
  res <- lm_multi("y", make_model_df(), "y~.", verbose = FALSE)

  testthat::expect_true(is.list(res))
  testthat::expect_true(inherits(res$y, "lm"))
  testthat::expect_equal(res$y$response, "y")
})

testthat::test_that("mod_multi turns a reg_multi object into a named list of lm models", {
  testthat::skip_if_not_installed("leaps")

  # FAILS at baseline: mod_multi() references `reg_multi_obj` (and helper
  # `reg_model_in`) which are never defined in the function body; the formal
  # argument is `mods_list`. Errors with "object 'reg_multi_obj' not found".
  df <- make_model_df()
  mods <- mod_multi(reg_multi("y", df, "y~."), data = df)

  testthat::expect_true(is.list(mods))
  testthat::expect_true(inherits(mods$y, "lm"))
})

testthat::test_that("pred_multi predicts each model into a <resp>_pd column", {
  mods <- lm_multi("y", make_model_df(), "y~.", verbose = FALSE)

  nd <- make_model_df(20, seed = 2)
  nd$id <- seq_len(nrow(nd))

  p <- pred_multi(mods, nd, dat0 = NA, id_col = "id", n_clus = 1)

  testthat::expect_true(is.data.frame(p))
  testthat::expect_true(all(c("id", "y_pd") %in% names(p)))
  testthat::expect_equal(nrow(p), nrow(nd))
})

testthat::test_that("multi_bs bootstraps a list of lm models into a data.frame", {
  testthat::skip_if_not_installed("bootstrap")

  set.seed(1)
  mbs <- multi_bs(list(y = lm(y ~ x1 + x2, make_model_df())),
                  n_boot = 20, n_clus = 1, lm_boot = lm_boot)

  testthat::expect_true(is.data.frame(mbs))
  testthat::expect_equal(nrow(mbs), 1)
  testthat::expect_equal(mbs$variable, "y")
  testthat::expect_gt(mbs$err_632, 0)
})

testthat::test_that("lm_summary returns validation summary for single and multiple models", {
  df <- make_model_df()

  s <- lm_summary(lm(y ~ x1 + x2, df), df)
  testthat::expect_true(is.data.frame(s))
  testthat::expect_equal(s$notes, "successful validation")

  s2 <- lm_summary(list(a = lm(y ~ x1, df), b = lm(y ~ x2, df)), df)
  testthat::expect_true(is.data.frame(s2))
  testthat::expect_equal(nrow(s2), 2)
})
