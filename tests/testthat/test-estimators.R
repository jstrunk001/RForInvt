library(testthat)

# Tests for R/estimators.r -- all objects are package internals, reached via
# RForInvt:::name. .estimate() always emits a "Not Yet Implemented" warning, so
# calls to it are wrapped in suppressWarnings().

testthat::test_that(".pop_test generates a population and a sample of requested sizes", {
  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "random")

  testthat::expect_equal(nrow(p$pop), 2000)
  testthat::expect_equal(nrow(p$s), 200)
  # random-type population carries response, auxiliary, weight and inclusion prob
  testthat::expect_true(all(c("y", "x", "wt", "pi") %in% names(p$pop)))
  testthat::expect_true(all(c("y", "x", "wt", "pi") %in% names(p$s)))
})

testthat::test_that(".summary computes deff and rsq from the two estimate lists", {
  # minimal lists carrying exactly the fields .summary() reads
  mk <- function(se_m, se_t) {
    list(type = "rand", resp_nm = "y", var_type = "asym", wt_nm = "wt",
         ef_nm = NA, su_nm = NA, formula = NA, mean = 10, total = 1000,
         se_m = se_m, se_t = se_t, rmse = 3, n = 50, n_str = NA,
         n_clus = NA, N = 100, deff = NA)
  }
  res  <- mk(se_m = 2, se_t = 200)
  res0 <- mk(se_m = 4, se_t = 400)

  s <- RForInvt:::.summary(res, res0)

  testthat::expect_equal(s$deff, res$se_m^2 / res0$se_m^2)
  testthat::expect_equal(s$rsq, round(100 * (1 - res$se_m^2 / res0$se_m^2), 1))
})

testthat::test_that(".rand estimates the mean for asym and svypkg variance types", {
  testthat::skip_if_not_installed("survey")

  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "random")
  N <- nrow(p$pop)
  s <- p$s

  r_asym <- RForInvt:::.rand(x = s, resp_nm = "y", wt_nm = "wt", ef_nm = NA,
                             pop = p$pop, N = N, type = "rand", var_type = "asym")
  testthat::expect_equal(r_asym$mean, mean(p$pop$y), tolerance = 0.1)
  testthat::expect_gt(r_asym$se_t, 0)

  r_svy <- RForInvt:::.rand(x = s, resp_nm = "y", wt_nm = "wt", ef_nm = NA,
                            pop = p$pop, N = N, type = "rand", var_type = "svypkg")
  testthat::expect_false(is.na(r_svy$deff))
})

testthat::test_that(".estimate ht/asym returns a one-row estimate near the population mean", {
  testthat::skip_if_not_installed("survey")

  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "random")
  N <- nrow(p$pop)

  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", wt_nm = "wt", N = N,
                         type = "ht", var_type = "asym")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 0.1)
})

testthat::test_that(".estimate regression/asym returns a one-row estimate near the population mean", {
  testthat::skip_if_not_installed("survey")

  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "regression")
  N <- nrow(p$pop)

  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", reg_form = as.formula("y ~ x"),
                         pop = data.frame(x = sum(p$pop$x)), wt_nm = "wt", N = N,
                         type = "regression", var_type = "asym")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 0.1)
})

testthat::test_that(".estimate regression/bs returns a one-row estimate near the population mean", {
  testthat::skip_if_not_installed("survey")
  testthat::skip_if_not_installed("bootstrap")

  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "regression")
  N <- nrow(p$pop)

  set.seed(2)
  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", reg_form = as.formula("y ~ x"),
                         pop = data.frame(x = sum(p$pop$x)), wt_nm = "wt", N = N,
                         type = "regression", var_type = "bs")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 0.1)
})

testthat::test_that(".estimate stratified/svypkg returns a one-row estimate near the population mean", {
  testthat::skip_if_not_installed("survey")
  testthat::skip_if_not_installed("plyr")

  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "stratified", nstrat = 5)
  N <- nrow(p$pop)
  pop_Ni <- aggregate(x ~ str, data = p$pop, FUN = length)

  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", wt_nm = "wt", strata_nm = "str",
                         pop = pop_Ni, N = N, type = "stratified", var_type = "svypkg")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 1)
})

testthat::test_that(".estimate regression/svypkg produces a valid estimate", {
  testthat::skip_if_not_installed("survey")

  # FAILS at baseline: .reg() svypkg branch tests class(cb) == "try-error"
  # (L244) on an object whose class() has length > 1, so the if() condition is
  # length > 1 and errors with "the condition has length > 1".
  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "regression")
  N <- nrow(p$pop)

  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", reg_form = as.formula("y ~ x"),
                         pop = data.frame(x = sum(p$pop$x)), wt_nm = "wt", N = N,
                         type = "regression", var_type = "svypkg")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 0.1)
})

testthat::test_that(".estimate stratified/asym produces a valid estimate", {
  testthat::skip_if_not_installed("survey")
  testthat::skip_if_not_installed("plyr")

  # FAILS at baseline: .str() asym branch calls mapply(estimate, ...) (L336)
  # but the per-stratum estimator function is the internal `.estimate`; there
  # is no object named `estimate`, so it errors with
  # "object 'estimate' not found".
  set.seed(1)
  p <- RForInvt:::.pop_test(N = 2000, n = 200, wt = "equal", type = "stratified", nstrat = 5)
  N <- nrow(p$pop)
  pop_Ni <- aggregate(x ~ str, data = p$pop, FUN = length)

  e <- suppressWarnings(
    RForInvt:::.estimate(x = p$s, resp_nm = "y", wt_nm = "wt", strata_nm = "str",
                         pop = pop_Ni, N = N, type = "stratified", var_type = "asym")
  )
  testthat::expect_true(is.data.frame(e))
  testthat::expect_equal(nrow(e), 1)
  testthat::expect_equal(e$mean, mean(p$pop$y), tolerance = 1)
})

testthat::test_that("not-yet-implemented estimator stubs error informatively", {
  testthat::expect_error(RForInvt:::.cb(), "calibration Not Yet Implemented")
  testthat::expect_error(RForInvt:::.ts(), "two-stage Not Yet Implemented")
  testthat::expect_error(RForInvt:::.ms(), "Multi-stage Not Yet Implemented")
})
