library(testthat)

# Tests for R/fia_annualized.R (Phase 5 annualized estimation). Built on the
# demo fixture: current = 2021 evaluation (532101), previous = 2019 (531901),
# the same 30 plots remeasured. The fixture is a full remeasurement (not a true
# rotating 1/10 panel), so these test the building blocks and exact invariants
# rather than the small-sample behavior the real-data vignette demonstrates.

demo_db <- function() {
  p <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
  if (!nzchar(p) || !file.exists(p)) p <- file.path("..", "..", "inst", "extdata", "FIADB_demo.db")
  p
}

skip_if_no_demo <- function() {
  p <- demo_db()
  if (!nzchar(p) || !file.exists(p)) skip("FIADB_demo.db fixture not available")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("sqldf")
  p
}

# compiled current + previous plot frames, paired with previous-measurement
# predictor columns and explicit year columns
demo_pairs <- function(p) {
  db <- fia_db(p)
  mk <- function(ev) suppressWarnings(fia_compile_plots(
    fia_compile_trees(fia_trees(db, evalid = ev), vol_source = "fiadb"),
    fia_plots(db, evalid = ev)))
  cur  <- mk(532101)
  prev <- mk(531901)
  pr <- prev[, c("PLT_CN", "vol_cf_net", "ba_ft", "stems")]
  names(pr) <- c("PREV_PLT_CN", "vol_t1", "ba_t1", "stems_t1")
  pairs <- merge(cur, pr, by = "PREV_PLT_CN")
  pairs$prev_yr <- 2019L
  pairs$curr_yr <- pairs$MEASYEAR
  pairs$interval <- pairs$curr_yr - pairs$prev_yr
  list(cur = cur, prev = prev, pairs = pairs)
}

# ---- fia_annual_panel ------------------------------------------------------

test_that("fia_annual_panel keeps only plots measured in the target year", {
  p <- skip_if_no_demo()
  d <- demo_pairs(p)
  combined <- plyr::rbind.fill(d$cur, d$prev)   # MEASYEAR 2021 + 2019

  panel <- fia_annual_panel(combined, 2021)
  expect_true(all(panel$MEASYEAR == 2021))
  expect_equal(nrow(panel), sum(combined$MEASYEAR == 2021))
  expect_warning(fia_annual_panel(combined, 1850), "no plots")
})

# ---- fia_growth_model ------------------------------------------------------

test_that("fia_growth_model fits level and increment models", {
  p <- skip_if_no_demo()
  pairs <- demo_pairs(p)$pairs

  m <- fia_growth_model(pairs, response = "vol_cf_net",
                        predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"))
  expect_s3_class(m, "fia_growth_model")
  expect_equal(m$engine, "ols")
  expect_false(m$increment)
  expect_output(print(m), "fia_growth_model")

  mi <- fia_growth_model(pairs, response = "vol_cf_net",
                         predictors = c("vol_t1", "ba_t1", "interval"),
                         increment = TRUE, prev_value_col = "vol_t1")
  expect_true(mi$increment)
  expect_error(fia_growth_model(pairs, "vol_cf_net",
                                c("vol_t1"), increment = TRUE), "prev_value_col")
})

# ---- fia_annualize_resid: the exact annualization arithmetic ---------------

test_that("fia_annualize_resid scales residuals exactly e_T -> e_annual -> e_k", {
  p <- skip_if_no_demo()
  pairs <- demo_pairs(p)$pairs
  m <- fia_growth_model(pairs, response = "vol_cf_net",
                        predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"))

  out <- fia_annualize_resid(m, pairs, response = "vol_cf_net",
                             prev_year_col = "prev_yr", curr_year_col = "curr_yr",
                             target_year = 2021)
  expect_true(all(c(".pred", ".resid_periodic", ".resid_annual",
                    ".resid_target") %in% names(out)))

  Tint <- out$curr_yr - out$prev_yr
  expect_equal(out$.resid_periodic, out$vol_cf_net - out$.pred)
  expect_equal(out$.resid_annual, out$.resid_periodic / Tint)
  expect_equal(out$.resid_target, (2021 - out$prev_yr) * out$.resid_annual)
})

# ---- fia_growth_cv ---------------------------------------------------------

test_that("fia_growth_cv returns finite bias / rmse / r2", {
  p <- skip_if_no_demo()
  pairs <- demo_pairs(p)$pairs

  cv <- fia_growth_cv(pairs, response = "vol_cf_net",
                      predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"),
                      k = 5, seed = 1)
  expect_equal(nrow(cv), 1L)
  expect_true(all(is.finite(c(cv$bias, cv$rmse, cv$r2))))
  expect_true(cv$rmse >= 0)
})

# ---- fia_estimate_model (approach 3) ---------------------------------------

test_that("fia_estimate_model estimates the prediction column", {
  p <- skip_if_no_demo()
  pairs <- demo_pairs(p)$pairs
  m <- fia_growth_model(pairs, response = "vol_cf_net",
                        predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"))
  pairs <- fia_annualize_resid(m, pairs, response = "vol_cf_net",
                               prev_year_col = "prev_yr", curr_year_col = "curr_yr",
                               target_year = 2021)

  mod <- fia_estimate_model(pairs, ".pred", by = "COUNTYCD", type = "total")
  ref <- fia_estimate(pairs, vars = ".pred", by = "COUNTYCD", type = "total")
  expect_equal(mod$estimate, ref$estimate, tolerance = 1e-8)
  expect_true(all(grepl("^model_", mod$type)))
})

# ---- fia_estimate_greg (approach 2): the collapse identity -----------------

test_that("GREG collapses to the design estimate when sample == population", {
  p <- skip_if_no_demo()
  pairs <- demo_pairs(p)$pairs
  m <- fia_growth_model(pairs, response = "vol_cf_net",
                        predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"))
  pairs <- fia_annualize_resid(m, pairs, response = "vol_cf_net",
                               prev_year_col = "prev_yr", curr_year_col = "curr_yr",
                               target_year = 2021)
  # full (non-annualized) residual at t2: obs - pred
  pairs$.resid_obs <- pairs$vol_cf_net - pairs$.pred

  greg   <- fia_estimate_greg(pairs, pred_col = ".pred", df_sample = pairs,
                              resid_col = ".resid_obs", by = "COUNTYCD", type = "total")
  design <- fia_estimate(pairs, vars = "vol_cf_net", by = "COUNTYCD", type = "total")

  m2 <- merge(greg[, c("COUNTYCD", "estimate")],
              design[, c("COUNTYCD", "estimate")], by = "COUNTYCD",
              suffixes = c("_greg", "_design"))
  expect_equal(m2$estimate_greg, m2$estimate_design, tolerance = 1e-4)

  # GREG variance is the design variance of the residual term
  rvar <- fia_estimate(pairs, vars = ".resid_obs", by = "COUNTYCD", type = "total")
  mv <- merge(greg[, c("COUNTYCD", "variance")],
              rvar[, c("COUNTYCD", "variance")], by = "COUNTYCD",
              suffixes = c("_greg", "_resid"))
  expect_equal(mv$variance_greg, mv$variance_resid, tolerance = 1e-6)
  expect_true(all(grepl("^greg_", greg$type)))
})
