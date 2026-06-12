library(testthat)

# Tests for R/fia_estimate.R (Phase 3 FIA estimation layer), against the bundled
# western-WA fixture. The synthetic numbers do NOT match EVALIDator (real-data
# validation is deferred), so these check internal consistency of the
# Bechtold & Patterson (2005) post-stratified estimators rather than absolute
# values: the design-based total must equal the direct EXPNS-weighted total,
# per-acre must equal total/area, components must sum to the whole, and the
# variance/structure must be well formed.

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

# compiled per-plot frame for the most recent volume evaluation
compiled_plots <- function(p, fns_compute = NULL, vars_group = NULL) {
  db <- fia_db(p)
  ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
  tr <- fia_trees(db, evalid = ev)
  if (is.null(fns_compute)) {
    trc <- fia_compile_trees(tr, vol_source = "fiadb")
  } else {
    trc <- fia_compile_trees(tr, vol_source = "fiadb",
                             fns_compute = fns_compute, vars_group = vars_group)
  }
  suppressWarnings(fia_compile_plots(trc, fia_plots(db, evalid = ev)))
}

# ---- fia_estimate: totals --------------------------------------------------

test_that("post-stratified total equals the direct EXPNS-weighted total", {
  p <- skip_if_no_demo()
  plc <- compiled_plots(p)

  est <- fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "total")

  # direct B&P identity: tau = sum_plots EXPNS * (per-acre value), by domain
  direct <- aggregate(v ~ COUNTYCD,
                      data = data.frame(COUNTYCD = plc$COUNTYCD,
                                        v = plc$EXPNS * plc$vol_cf_net), FUN = sum)
  m <- merge(est[, c("COUNTYCD", "estimate")], direct, by = "COUNTYCD")
  expect_equal(m$estimate, m$v, tolerance = 1e-6)

  # well-formed output
  expect_true(all(c("estimate", "variance", "se", "se_pct",
                    "n_plots", "n_nonzero") %in% names(est)))
  expect_equal(est$se, sqrt(est$variance), tolerance = 1e-8)
  expect_true(all(est$variance >= 0))
})

# ---- fia_estimate: per-acre is a ratio of total to area --------------------

test_that("per-acre estimate equals total / forest-area", {
  p <- skip_if_no_demo()
  plc <- compiled_plots(p)

  tot  <- fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "total")
  area <- fia_estimate(plc, by = "COUNTYCD", type = "area")
  pac  <- fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "per_acre")

  m <- merge(tot[, c("COUNTYCD", "estimate")],
             area[, c("COUNTYCD", "estimate")], by = "COUNTYCD",
             suffixes = c("_tot", "_area"))
  m <- merge(m, pac[, c("COUNTYCD", "estimate")], by = "COUNTYCD")
  expect_equal(m$estimate, m$estimate_tot / m$estimate_area, tolerance = 1e-6)
})

test_that("forest-area estimate equals the EXPNS-weighted forest proportion", {
  p <- skip_if_no_demo()
  plc <- compiled_plots(p)

  area <- fia_estimate(plc, by = "COUNTYCD", type = "area")
  direct <- aggregate(v ~ COUNTYCD,
                      data = data.frame(COUNTYCD = plc$COUNTYCD,
                                        v = plc$EXPNS * plc$CONDPROP_FOR), FUN = sum)
  m <- merge(area[, c("COUNTYCD", "estimate")], direct, by = "COUNTYCD")
  expect_equal(m$estimate, m$v, tolerance = 1e-6)
})

# ---- components sum to the whole -------------------------------------------

test_that("species component totals sum to the overall total", {
  p <- skip_if_no_demo()
  plc <- compiled_plots(p, fns_compute = list(ba_ft, dbcl, fia_vol, spp_y),
                        vars_group = c("vol_cf_net"))

  whole <- fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "total")
  comps <- fia_estimate(plc, vars = "^vol_cf_net_SPCD_", by = "COUNTYCD", type = "total")

  comp_sum <- aggregate(estimate ~ COUNTYCD, data = comps, FUN = sum)
  m <- merge(whole[, c("COUNTYCD", "estimate")], comp_sum, by = "COUNTYCD",
             suffixes = c("_whole", "_comp"))
  expect_equal(m$estimate_whole, m$estimate_comp, tolerance = 1e-6)
})

# ---- fia_components_long ----------------------------------------------------

test_that("fia_components_long parses SPCD and dbcl from variable names", {
  est <- data.frame(
    COUNTYCD = 1,
    variable = c("vol_cf_net_SPCD_202", "vol_cf_net_dbcl_15",
                 "vol_cf_net_SPCD_202_dbcl_15", "vol_cf_net"),
    estimate = 1:4, stringsAsFactors = FALSE
  )
  out <- fia_components_long(est)
  expect_equal(out$SPCD, c(202L, NA, 202L, NA))
  expect_equal(out$dbcl, c(NA, 15, 15, NA))
  expect_equal(out$attribute, rep("vol_cf_net", 4))
})

# ---- fia_estimate_annual ----------------------------------------------------

test_that("fia_estimate_annual stacks per-year estimates with EVALID/year", {
  p <- skip_if_no_demo()
  db <- fia_db(p)

  res <- fia_estimate_annual(db, statecd = 53, years = c(2019, 2021),
                             vars = "vol_cf_net", by = "COUNTYCD",
                             type = "per_acre")
  expect_true(all(c("EVALID", "year", "COUNTYCD", "estimate") %in% names(res)))
  expect_setequal(unique(res$year), c(2019, 2021))
  expect_setequal(unique(res$EVALID), c(531901, 532101))
})

# ---- fia_estimate_change ----------------------------------------------------

test_that("fia_estimate_change (remeas) returns finite paired-plot change", {
  p <- skip_if_no_demo()
  db <- fia_db(p)

  ch <- fia_estimate_change(db, statecd = 53,
                            evalid_t1 = 531901, evalid_t2 = 532101,
                            mode = "remeas", vars = "vol_cf_net",
                            by = "COUNTYCD", type = "per_acre")
  expect_true(all(c("COUNTYCD", "variable", "estimate", "se") %in% names(ch)))
  expect_equal(unique(ch$variable), "vol_cf_net")
  expect_true(all(is.finite(ch$estimate)))
  expect_true(all(ch$variance >= 0))
})

test_that("fia_estimate_change grm mode is a clear not-implemented stop", {
  p <- skip_if_no_demo()
  db <- fia_db(p)
  expect_error(
    fia_estimate_change(db, statecd = 53, evalid_t1 = 531901, evalid_t2 = 532101,
                        mode = "grm", vars = "vol_cf_net"),
    "grm"
  )
})

# ---- input validation -------------------------------------------------------

test_that("fia_estimate errors on missing design columns and unmatched vars", {
  p <- skip_if_no_demo()
  plc <- compiled_plots(p)

  expect_error(fia_estimate(plc[, !names(plc) %in% "STRATUM_CN"], vars = "vol_cf_net",
                            type = "total"), "STRATUM_CN")
  expect_error(fia_estimate(plc, vars = "no_such_column", type = "total"),
               "no metric columns matched")
})
