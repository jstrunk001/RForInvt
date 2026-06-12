library(testthat)

# Tests for R/fia_compile.R (Phase 2 FIA compilation layer). These build on the
# bundled western-WA fixture inst/extdata/FIADB_demo.db via the Phase-1
# accessors, then compile trees -> plots and check the FIA-correct behaviors:
# tidy volume columns, per-acre weighted sums through TPA_EXP, and zero-filling
# of treeless/nonforest plots.

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

# assemble the compiled tree + plot frames once per test (cheap on the fixture)
fia_demo_frames <- function(p) {
  db <- fia_db(p)
  ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
  list(
    ev = ev,
    tr = fia_trees(db, evalid = ev),
    pl = fia_plots(db, evalid = ev)
  )
}

# ---- fia_compile_trees -----------------------------------------------------

test_that("fia_compile_trees adds ba, diameter class, tidy volume, and stems", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)

  trc <- fia_compile_trees(f$tr, vol_source = "fiadb")
  expect_s3_class(trc, "data.frame")
  expect_true(all(c("ba_ft", "dbcl", "vol_cf_net", "vol_bf_net", "dry_bio",
                    "stems") %in% names(trc)))

  # ba_ft is the basal-area-in-ft^2 constant (pi/576) times DIA^2; stems is the
  # per-tree count constant used to roll up trees-per-acre downstream
  expect_equal(trc$ba_ft, 0.005454154 * trc$DIA^2)
  expect_true(all(trc$stems == 1))

  # fiadb volume columns are copied verbatim onto the tidy names
  expect_equal(trc$vol_cf_net, f$tr$VOLCFNET)
  expect_equal(trc$dry_bio,    f$tr$DRYBIO_AG)

  # diameter class is populated and within the requested 2-inch breaks
  expect_false(any(is.na(trc$dbcl)))
})

test_that("fia_compile_trees accepts a custom fns_compute (components)", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)

  # add species breakdown of basal area + volume
  trc <- fia_compile_trees(
    f$tr, vol_source = "fiadb",
    fns_compute = list(ba_ft, dbcl, fia_vol, spp_y),
    vars_group  = c("ba_ft", "vol_cf_net")
  )
  # spp_y creates wide columns keyed by SPCD, e.g. ba_ft_SPCD_202
  expect_true(any(grepl("^ba_ft_SPCD_", names(trc))))
  expect_true(any(grepl("^vol_cf_net_SPCD_", names(trc))))
})

# ---- fia_vol ---------------------------------------------------------------

test_that("fia_vol(fiadb) maps FIADB columns and warns when absent", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)

  out <- fia_vol(f$tr, tree_nms = RForInvt:::.fia_tree_nms, vol_source = "fiadb")
  expect_true(all(c("vol_cf_net", "vol_bf_net", "dry_bio") %in% names(out)))

  # a frame lacking the FIADB volume columns triggers a warning
  bare <- f$tr[, c("PLT_CN", "DIA", "HT", "SPCD", "TPA_EXP")]
  expect_warning(
    fia_vol(bare, tree_nms = RForInvt:::.fia_tree_nms, vol_source = "fiadb"),
    "volume columns"
  )
})

# ---- fia_compile_plots -----------------------------------------------------

test_that("fia_compile_plots produces per-acre sums and carries design columns", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)
  trc <- fia_compile_trees(f$tr, vol_source = "fiadb")

  plc <- suppressWarnings(fia_compile_plots(trc, f$pl))
  expect_s3_class(plc, "data.frame")

  # one row per plot in the evaluation
  expect_equal(nrow(plc), length(unique(f$pl$PLT_CN)))
  expect_false(any(duplicated(plc$PLT_CN)))

  # design columns are carried through
  expect_true(all(c("EXPNS", "COUNTYCD", "ESTN_UNIT", "STRATUM_CN",
                    "CONDPROP_FOR") %in% names(plc)))

  # per-acre basal area equals sum(per-tree ba_ft * TPA_EXP) on a treed plot
  p1  <- trc$PLT_CN[1]
  man <- sum(trc$ba_ft[trc$PLT_CN == p1] * trc$TPA_EXP[trc$PLT_CN == p1])
  expect_equal(plc$ba_ft[plc$PLT_CN == p1], man, tolerance = 1e-6)
})

test_that("fia_compile_plots zero-fills treeless and nonforest plots", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)
  trc <- fia_compile_trees(f$tr, vol_source = "fiadb")
  plc <- suppressWarnings(fia_compile_plots(trc, f$pl))

  # plots with no compiled trees must still appear, with zeroed metrics
  treed   <- unique(trc$PLT_CN)
  treeless <- plc[!plc$PLT_CN %in% treed, , drop = FALSE]
  expect_gt(nrow(treeless), 0)            # the fixture has nonforest plots
  expect_true(all(treeless$stems == 0))
  expect_true(all(treeless$ba_ft == 0))
  expect_true(all(treeless$vol_cf_net == 0))

  # totals are unaffected by the zero rows (they contribute 0)
  expect_equal(sum(plc$vol_cf_net),
               sum(plc$vol_cf_net[plc$PLT_CN %in% treed]))
})

test_that("fia_compile_plots respects an explicit sum_nms", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)
  trc <- fia_compile_trees(f$tr, vol_source = "fiadb")

  plc <- suppressWarnings(
    fia_compile_plots(trc, f$pl, sum_nms = c("ba_ft", "vol_cf_net"))
  )
  # plot_lor_qmd still contributes its columns; the summed metrics are present
  expect_true(all(c("ba_ft", "vol_cf_net") %in% names(plc)))
})

# ---- end-to-end shape ------------------------------------------------------

test_that("full fia_trees -> compile_trees -> compile_plots pipeline runs", {
  p <- skip_if_no_demo()
  f <- fia_demo_frames(p)

  plc <- suppressWarnings(fia_compile_plots(
    fia_compile_trees(f$tr, vol_source = "fiadb"), f$pl
  ))
  # ready for estimation: design weight + a per-acre response, no NAs in metrics
  expect_false(any(is.na(plc$EXPNS)))
  expect_false(any(is.na(plc$vol_cf_net)))
  expect_true(all(plc$vol_cf_net >= 0))
})
