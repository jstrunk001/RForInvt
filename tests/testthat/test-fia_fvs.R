library(testthat)

# Tests for R/fia_fvs.R (Phase 4 FVS bridge). The build-from-FIA input path and
# the tree-list compile path are exercised against the bundled synthetic fixture
# inst/extdata/FIADB_demo.db; an actual FVS projection (fia_fvs_run) needs a
# local FVS executable and is not run here.

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

# ---- fia_fvs_input: build from PLOT/TREE/COND ------------------------------

test_that("fia_fvs_input builds FVS_StandInit/FVS_TreeInit from FIA tables", {
  p  <- skip_if_no_demo()
  db <- fia_db(p)
  ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)

  pl <- fia_plots(db, evalid = ev)
  tr <- fia_trees(db, evalid = ev)            # default = live trees
  n_stands_exp <- length(unique(pl$PLT_CN))
  n_trees_exp  <- nrow(tr)

  out <- fia_fvs_input(db, evalid = ev, dir_out = tempfile("fvsin"),
                       variant = "PN")
  expect_true(file.exists(out))
  expect_equal(attr(out, "variant"), "PN")
  expect_equal(attr(out, "n_stands"), n_stands_exp)
  expect_equal(attr(out, "n_trees"),  n_trees_exp)

  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  expect_true(all(c("FVS_StandInit", "FVS_TreeInit") %in% DBI::dbListTables(con)))

  st <- DBI::dbGetQuery(con, "select * from FVS_StandInit")
  tt <- DBI::dbGetQuery(con, "select * from FVS_TreeInit")

  # stand = plot: one row per plot, STAND_CN = PLT_CN, variant carried
  expect_equal(nrow(st), n_stands_exp)
  expect_setequal(st$STAND_CN, as.character(unique(pl$PLT_CN)))
  expect_true(all(st$VARIANT == "PN"))
  expect_false(any(is.na(st$INV_YEAR)))

  # tree records: TREE_COUNT carries TPA_UNADJ, SPECIES gets FVS alpha codes
  # via the built-in westside crosswalk, the FIA SPCD is kept as a bonus column,
  # HISTORY marks live trees, every tree maps to a known stand
  expect_equal(nrow(tt), n_trees_exp)
  expect_equal(sort(tt$TREE_COUNT), sort(tr$TPA_UNADJ), tolerance = 1e-9)
  expect_true("SPCD" %in% names(tt))
  expect_setequal(unique(tt$SPCD), unique(tr$SPCD))
  # the five demo species resolve to their FVS alpha codes
  xwalk <- c("202" = "DF", "263" = "WH", "242" = "RC", "351" = "RA", "17" = "GF")
  expect_equal(tt$SPECIES, unname(xwalk[as.character(tt$SPCD)]))
  expect_true(all(tt$HISTORY == 1))
  expect_true(all(tt$STAND_CN %in% st$STAND_CN))
})

test_that("fia_fvs_input honors spp_map and overwrite", {
  p  <- skip_if_no_demo()
  db <- fia_db(p)
  ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)

  spp_map <- c("202" = "DF", "263" = "WH", "242" = "RC", "351" = "RA", "17" = "GF")
  dir_out <- tempfile("fvsmap"); db_name <- "in.db"

  out <- fia_fvs_input(db, evalid = ev, dir_out = dir_out, db_name = db_name,
                       variant = "PN", spp_map = spp_map)
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con))
  tt <- DBI::dbGetQuery(con, "select * from FVS_TreeInit")
  expect_true(all(tt$SPECIES %in% unname(spp_map)))

  # overwrite = TRUE on a second build does not duplicate rows
  n1 <- nrow(tt)
  DBI::dbDisconnect(con); on.exit()
  out2 <- fia_fvs_input(db, evalid = ev, dir_out = dir_out, db_name = db_name,
                        variant = "PN", spp_map = spp_map, overwrite = TRUE)
  con2 <- DBI::dbConnect(RSQLite::SQLite(), out2)
  on.exit(DBI::dbDisconnect(con2))
  expect_equal(nrow(DBI::dbGetQuery(con2, "select * from FVS_TreeInit")), n1)
})

# ---- fia_fvs_input: filter pre-existing DataMart FVS tables -----------------

test_that("fia_fvs_input filters existing FVS_*INIT_PLOT tables to the evaluation", {
  p  <- skip_if_no_demo()
  db <- fia_db(p)
  ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
  plt_cns <- unique(fia_plots(db, evalid = ev)$PLT_CN)

  # synthesize DataMart-style FVS tables, including an out-of-evaluation stand
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  tabs <- list(
    PLOT = DBI::dbGetQuery(con, "select * from PLOT"),
    POP_PLOT_STRATUM_ASSGN = DBI::dbGetQuery(con, "select * from POP_PLOT_STRATUM_ASSGN")
  )
  DBI::dbDisconnect(con)
  tabs$FVS_STANDINIT_PLOT <- data.frame(
    STAND_CN = as.character(c(plt_cns, "ZZZ_not_in_eval")),
    STAND_ID = c(seq_along(plt_cns), 9999),
    VARIANT  = "PN", INV_YEAR = 2021, stringsAsFactors = FALSE)
  tabs$FVS_TREEINIT_PLOT <- data.frame(
    STAND_CN = as.character(c(plt_cns[1], plt_cns[1], "ZZZ_not_in_eval")),
    TREE_ID = 1:3, SPECIES = "202", DIAMETER = 10, HT = 60,
    TREE_COUNT = 6, stringsAsFactors = FALSE)

  dblist <- fia_db(tables = tabs)
  out <- fia_fvs_input(dblist, evalid = ev, dir_out = tempfile("fvsdm"),
                       variant = "PN")

  con2 <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con2))
  st <- DBI::dbGetQuery(con2, "select * from FVS_StandInit")
  tt <- DBI::dbGetQuery(con2, "select * from FVS_TreeInit")
  # the out-of-evaluation stand/tree is dropped
  expect_false("ZZZ_not_in_eval" %in% st$STAND_CN)
  expect_false("ZZZ_not_in_eval" %in% tt$STAND_CN)
  expect_setequal(st$STAND_CN, as.character(plt_cns))
})

# ---- fia_fvs_compile: map FVS_TreeList onto fia conventions -----------------

# build a tiny synthetic FVS output database (two stands, two projection years)
make_fvs_out <- function() {
  path <- tempfile("fvsout", fileext = ".db")
  tl <- data.frame(
    StandID    = c("1", "1", "2", "1", "1", "2"),
    StandCN    = c("100", "100", "200", "100", "100", "200"),
    Year       = c(2021, 2021, 2021, 2031, 2031, 2031),
    TreeId     = c(1, 2, 1, 1, 2, 1),
    SpeciesFIA = c(202, 263, 202, 202, 263, 202),
    SpeciesFVS = c("DF", "WH", "DF", "DF", "WH", "DF"),
    TPA        = c(5, 10, 8, 5, 10, 8),
    DBH        = c(10, 8, 12, 13, 11, 15),
    Ht         = c(60, 50, 70, 75, 64, 85),
    MCuFt      = c(20, 12, 30, 35, 22, 48),
    BdFt       = c(80, 40, 130, 160, 95, 240),
    TCuFt      = c(24, 15, 35, 40, 26, 55),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "FVS_TreeList", tl)
  DBI::dbDisconnect(con)
  path
}

test_that("fia_fvs_compile renames FVS columns onto fia_trees conventions", {
  testthat::skip_if_not_installed("RSQLite")
  out <- make_fvs_out()

  tr <- fia_fvs_compile(out, year = 2031)
  expect_s3_class(tr, "data.frame")
  expect_true(all(c("PLT_CN", "SPCD", "SPP_FVS", "DIA", "HT", "TPA_EXP",
                    "VOLCFNET", "VOLCSNET", "YEAR") %in% names(tr)))
  expect_true(all(tr$YEAR == 2031))
  expect_equal(nrow(tr), 3L)

  # both species codes are carried: FIA numeric in SPCD, FVS alpha in SPP_FVS
  expect_setequal(unique(tr$SPP_FVS), c("DF", "WH"))
  expect_true(all((tr$SPCD == 202) == (tr$SPP_FVS == "DF")))

  # PLT_CN = StandCN; FVS per-acre TPA is the expansion
  expect_setequal(unique(tr$PLT_CN), c("100", "200"))
  expect_equal(tr$TPA_EXP, tr$TPA_UNADJ)

  # the projected tree list flows through the FIA compile pipeline
  tr_c <- fia_compile_trees(tr, vol_source = "fiadb")
  expect_true(all(c("ba_ft", "dbcl", "vol_cf_net") %in% names(tr_c)))
  expect_equal(tr_c$vol_cf_net, tr$VOLCFNET)
})

test_that("fia_fvs_compile returns all years when year is NULL and errors on bad table", {
  testthat::skip_if_not_installed("RSQLite")
  out <- make_fvs_out()

  tr_all <- fia_fvs_compile(out, year = NULL)
  expect_setequal(unique(tr_all$YEAR), c(2021, 2031))
  expect_equal(nrow(tr_all), 6L)

  expect_error(fia_fvs_compile(out, treelist_table = "NoSuchTable"),
               "not found")
})
