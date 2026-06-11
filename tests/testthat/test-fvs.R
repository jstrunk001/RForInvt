library(testthat)

# ---------------------------------------------------------------------------
# fvs_prototype_params() - pure, builds an empty template parameter data.frame
# ---------------------------------------------------------------------------

testthat::test_that("fvs_prototype_params returns an empty template with the expected columns", {
  p <- fvs_prototype_params()
  testthat::expect_true(is.data.frame(p))
  testthat::expect_equal(nrow(p), 0L)
  testthat::expect_identical(
    names(p),
    c("std_cn", "std_id", "invyr", "timeint", "numcycle",
      "input_db", "fvs_path", "tree_table", "stand_table")
  )
})

testthat::test_that("fvs_prototype_params appends additional parameter columns via add_params", {
  p1 <- fvs_prototype_params(add_params = ",extra1")
  testthat::expect_true("extra1" %in% names(p1))
  testthat::expect_equal(length(names(p1)), 10L)

  p2 <- fvs_prototype_params(add_params = ",extra1,extra2")
  testthat::expect_true(all(c("extra1", "extra2") %in% names(p2)))
  testthat::expect_equal(length(names(p2)), 11L)
})

# ---------------------------------------------------------------------------
# fvs_prototype_keyfile() - pure string builder
# ---------------------------------------------------------------------------

testthat::test_that("fvs_prototype_keyfile builds the default keyword prototype", {
  out <- fvs_prototype_keyfile()
  testthat::expect_true(is.character(out))
  testthat::expect_equal(length(out), 48L)

  # anchor lines that must be present in the default prototype
  testthat::expect_true(any(grepl("StdIdent", out)))
  testthat::expect_true(any(out == "InvYear       @invyr@"))
  testthat::expect_true(any(grepl("DSNOut", out)))
  testthat::expect_true(any(grepl("@output_db_q@", out)))
  testthat::expect_true(any(grepl("SELECT \\* FROM @stand_table@", out)))
  testthat::expect_true(any(out == "Process"))
  testthat::expect_true(any(out == "Stop"))
})

testthat::test_that("fvs_prototype_keyfile honors optional notriple keyword", {
  with_nt <- fvs_prototype_keyfile(notriple = "NoTriple")
  testthat::expect_equal(length(with_nt), 49L)
  testthat::expect_true("NoTriple" %in% with_nt)

  without_nt <- fvs_prototype_keyfile(notriple = NULL)
  testthat::expect_false("NoTriple" %in% without_nt)
  testthat::expect_equal(length(without_nt), 48L)
})

# ---------------------------------------------------------------------------
# .write_batch() - internal single-keyfile writer
# ---------------------------------------------------------------------------

testthat::test_that(".write_batch substitutes tokens and writes a keyfile", {
  tmp <- withr::local_tempdir()
  line <- data.frame(
    std_id       = "S1",
    invyr        = 2005,
    output_db_q  = "out.db",
    stand_table  = "st",
    key_path     = file.path(tmp, "x.key"),
    stringsAsFactors = FALSE
  )
  proto <- c("@std_id@", "InvYear @invyr@", "@output_db_q@", "FROM @stand_table@")

  p <- RForInvt:::.write_batch(line, proto)

  testthat::expect_equal(p, line$key_path)
  testthat::expect_true(file.exists(p))
  testthat::expect_identical(
    readLines(p),
    c("S1", "InvYear 2005", "out.db", "FROM st")
  )
})

testthat::test_that(".write_batch converts backslashes to forward slashes in substituted values", {
  tmp <- withr::local_tempdir()
  line <- data.frame(
    std_id   = "a\\b",
    key_path = file.path(tmp, "bslash.key"),
    stringsAsFactors = FALSE
  )
  proto <- c("@std_id@")

  p <- RForInvt:::.write_batch(line, proto)
  testthat::expect_identical(readLines(p), "a/b")
})

# ---------------------------------------------------------------------------
# fvs_make_keyfiles() - end-to-end keyfile generation (touches local SQLite)
# ---------------------------------------------------------------------------

testthat::test_that("fvs_make_keyfiles generates one fully-substituted keyfile per row", {
  testthat::skip_if_not_installed("RSQLite")

  pd <- fvs_prototype_params()
  pd <- pd[FALSE, ]
  n <- 3L
  pd[1:n, ] <- NA
  pd[, "std_cn"]      <- paste0("cn", 1:n)
  pd[, "std_id"]      <- paste0("stand", 1:n)
  pd[, "invyr"]       <- 2000:(2000 + n - 1)
  pd[, "timeint"]     <- 1
  pd[, "numcycle"]    <- 1
  pd[, "input_db"]    <- file.path(tempdir(), "fordata.db")
  pd[, "fvs_path"]    <- "C:/FVSbin/FVSca.exe"
  pd[, "tree_table"]  <- "fvs_treeinit"
  pd[, "stand_table"] <- "fvs_standinit"

  proc <- file.path(withr::local_tempdir(), "fvs_test")

  out <- fvs_make_keyfiles(
    param_df       = pd,
    processing_dir = proc,
    id             = "std_id",
    cluster        = NA
  )

  testthat::expect_true(is.data.frame(out))
  testthat::expect_equal(nrow(out), nrow(pd))

  # new columns the function adds
  testthat::expect_true(all(
    c("input_db_q", "cluster", "fvs_dir", "output_db", "output_db_q", "key_path")
    %in% names(out)
  ))

  # one keyfile per row
  keys <- list.files(file.path(proc, "keyfiles"), pattern = "\\.key$")
  testthat::expect_equal(length(keys), nrow(pd))

  # all @token@ substitutions resolved in the written keyfile
  testthat::expect_true(file.exists(out$key_path[1]))
  testthat::expect_false(any(grepl("@", readLines(out$key_path[1]))))
})

# ---------------------------------------------------------------------------
# fvs_load() / fvs_run() - require external resources, skipped
# ---------------------------------------------------------------------------

testthat::test_that("fvs_load requires network + installed-pkg writes", {
  testthat::skip("requires network + writes to installed package dir")
})

testthat::test_that("fvs_run executes FVS end-to-end and merges the output database", {
  # Integration test: builds a keyfile for one shipped FIA stand, runs the FVS
  # Inland Empire executable, and checks the merged output DB. Skips cleanly
  # when FVS is not installed (set RFORINVT_FVS_DIR to point at an FVS*.exe dir).
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("DBI")
  fvs <- fvs_exe("FVSie")
  testthat::skip_if(!nzchar(fvs), "FVS executable (FVSie) not found")
  db_src <- system.file("extdata", "FIADB_RI.db", package = "RForInvt")
  testthat::skip_if(!nzchar(db_src) || !file.exists(db_src), "FIADB_RI.db not available")

  db_src <- normalizePath(db_src, winslash = "/")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_src)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  sid <- DBI::dbGetQuery(con,
    "SELECT STAND_ID, COUNT(*) n FROM FVS_TREEINIT_PLOT GROUP BY STAND_ID
     ORDER BY n DESC LIMIT 1")$STAND_ID
  inv <- DBI::dbGetQuery(con,
    sprintf("SELECT INV_YEAR FROM FVS_STANDINIT_PLOT WHERE STAND_ID='%s'", sid))$INV_YEAR[1]

  proc <- file.path(tempdir(), paste0("fvs_run_", as.integer(runif(1, 1, 1e8))))

  pd <- fvs_prototype_params()
  pd[1, ] <- NA
  pd$std_id <- sid; pd$std_cn <- sid
  pd$invyr <- inv; pd$timeint <- 10; pd$numcycle <- 1
  pd$input_db <- db_src
  pd$fvs_path <- fvs
  pd$tree_table  <- "FVS_TREEINIT_PLOT"
  pd$stand_table <- "FVS_STANDINIT_PLOT"

  df_keys <- fvs_make_keyfiles(
    pd, key_proto = fvs_prototype_keyfile(notriple = NULL),
    processing_dir = proc, id = "std_id", cluster = NA
  )
  testthat::expect_true(file.exists(df_keys$key_path[1]))

  suppressWarnings(suppressMessages(
    fvs_run(df_keys, db_merge = "FVS_Out.db", cluster = NA,
            merge_dbs = TRUE, delete_temp_db = FALSE)
  ))

  out_db <- file.path(dirname(df_keys$output_db[1]), "FVS_Out.db")
  testthat::expect_true(file.exists(out_db))

  con2 <- DBI::dbConnect(RSQLite::SQLite(), out_db)
  on.exit(try(DBI::dbDisconnect(con2), silent = TRUE), add = TRUE)
  tbs <- DBI::dbListTables(con2)
  # FVS always records a case; a 1-cycle projection yields >=1 summary row
  testthat::expect_true("FVS_Cases" %in% tbs)
  testthat::expect_gt(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM FVS_Cases")$n, 0)
  testthat::expect_true(any(grepl("^FVS_Summary", tbs)))
})

