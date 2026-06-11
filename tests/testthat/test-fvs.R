library(testthat)

# ---------------------------------------------------------------------------
# fvs_protype_params() - pure, builds an empty template parameter data.frame
# ---------------------------------------------------------------------------

testthat::test_that("fvs_protype_params returns an empty template with the expected columns", {
  p <- fvs_protype_params()
  testthat::expect_true(is.data.frame(p))
  testthat::expect_equal(nrow(p), 0L)
  testthat::expect_identical(
    names(p),
    c("std_cn", "std_id", "invyr", "timeint", "numcycle",
      "input_db", "fvs_path", "tree_table", "stand_table")
  )
})

testthat::test_that("fvs_protype_params appends additional parameter columns via add_params", {
  p1 <- fvs_protype_params(add_params = ",extra1")
  testthat::expect_true("extra1" %in% names(p1))
  testthat::expect_equal(length(names(p1)), 10L)

  p2 <- fvs_protype_params(add_params = ",extra1,extra2")
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

  pd <- fvs_protype_params()
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

testthat::test_that("fvs_run requires the external FVS binary", {
  testthat::skip("requires FVS binary")
})
