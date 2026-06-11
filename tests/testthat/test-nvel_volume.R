library(testthat)

# Tests for NVEL_volume (exported) and internals .formatTL2NVEL,
# .fn_fortran_vol, and the DLL loader .nvel_load_dll.

# ---- NVEL_volume (FIXED) ---------------------------------------------------

# Now FIXED: the DLL loader is the 2-arg RForInvt:::.nvel_load_dll(dll_64,
# dll_32), and NVEL_volume returns volume columns.
testthat::test_that("NVEL_volume returns volume columns", {
  testthat::skip_if_not(nvel_dll_available())
  tl <- data.frame(
    region   = 6,
    forest   = "01",
    district = "01",
    dbh      = c(12, 15),
    ht       = c(70, 80),
    spcd     = c(202, 122)
  )
  res <- NVEL_volume(dfTL = tl)
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_true(all(c("TCFV", "BFV_GRS") %in% names(res)))
})

# ---- .formatTL2NVEL2 (pure R, WORKS) ---------------------------------------

testthat::test_that(".formatTL2NVEL2 standardizes columns (no DLL)", {
  r <- RForInvt:::.formatTL2NVEL2(
    dfTL0     = data.frame(region = 6, forest = "01", district = "01",
                           dbh = 12, ht = 70, spcd = 202),
    voleq     = NA,
    region    = NA,
    forest    = NA,
    district  = NA,
    voleqNm   = "voleq",
    regionNm  = "region",
    forestNm  = "forest",
    districtNm = "district",
    spcdNm    = "spcd",
    dbhNm     = "dbh",
    htNm      = "ht",
    pulpDbNm  = "pulpDb",
    sawDbNm   = "sawDb",
    htPrd1Nm  = "htPrd1",
    htPrd2Nm  = "htPrd2",
    upHt1Nm   = "upHt1",
    upDb1Nm   = "upDb1",
    stumpHtNm = "stumpHt",
    fclassNm  = "fclass",
    dbtbhNm   = "dbtbh",
    btrNm     = "btr"
  )
  testthat::expect_true(all(c("dbh", "ht", "spcd", "pulpDb", "fclass") %in% names(r)))
  testthat::expect_equal(r$pulpDb, 0)
})

# ---- .fn_fortran_vol / .nvel_load_dll DLL smoke test -----------------------

testthat::test_that("vollib_r symbol resolves when DLL loaded", {
  testthat::skip_if_not(nvel_dll_available())
  RForInvt:::.nvel_load_dll(nvel_dll_path(), nvel_dll_path())
  testthat::expect_true(is.loaded("vollib_r"))
})
