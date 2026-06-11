library(testthat)

# Tests for NVEL_volume (exported) and internals .formatTL2NVEL,
# .fn_fortran_vol, .load_dll, plus the dead stubs NVEL_ht2topd / NVEL_calcdob.

# ---- NVEL_volume (BROKEN at baseline) --------------------------------------

# FAILS at baseline: NVEL_volume calls .load_dll(dll_64, dll_32, dll_func_vol)
# but the only surviving .load_dll in the namespace is the 2-arg version from
# NVEL_wtfactor.R, so it errors with `unused argument (dll_func_vol)`.
testthat::test_that("NVEL_volume returns volume columns (intended)", {
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

# ---- .formatTL2NVEL (pure R, WORKS) ----------------------------------------

testthat::test_that(".formatTL2NVEL standardizes columns (no DLL)", {
  r <- RForInvt:::.formatTL2NVEL(
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

# ---- .fn_fortran_vol / .load_dll DLL smoke test ----------------------------

testthat::test_that("vollib_r symbol resolves when DLL loaded", {
  testthat::skip_if_not(nvel_dll_available())
  # .load_dll here is the surviving 2-arg version (NVEL_wtfactor.R).
  RForInvt:::.load_dll(nvel_dll_path(), nvel_dll_path())
  testthat::expect_true(is.loaded("vollib_r"))
})

# ---- dead stubs ------------------------------------------------------------

testthat::test_that("NVEL_ht2topd and NVEL_calcdob are empty stubs", {
  testthat::expect_null(RForInvt:::NVEL_ht2topd())
  testthat::expect_null(RForInvt:::NVEL_calcdob())
})
