library(testthat)

# Tests for NVEL_biomass (exported but MASKED) and internals
# .formatTL2NVEL_bio, .fn_fortran_bio.

# ---- NVEL_biomass (MASKED at baseline) -------------------------------------

# FAILS at baseline: the working NVEL_biomass in NVEL_biomass.R is masked by an
# empty stub `NVEL_biomass = function(){}` defined at NVEL_volume.R:460, so
# args(NVEL_biomass) is `function()` and calling it with dfTL=/bioeq= errors
# `unused argument`.
testthat::test_that("NVEL_biomass returns biomass columns (intended)", {
  testthat::skip_if_not(nvel_dll_available())
  res <- NVEL_biomass(
    dfTL  = data.frame(spcd = 202, dbh = 15, ht = 65, cl = 30),
    bioeq = "AFF019AST01D"
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(all(c("biogrn", "biodry", "err") %in% names(res)))
})

# ---- .formatTL2NVEL_bio (pure R, WORKS) ------------------------------------

testthat::test_that(".formatTL2NVEL_bio standardizes columns (no DLL)", {
  r <- RForInvt:::.formatTL2NVEL_bio(
    dfTL0     = data.frame(spcd = 202, dbh = 15, ht = 65, cl = 30),
    bioeq     = NA,
    geosub    = NA,
    bioeqNm   = "bioeq",
    geosubNm  = "geosub",
    spcdNm    = "spcd",
    dbhNm     = "dbh",
    htNm      = "ht",
    stemsNm   = "stems",
    clNm      = "cl",
    merchDbNm = "merchDb",
    htPrd1Nm  = "htPrd1",
    htPrd2Nm  = "htPrd2",
    cv4Nm     = "cv4"
  )
  testthat::expect_true(all(c("bioeq", "geosub", "stems", "cl", "merchDb", "cv4") %in% names(r)))
})
