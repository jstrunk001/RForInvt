# Revision history:
# 2026-06-10 — JLS — tests for the shipped NBEL biomass archive: catalog
#                    presence, closed-form predict pipeline vs an independent
#                    coefficient substitution, and the plausibility guarantee.

# The shipped registry lives in the package's installed extdata/ (under
# devtools::test() / load_all this resolves to inst/extdata/models.csv).

skip_if_no_archive = function(){
  reg = system.file("extdata", "models.csv", package = "RForInvt")
  if (!nzchar(reg) || !file.exists(reg)) skip("shipped models.csv not available")
}

# Independent reference: substitute the b* values straight into the formula
# string with gsub and eval — a different mechanism than model_predict()'s
# substitute()/eval(), so agreement exercises the whole predict wiring.
indep_eval = function(row, dat){
  f = row$formula[1]
  for (b in paste0("b", 0:8)){
    val = suppressWarnings(as.numeric(row[[b]][1]))
    if (is.na(val)) val = 0
    f = gsub(paste0("\\b", b, "\\b"), sprintf("(%.12g)", val), f)
  }
  eval(parse(text = f), envir = as.list(dat))
}

test_that("shipped archive catalogs NBEL biomass and NVEL volume models", {

  skip_if_no_archive()
  cat_all = model_archive_list(package = "RForInvt")
  expect_s3_class(cat_all, "data.table")
  expect_true(nrow(cat_all) > 200)
  expect_true(all(c("pnw", "se") %in% cat_all$region_code))
  expect_true("closed_form" %in% cat_all$model_class)
  # biomass rows carry the unit/bias factor convention
  bm = cat_all[model_class == "closed_form"]
  expect_true(all(c("b5", "b6", "b7", "b8") %in% names(bm)))
  expect_false(any(is.na(bm[, c(paste0("b", 0:8)), with = FALSE])))

})

test_that("closed-form biomass predict matches independent coefficient substitution", {

  skip_if_no_archive()
  hit = model_archive_load(
    filter  = quote(spp == "pinus.taeda" & model_class == "closed_form"),
    package = "RForInvt"
  )
  expect_true(length(hit) >= 1)
  row = hit[[1]]$row

  dat = data.table::data.table(dbh = c(8, 12, 16), ht = c(50, 70, 90))
  pred = model_predict(
    dat = dat, nms_dat_predict = c(dbh = "dbh", ht = "ht"),
    params = row, package = "RForInvt"
  )
  got = pred[[row$response[1]]]
  ref = indep_eval(row, dat)
  expect_equal(as.numeric(got), as.numeric(ref), tolerance = 1e-8)

})

test_that("every shipped biomass model is physically plausible at a reference tree", {

  skip_if_no_archive()
  cat_bm = model_archive_list(filter = quote(model_class == "closed_form"),
                              package = "RForInvt")
  dat = data.table::data.table(dbh = 10, ht = 60)
  vals = vapply(seq_len(nrow(cat_bm)), function(i){
    pd = suppressWarnings(model_predict(
      dat = dat, nms_dat_predict = c(dbh = "dbh", ht = "ht"),
      params = cat_bm[i], package = "RForInvt"))
    as.numeric(pd[[cat_bm$response[i]]][1])
  }, numeric(1))
  expect_true(all(is.finite(vals)))
  expect_true(all(vals > 0))
  expect_true(all(vals < 1e5))

})

test_that("a height-using equation responds to height", {

  skip_if_no_archive()
  # form 16 is biomass = a*dia^b*tht^c; a row with c (b2) > 0 genuinely
  # increases with height. Confirm taller trees yield more biomass.
  cat_bm = model_archive_load(
    filter  = quote(model_class == "closed_form" & x1 == 16 & b2 > 0),
    package = "RForInvt")
  skip_if(length(cat_bm) == 0, "no height-using form-16 rows shipped")
  row = cat_bm[[1]]$row
  short = model_predict(dat = data.table::data.table(dbh = 10, ht = 40),
                        nms_dat_predict = c(dbh = "dbh", ht = "ht"),
                        params = row, package = "RForInvt")[[row$response[1]]]
  tall  = model_predict(dat = data.table::data.table(dbh = 10, ht = 90),
                        nms_dat_predict = c(dbh = "dbh", ht = "ht"),
                        params = row, package = "RForInvt")[[row$response[1]]]
  expect_gt(as.numeric(tall), as.numeric(short))

})
