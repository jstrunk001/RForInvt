# Revision history:
# 2026-06-10 — JLS — DLL-gated tests for the model_class == "nvel" path:
#                    register -> load -> predict round-trip and shipped-row
#                    prediction. Skipped where the vollib DLL is unavailable.

skip_if_no_dll = function(){
  if (!RForInvt:::.nvel_dll_available()) skip("NVEL vollib DLL not available")
}

test_that("nvel_volume_register round-trips and predicts a positive volume", {

  skip_if_no_dll()
  arch = file.path(tempdir(), "rforinvt_nvel_reg")
  unlink(arch, recursive = TRUE)

  nvel_volume_register(archive_dir = arch, spcd = 131, region = 8,
                       spp = "pinus.taeda", region_code = "se")

  loaded = model_archive_load(model_id = "NVEL_131_r8", archive_dir = arch,
                              package = NULL)
  expect_length(loaded, 1)
  row = loaded[[1]]$row
  expect_equal(row$model_class[1], "nvel")
  expect_null(loaded[[1]]$fit)   # no serialized fit for DLL-backed rows

  pred = model_predict(
    dat = data.table::data.table(dbh = c(8, 12, 16), ht = c(50, 70, 90)),
    nms_dat_predict = c(dbh = "dbh", ht = "ht"),
    params = row, archive_dir = arch, package = NULL
  )
  v = pred[[row$response[1]]]
  expect_equal(length(v), 3)
  expect_true(all(is.finite(v)))
  expect_true(all(v > 0))
  expect_true(all(diff(v) > 0))   # volume increases with size

})

test_that("shipped NVEL rows predict total cubic volume", {

  skip_if_no_dll()
  reg = system.file("extdata", "models.csv", package = "RForInvt")
  skip_if(!nzchar(reg) || !file.exists(reg), "shipped models.csv not available")

  hits = model_archive_load(filter = quote(model_class == "nvel"),
                            package = "RForInvt")
  skip_if(length(hits) == 0, "no shipped nvel rows")
  row = hits[[1]]$row

  pred = model_predict(
    dat = data.table::data.table(dbh = 10, ht = 60),
    nms_dat_predict = c(dbh = "dbh", ht = "ht"),
    params = row, package = "RForInvt"
  )
  v = as.numeric(pred[[row$response[1]]][1])
  expect_true(is.finite(v))
  expect_gt(v, 0)

})
