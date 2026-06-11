# Revision history:
# 2026-05-22 — JLS — initial archive round-trip + list tests
# 2026-06-08 — JLS — Pass-1: archive_dir signature, uniqueness guard, and
#                    per-class fit round-trip (randomForest, xgboost) tests.

# Build a small, self-contained registry fixture in a temp archive so these
# tests depend only on this package (no external data or companion package).
make_fixture_archive = function(){
  arch = file.path(tempdir(), "rforinvt_archive_fixture")
  unlink(arch, recursive = TRUE)
  rows = list(
    data.table::data.table(model_id = 1L, model_class = "closed_form", response = "vol1",
                           spp = "pinus.taeda",     region_code = "lcp",
                           formula = "b0 + b1 * age1", b0 = 1.0, b1 = 0.5),
    data.table::data.table(model_id = 2L, model_class = "closed_form", response = "vol1",
                           spp = "pinus.taeda",     region_code = "ucp",
                           formula = "b0 + b1 * age1", b0 = 2.0, b1 = 0.4),
    data.table::data.table(model_id = 3L, model_class = "closed_form", response = "vol1",
                           spp = "pinus.elliottii", region_code = "lcp",
                           formula = "b0 + b1 * age1", b0 = 1.5, b1 = 0.6)
  )
  for (r in rows){
    model_archive_save(fit = NULL, registry_row = r, registry = "models.csv",
                       archive_dir = arch, overwrite = TRUE)
  }
  arch
}
fixture_arch = make_fixture_archive()

test_that("model_archive_list reads a registry and finds known rows", {

  cat_full = model_archive_list(registry = "models.csv", archive_dir = fixture_arch)
  expect_s3_class(cat_full, "data.table")
  expect_true("model_id" %in% names(cat_full))
  expect_true("model_class" %in% names(cat_full))
  expect_true(nrow(cat_full) > 0)

  cat_lobl = model_archive_list(filter = quote(spp == "pinus.taeda"),
                                registry = "models.csv", archive_dir = fixture_arch)
  expect_true(nrow(cat_lobl) > 0)
  expect_true(all(cat_lobl$spp == "pinus.taeda"))

})

test_that("model_archive_list applies a filter expression", {

  cat_full   = model_archive_list(registry = "models.csv", archive_dir = fixture_arch)
  cat_filter = model_archive_list(filter = quote(region_code == "lcp"),
                                  registry = "models.csv", archive_dir = fixture_arch)
  expect_true(nrow(cat_filter) <= nrow(cat_full))
  expect_true(all(cat_filter$region_code == "lcp"))

})

test_that("model_archive_load returns closed-form rows with NULL sidecars", {

  hits = model_archive_load(model_id = 1, registry = "models.csv",
                            archive_dir = fixture_arch, package = NULL)
  expect_type(hits, "list")
  expect_true(length(hits) >= 1)
  expect_named(hits[[1]], c("row", "fit", "vcov", "predictor_spec"))
  expect_s3_class(hits[[1]]$row, "data.table")
  expect_null(hits[[1]]$fit)
  expect_null(hits[[1]]$vcov)
  expect_null(hits[[1]]$predictor_spec)

})

test_that("model_predict dispatches closed-form rows through the legacy path", {

  row_one = model_archive_load(model_id = 1, registry = "models.csv",
                               archive_dir = fixture_arch, package = NULL)[[1]]$row

  dat_in = data.table::data.table(
    tpa = c(400, 450),
    ht  = c(35, 40),
    age = c(15, 18)
  )

  pred_out = model_predict(
    dat = dat_in,
    nms_dat_predict = c(tpa1 = "tpa", hd1 = "ht", age1 = "age"),
    params = row_one
  )

  expect_s3_class(pred_out, "data.table")
  expect_true(row_one$response[1] %in% names(pred_out))
  expect_equal(nrow(pred_out), 2)

})

test_that("model_archive_save round-trips a closed-form row to a temp archive", {

  arch = file.path(tempdir(), "rforinvt_archive_cf")
  unlink(arch, recursive = TRUE)

  row_new = data.table::data.table(
    model_id    = 99999L,
    model_class = "closed_form",
    response    = "ba1",
    spp         = "pinus.taeda",
    region_code = "test",
    formula     = "b0 + b1 * age1",
    b0          = 1.0,
    b1          = 0.5
  )

  saved = model_archive_save(
    fit = NULL, registry_row = row_new,
    registry = "models.csv", archive_dir = arch
  )
  expect_s3_class(saved, "data.table")

  back = model_archive_load(model_id = 99999L, registry = "models.csv",
                            archive_dir = arch, package = NULL)
  expect_length(back, 1)
  expect_equal(back[[1]]$row$model_class, "closed_form")
  expect_equal(back[[1]]$row$response, "ba1")
  expect_null(back[[1]]$fit)

  # uniqueness guard: a second save of the same model_id errors ...
  expect_error(
    model_archive_save(fit = NULL, registry_row = row_new,
                       registry = "models.csv", archive_dir = arch),
    "already exists"
  )
  # ... unless overwrite = TRUE
  expect_s3_class(
    model_archive_save(fit = NULL, registry_row = row_new,
                       registry = "models.csv", archive_dir = arch, overwrite = TRUE),
    "data.table"
  )

})

test_that("model_archive_save requires an explicit archive_dir (D1)", {

  row_new = data.table::data.table(
    model_id = 1L, model_class = "closed_form", response = "ba1",
    spp = "x", region_code = "t", formula = "b0", b0 = 1
  )
  expect_error(
    model_archive_save(fit = NULL, registry_row = row_new),
    "archive_dir"
  )

})

test_that("randomForest fit round-trips and predicts via the generic adapter", {

  skip_if_not_installed("randomForest")
  arch = file.path(tempdir(), "rforinvt_archive_rf")
  unlink(arch, recursive = TRUE)

  set.seed(1)
  train = data.frame(age1 = runif(60, 5, 40), tpa1 = runif(60, 100, 800))
  train$ba1 = 2 * train$age1 + 0.05 * train$tpa1 + rnorm(60)
  fit = randomForest::randomForest(ba1 ~ age1 + tpa1, data = train, ntree = 50)

  row = data.table::data.table(model_id = 1001L, model_class = "randomForest",
                               response = "ba1", spp = "x", region_code = "t")
  model_archive_save(fit = fit, registry_row = row,
                     registry = "models.csv", archive_dir = arch)

  loaded = model_archive_load(model_id = 1001L, registry = "models.csv",
                              archive_dir = arch, package = NULL)
  expect_s3_class(loaded[[1]]$fit, "randomForest")

  newdat = data.frame(age1 = c(10, 30), tpa1 = c(200, 600))
  pred = model_predict(dat = newdat, nms_dat_predict = c(age1 = "age1", tpa1 = "tpa1"),
                       params = loaded[[1]]$row, archive_dir = arch, package = NULL)
  expect_true("ba1" %in% names(pred))
  expect_false(any(is.na(pred$ba1)))

})

test_that("xgboost fit round-trips and predicts via the matrix adapter", {

  skip_if_not_installed("xgboost")
  arch = file.path(tempdir(), "rforinvt_archive_xgb")
  unlink(arch, recursive = TRUE)

  set.seed(2)
  X = cbind(age1 = runif(80, 5, 40), tpa1 = runif(80, 100, 800))
  y = 2 * X[, "age1"] + 0.05 * X[, "tpa1"] + rnorm(80)
  dtrain = xgboost::xgb.DMatrix(data = X, label = y)
  fit = xgboost::xgb.train(
    params = list(objective = "reg:squarederror"),
    data = dtrain, nrounds = 8
  )

  row = data.table::data.table(model_id = 1002L, model_class = "xgboost",
                               response = "ba1", spp = "x", region_code = "t")
  saved = model_archive_save(fit = fit, registry_row = row,
                             registry = "models.csv", archive_dir = arch)

  # native xgboost serialization + provenance
  expect_match(saved$fit_object_path, "\\.ubj$")
  expect_equal(saved$serializer, "xgb.ubj")
  expect_equal(saved$pkg, "xgboost")
  expect_true(file.exists(file.path(arch, "fits", "1002.ubj")))
  expect_false(file.exists(file.path(arch, "fits", "1002.rds")))

  loaded = model_archive_load(model_id = 1002L, registry = "models.csv",
                              archive_dir = arch, package = NULL)
  newdat = data.frame(age1 = c(10, 30), tpa1 = c(200, 600))
  pred = model_predict(dat = newdat, nms_dat_predict = c(age1 = "age1", tpa1 = "tpa1"),
                       params = loaded[[1]]$row, archive_dir = arch, package = NULL)
  expect_true("ba1" %in% names(pred))
  expect_false(any(is.na(pred$ba1)))

})

test_that("lighten=TRUE (butcher) preserves lme4 predictions within tolerance", {

  skip_if_not_installed("lme4")
  skip_if_not_installed("butcher")
  arch = file.path(tempdir(), "rforinvt_archive_lmer")
  unlink(arch, recursive = TRUE)

  set.seed(3)
  grp = factor(rep(1:8, each = 12))
  train = data.frame(
    age1 = runif(96, 5, 40),
    grp  = grp
  )
  train$ba1 = 2 * train$age1 + as.numeric(grp) + rnorm(96)
  fit = lme4::lmer(ba1 ~ age1 + (1 | grp), data = train)
  newdat = data.frame(age1 = c(10, 25, 35), grp = factor(c(1, 4, 8)))
  pred_ref = as.numeric(predict(fit, newdata = newdat, allow.new.levels = TRUE))

  row = data.table::data.table(model_id = 1003L, model_class = "lme4",
                               response = "ba1", spp = "x", region_code = "t")
  saved = model_archive_save(fit = fit, registry_row = row, registry = "models.csv",
                             archive_dir = arch, lighten = TRUE)
  expect_true(isTRUE(saved$lightened))

  loaded = model_archive_load(model_id = 1003L, registry = "models.csv",
                              archive_dir = arch, package = NULL)
  pred_bch = as.numeric(predict(loaded[[1]]$fit, newdata = newdat, allow.new.levels = TRUE))
  expect_equal(pred_bch, pred_ref, tolerance = 1e-8)

})

test_that("lighten only applies to the allowlist (randomForest stays un-lightened)", {

  skip_if_not_installed("randomForest")
  skip_if_not_installed("butcher")
  arch = file.path(tempdir(), "rforinvt_archive_rf_lit")
  unlink(arch, recursive = TRUE)

  set.seed(4)
  train = data.frame(age1 = runif(50, 5, 40), tpa1 = runif(50, 100, 800))
  train$ba1 = 2 * train$age1 + 0.05 * train$tpa1 + rnorm(50)
  fit = randomForest::randomForest(ba1 ~ age1 + tpa1, data = train, ntree = 30)

  row = data.table::data.table(model_id = 1004L, model_class = "randomForest",
                               response = "ba1", spp = "x", region_code = "t")
  saved = model_archive_save(fit = fit, registry_row = row, registry = "models.csv",
                             archive_dir = arch, lighten = TRUE)
  expect_false(isTRUE(saved$lightened))

})

test_that("non-closed-form path with empty fit_object_path yields NA, not a crash", {

  row_bad = data.table::data.table(
    response        = "ba1",
    model_class     = "xgboost",
    formula         = NA_character_,
    b0              = NA_real_,
    fit_object_path = NA_character_
  )
  dat_in = data.table::data.table(tpa1 = 400, hd1 = 35, age1 = 15)

  out_chk = suppressWarnings(model_predict(
    dat = dat_in,
    nms_dat_predict = c(tpa1 = "tpa1", hd1 = "hd1", age1 = "age1"),
    params = row_bad,
    archive_dir = fixture_arch,
    package = NULL
  ))
  expect_true(all(is.na(out_chk[["ba1"]])))

})
