library(testthat)

# Value-pinning regression tests for the 2026-06 package-review fixes.
# These lock in behavior that the previous (structural-only) suite did not
# catch. Fixtures from helper-rforinvt.R (make_model_df) are reused where useful.

# --- sample_systematic: the final frame unit N is sampleable (idx <= N) ------
test_that("sample_systematic can include the final unit N", {
  set.seed(1)
  N <- 20L; n <- 5L
  # unit N has ~n/N inclusion prob; across many draws it must appear at least once
  expect_true(any(replicate(300, N %in% sample_systematic(N = N, n = n))))
  s <- sample_systematic(N = N, n = n)
  expect_length(s, n)
  expect_true(all(s >= 1 & s <= N))
})

# --- yai_r2: residual-variance R2 (not 1 - var(pred)/var(obs)) ----------------
test_that("yai_r2 equals 1 - var(obs - pred)/var(obs)", {
  skip_if_not_installed("yaImpute")
  d <- make_knn_ref()
  rownames(d) <- d$id
  yai <- yaImpute::yai(x = d[, c("x1", "x2")], y = d[, c("y1", "y2")],
                       method = "euclidean", k = 3)
  imp <- yaImpute::impute(yai, observed = TRUE)
  r <- yai_r2(imp)
  # manual residual-variance R2 for y1 (the OLD formula 1 - var(pred)/var(obs)
  # would differ from this)
  man <- 1 - stats::var(imp[["y1.o"]] - imp[["y1"]]) / stats::var(imp[["y1.o"]])
  expect_equal(r["y1", "R2_in"], man, tolerance = 1e-8)
})

# --- reg_model: rank_by = "rss" ranks lowest-RSS model first ------------------
test_that("reg_model rank_by='rss' selects the minimum-RSS model at rank 1", {
  skip_if_not_installed("leaps")
  df <- make_model_df()
  rg <- leaps::regsubsets(y ~ x1 + x2 + x3, df, nvmax = 3)
  rss <- summary(rg)$rss
  m1  <- reg_model(df[, c("y", "x1", "x2", "x3")], rg, rank = 1, rank_by = "rss")
  # the rank-1 model must be the one with the smallest RSS (most predictors here)
  n_pred_rank1 <- length(attr(stats::terms(m1), "term.labels"))
  expect_equal(n_pred_rank1, which.min(rss) + 0)  # nvmax index == #predictors
  expect_true(inherits(m1, "lm"))
})

# --- multi_bs: numeric statistic columns (not character) ----------------------
test_that("multi_bs returns numeric err_632 (not coerced to character)", {
  skip_if_not_installed("bootstrap")
  set.seed(1)
  mbs <- multi_bs(list(y = lm(y ~ x1 + x2, make_model_df())),
                  n_boot = 20, n_clus = 1, lm_boot = lm_boot)
  expect_type(mbs$err_632, "double")
  expect_true(is.character(mbs$predictors))
})

# --- lm_summary(resids=TRUE): returns res + residuals, no dfIn crash ----------
test_that("lm_summary(resids=TRUE) returns the summary and per-obs residuals", {
  df <- make_model_df()
  out <- lm_summary(lm(y ~ x1 + x2, df), df, resids = TRUE)
  expect_type(out, "list")
  expect_true(is.data.frame(out$res))
  expect_true("resids" %in% names(out))
  expect_true(is.data.frame(out$resids))
})

# --- pred_multi(fix_outliers=TRUE): no se.pd/se_pd crash ----------------------
test_that("pred_multi clamps predictions with fix_outliers and se_fit", {
  mods <- lm_multi("y", make_model_df(), "y~.", verbose = FALSE)
  nd <- make_model_df(20, seed = 2); nd$id <- seq_len(nrow(nd))
  p <- pred_multi(mods, nd, dat0 = make_model_df(), id_col = "id",
                  n_clus = 1, se_fit = TRUE, fix_outliers = TRUE)
  expect_true(is.data.frame(p))
  expect_true("id" %in% names(p))
  expect_equal(nrow(p), nrow(nd))
})

# --- NVEL_merch: output spec columns carry SPEC values, not log values --------
# (pure-R post-processing; no DLL needed)
test_that("NVEL_merch returns the spec MinDib/MinLen, not the log's dib/len", {
  skip_if_not_installed("data.table")
  df_logs <- data.table::data.table(
    Species = 202, dib_sm = 20, log_len = 16, dbh = 22
  )
  df_specs <- data.table::data.table(
    Species = 202,
    Sort    = c("S", "P"),
    Grade   = c("4", "0"),
    Label   = c("Saw", "Pulp"),
    Rank    = c(1, 2),
    MinDib  = c(18, 6),
    MinDbh  = c(7, 7),
    MinLen  = c(8, 1)
  )
  res <- NVEL_merch(df_logs, df_specs)
  # best (lowest Rank) qualifying grade is Rank 1 (Saw): MinDib 18, MinLen 8 -
  # the PRE-FIX bug returned the log's dib_sm (20) / log_len (16) here.
  expect_equal(res$MinDib[1], 18)
  expect_equal(res$MinLen[1], 8)
  expect_equal(res$Label[1], "Saw")
})

# --- make_strata: a value just above the training max still gets a stratum ----
test_that("make_strata (non-nested numeric x2) assigns values just above the max", {
  skip_if_not_installed("plyr")
  set.seed(7)
  df <- data.frame(a = rnorm(200) * 10 + 50, b = rnorm(200) * 5 + 20)
  s <- make_strata(df, x1 = "a", x2 = "b", split_x1 = "qt", split_x2 = "qt",
                   nest_x2 = FALSE, n1 = 3, n2 = 3, collapse = FALSE)
  # a record with x2 slightly above the training max must land in the top
  # stratum (upper sentinel = max + range), not become NA
  nd <- data.frame(a = mean(df$a), b = max(df$b) + 1e-6)
  asn <- assign_strata(s, nd)
  expect_false(is.na(asn$stratum[1]))
})

# --- fia_clean_best_cds: the filter step actually runs ------------------------
test_that("fia_clean_best_cds applies the documented filters", {
  skip_if_not_installed("sf")
  base <- data.frame(
    INV_PLOT_ID = 1:2, STATECD = c(41, 6), REGION = 6, COUNTYCD = 1,
    PLOT = 1:2, PLOT_PUBLIC = 1:2, NFS_PLOT_NUMBER = 1:2,
    COORD_TYPE_NAME = "SP1", PC_COORD_METHOD_NAME = "HPGPS", PC_SRID = 6318,
    PC_LON_X = c(-120.1, -121.1), PC_LAT_Y = c(45.1, 46.1),
    SP2_LON_X = c(-120.1, -121.1), SP2_LAT_Y = c(45.1, 46.1),
    SP3_LON_X = c(-120.1, -121.1), SP3_LAT_Y = c(45.1, 46.1),
    SP4_LON_X = c(-120.1, -121.1), SP4_LAT_Y = c(45.1, 46.1),
    stringsAsFactors = FALSE
  )
  # default filter keeps STATECD == 41 only -> 1 plot, 4 subplot rows
  res <- suppressWarnings(fia_clean_best_cds(base, geom_type = "SUBPLOT POINT"))
  sub <- res[["SUBPLOT data.frame"]]
  expect_true(all(sub$STATECD == 41))
  expect_equal(nrow(sub), 4L)
})

# --- model_predict: constant formula recycles; sandbox blocks stray calls -----
test_that("model_predict closed-form handles a constant formula", {
  dat <- data.frame(x = 1:3)
  params <- data.frame(model_class = "closed_form", response = "pred",
                       formula = "b0", b0 = 5, stringsAsFactors = FALSE)
  out <- model_predict(dat = dat, params = params, nms_dat_predict = c(x = "x"))
  expect_equal(out$pred, c(5, 5, 5))
})

test_that("model_predict sandbox prevents non-math calls in the formula", {
  dat <- data.frame(x = 1:3)
  params <- data.frame(model_class = "closed_form", response = "pred",
                       formula = "system('echo hi')", b0 = 0, stringsAsFactors = FALSE)
  out <- model_predict(dat = dat, params = params, nms_dat_predict = c(x = "x"))
  # the disallowed call cannot resolve in the restricted env -> caught -> NA
  expect_true(all(is.na(out$pred)))
})
