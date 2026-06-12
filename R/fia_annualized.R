#'@name fia_annualized
#'
#'@title Annualized FIA estimation: design-based vs model-based (Phase 5)
#'
#'@description
#'  Tools for estimating a forest attribute for a single target year \eqn{Y} when
#'  only a fraction of plots are measured that year (FIA's annual panel design,
#'  ~1 in 10 plots in the West). They support four strategies that trade bias for
#'  precision, all consuming the compiled per-plot frames from
#'  \code{fia_compile_plots()} and the post-stratified estimator
#'  \code{fia_estimate()}:
#'
#'  \enumerate{
#'    \item \strong{Design-based, single panel} - \code{fia_annual_panel()} keeps
#'      only the plots measured in \eqn{Y}; estimate with \code{fia_estimate()}.
#'      Unbiased, but noisy (~1/10 of the plots).
#'    \item \strong{Model-assisted (GREG)} - \code{fia_growth_model()} fits a
#'      growth/projection model on remeasured plots; \code{fia_annualize_resid()}
#'      puts residuals on the target-year horizon; \code{fia_estimate_greg()}
#'      combines all-plot predictions with the design-weighted residual
#'      correction. Lower variance, approximately design-consistent.
#'    \item \strong{Pure model-based} - \code{fia_estimate_model()} reports the
#'      model predictions for every plot at \eqn{Y} with no correction;
#'      \code{fia_growth_cv()} characterizes bias/RMSE by cross-validation.
#'      Lowest variance, exposed to model bias.
#'    \item \strong{FVS projections} - grow each plot's previous measurement to
#'      \eqn{Y} with the FVS bridge (\code{fia_fvs_*}), then compile and estimate
#'      through the same pipeline. Process-based; carries FVS calibration bias.
#'  }
#'
#'@details
#'  \strong{Residual annualization.} A plot remeasured over \eqn{T} years gives a
#'  periodic residual \eqn{e_T = y_{obs}(t_2) - \hat y(t_2)}. It is annualized and
#'  rescaled to the target offset \eqn{k = Y - t_1}:
#'  \deqn{e_{annual} = e_T / T, \qquad e_k = k \, e_{annual}.}
#'  e.g. measurements in 2010 and 2020 (\eqn{T=10}) and target 2017 (\eqn{k=7}):
#'  \eqn{e_{annual} = e_{10}/10}, \eqn{e_{7y} = 7 e_{annual}}. This pools residual
#'  information across plots with different remeasurement intervals onto one
#'  annual scale.
#'
#'  \strong{GREG.} For a domain total, \code{fia_estimate_greg()} uses the
#'  generalized-regression / difference form
#'  \deqn{\hat\tau_{GREG} = \hat T_{design}(\hat y; \text{all plots}) +
#'        \hat T_{design}(e_k; \text{measured panel}),}
#'  with the design variance of the (annualized) residual term as the
#'  variance approximation. When the measured panel equals the full plot set and
#'  \eqn{e = y - \hat y}, this collapses identically to the plain design estimate
#'  of \eqn{y} (a useful correctness check, exercised in the tests).
#'
#'  This program is free software but it is provided WITHOUT WARRANTY and with
#'  ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose; you can
#'  redistribute it and/or modify it under the terms of the GNU General Public
#'  License as published by the Free Software Foundation; either version 2 of the
#'  License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 6/12/2026 Created (Phase 5 annualized estimation) \cr
#'}
#'
#'@author Jacob Strunk <someone@@somewhere.com>
#'
#'@param df_plot a compiled per-plot frame (from \code{fia_compile_plots()}).
#'@param year target year \eqn{Y}.
#'@param year_col plot measurement-year column (default \code{"MEASYEAR"}).
#'@param df a paired/remeasured per-plot frame: current attributes plus the
#'  previous-measurement predictor columns and the year columns.
#'@param response current-measurement response column to model/estimate.
#'@param predictors character vector of predictor columns (previous-measurement
#'  attributes, site covariates, and - for projection - the remeasurement
#'  interval column).
#'@param increment if \code{TRUE}, model the change \code{response - prev_value_col}
#'  rather than the level.
#'@param prev_value_col previous-measurement value of \code{response} (required
#'  when \code{increment = TRUE} and to project a level forward).
#'@param engine model engine: \code{"ols"} (\code{stats::lm}) or \code{"rf"}
#'  (\code{randomForest}).
#'@param model a \code{fia_growth_model} object from \code{fia_growth_model()}.
#'@param prev_year_col,curr_year_col previous and current measurement-year
#'  columns used to form the interval \eqn{T} and offset \eqn{k}.
#'@param target_year target year \eqn{Y} for residual annualization / projection.
#'@param k number of cross-validation folds (\code{fia_growth_cv}).
#'@param seed optional RNG seed for reproducible CV folds.
#'@param df_pop population plot frame carrying the prediction column
#'  \code{pred_col} and the design columns for \code{fia_estimate()}.
#'@param pred_col per-plot model prediction column at \eqn{Y}.
#'@param df_sample measured-panel plot frame carrying the residual column.
#'@param resid_col per-plot (annualized) residual column on the measured panel.
#'@param by,type,... domain columns, estimate type, and arguments forwarded to
#'  \code{fia_estimate()}.
#'
#'@return
#'  \code{fia_annual_panel()} a subset data.frame; \code{fia_growth_model()} a
#'  \code{fia_growth_model} object; \code{fia_annualize_resid()} the input with
#'  prediction/residual columns appended; \code{fia_growth_cv()} a one-row
#'  diagnostics data.frame (bias, rmse, r2); \code{fia_estimate_model()} and
#'  \code{fia_estimate_greg()} \code{fia_estimate()}-shaped long frames tagged by
#'  \code{type}.
#'
#'@examples
#'\donttest{
#'  db_path <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
#'  if (nzchar(db_path)) {
#'    db <- fia_db(db_path)
#'    #current (2021) and previous (2019) compiled plot frames
#'    cur  <- fia_compile_plots(fia_compile_trees(fia_trees(db, evalid = 532101)),
#'                              fia_plots(db, evalid = 532101))
#'    prev <- fia_compile_plots(fia_compile_trees(fia_trees(db, evalid = 531901)),
#'                              fia_plots(db, evalid = 531901))
#'
#'    #pair current to previous by plot identity, build interval + predictors
#'    pr <- prev[, c("PLT_CN", "vol_cf_net", "ba_ft", "stems")]
#'    names(pr) <- c("PREV_PLT_CN", "vol_t1", "ba_t1", "stems_t1")
#'    pairs <- merge(cur, pr, by = "PREV_PLT_CN")
#'    pairs$interval <- pairs$MEASYEAR - 2019
#'
#'    m <- fia_growth_model(pairs, response = "vol_cf_net",
#'                          predictors = c("vol_t1", "ba_t1", "stems_t1", "interval"))
#'    pairs <- fia_annualize_resid(m, pairs, response = "vol_cf_net",
#'                                 prev_year_col = "interval", curr_year_col = "interval",
#'                                 target_year = 1)  # 1 year past t1 -> annual
#'    head(pairs[, c("PLT_CN", ".pred", ".resid_periodic", ".resid_target")])
#'  }
#'}
#'
#'@import plyr
#'
#'@seealso \code{\link{fia_estimate}}\cr \code{\link{fia_compile_plots}}\cr
NULL

# ---------------------------------------------------------------------------
# approach 1: single-year panel
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_annual_panel = function(df_plot, year, year_col = "MEASYEAR"){
  df = as.data.frame(df_plot)
  if(!year_col %in% names(df)) stop("year_col '", year_col, "' not found in df_plot")
  out = df[df[[year_col]] %in% year, , drop = FALSE]
  if(nrow(out) == 0) warning("no plots measured in year(s): ", paste(year, collapse = ", "))
  rownames(out) = NULL
  out
}

# ---------------------------------------------------------------------------
# growth/projection model
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_growth_model = function(df, response, predictors,
                            increment = FALSE, prev_value_col = NULL,
                            engine = c("ols", "rf")){
  engine = match.arg(engine)
  df = as.data.frame(df)

  need = c(response, predictors, if(increment) prev_value_col)
  miss = need[!need %in% names(df)]
  if(length(miss)) stop("df is missing column(s): ", paste(miss, collapse = ", "))
  if(increment && is.null(prev_value_col))
    stop("increment = TRUE requires 'prev_value_col'")

  #build the modeling response (level or increment)
  y = if(increment) df[[response]] - df[[prev_value_col]] else df[[response]]
  dat = data.frame(.y = y, df[, predictors, drop = FALSE])
  ok  = stats::complete.cases(dat)
  dat = dat[ok, , drop = FALSE]
  form = stats::as.formula(paste(".y ~", paste(predictors, collapse = " + ")))

  if(engine == "ols"){
    fit = stats::lm(form, data = dat)
  } else {
    if(!requireNamespace("randomForest", quietly = TRUE))
      stop("engine = 'rf' needs the randomForest package")
    fit = randomForest::randomForest(form, data = dat)
  }

  structure(list(model = fit, engine = engine, response = response,
                 predictors = predictors, increment = increment,
                 prev_value_col = prev_value_col, n = nrow(dat)),
            class = "fia_growth_model")
}

#'@export
print.fia_growth_model = function(x, ...){
  cat("<fia_growth_model:", x$engine,
      if(x$increment) "(increment)" else "(level)",
      "| n =", x$n, "| response:", x$response, ">\n")
  invisible(x)
}

# predict the modeled quantity (level or increment) for new rows
.fia_gm_predict = function(model, newdata){
  as.numeric(stats::predict(model$model, newdata = as.data.frame(newdata)))
}

# project a level prediction of `response` for new rows. For increment models the
# predicted increment is added back onto prev_value_col.
.fia_gm_project = function(model, newdata){
  p = .fia_gm_predict(model, newdata)
  if(model$increment) p = p + as.data.frame(newdata)[[model$prev_value_col]]
  p
}

# ---------------------------------------------------------------------------
# residual annualization
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_annualize_resid = function(model, df, response,
                               prev_year_col, curr_year_col, target_year){
  df = as.data.frame(df)
  need = c(response, prev_year_col, curr_year_col)
  miss = need[!need %in% names(df)]
  if(length(miss)) stop("df is missing column(s): ", paste(miss, collapse = ", "))

  pred = .fia_gm_project(model, df)            # predicted level of response at t2
  obs  = df[[response]]
  Tint = df[[curr_year_col]] - df[[prev_year_col]]
  if(any(Tint <= 0, na.rm = TRUE))
    warning("non-positive remeasurement interval(s); annualized residuals will be Inf/NaN")

  e_T      = obs - pred
  e_annual = e_T / Tint
  k        = target_year - df[[prev_year_col]]
  e_target = k * e_annual

  df[[".pred"]]           = pred
  df[[".resid_periodic"]] = e_T
  df[[".resid_annual"]]   = e_annual
  df[[".resid_target"]]   = e_target
  df
}

# ---------------------------------------------------------------------------
# cross-validated model diagnostics (approach 3 bias/precision)
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_growth_cv = function(df, response, predictors,
                         increment = FALSE, prev_value_col = NULL,
                         engine = c("ols", "rf"), k = 10, seed = NULL){
  engine = match.arg(engine)
  df = as.data.frame(df)
  if(!is.null(seed)) set.seed(seed)

  y_all = if(increment) df[[response]] - df[[prev_value_col]] else df[[response]]
  keep  = stats::complete.cases(df[, c(response, predictors,
                                       if(increment) prev_value_col), drop = FALSE])
  df = df[keep, , drop = FALSE]; y_all = y_all[keep]
  n = nrow(df)
  if(n < k) k = max(2, n)                       # not enough rows for k folds
  fold = sample(rep(seq_len(k), length.out = n))

  pred = rep(NA_real_, n)
  for(f in seq_len(k)){
    tr = df[fold != f, , drop = FALSE]
    te = df[fold == f, , drop = FALSE]
    m  = fia_growth_model(tr, response, predictors, increment, prev_value_col, engine)
    pred[fold == f] = .fia_gm_predict(m, te)    # predict the modeled quantity
  }
  resid = y_all - pred
  data.frame(
    engine = engine, n = n, folds = k,
    bias = mean(resid, na.rm = TRUE),
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    r2   = 1 - sum(resid^2, na.rm = TRUE) / sum((y_all - mean(y_all))^2, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# approach 3: pure model-based estimate (predictions for all plots)
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_estimate_model = function(df_pop, pred_col, by = "COUNTYCD",
                              type = c("per_acre", "total", "area"), ...){
  type = match.arg(type)
  if(!pred_col %in% names(df_pop)) stop("pred_col '", pred_col, "' not found in df_pop")
  est = fia_estimate(df_pop, vars = pred_col, by = by, type = type, ...)
  est$variable = pred_col
  est$type = paste0("model_", type)
  est
}

# ---------------------------------------------------------------------------
# approach 2: GREG / difference estimator with annualized residual correction
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_annualized
fia_estimate_greg = function(df_pop, pred_col, df_sample, resid_col,
                             by = "COUNTYCD",
                             type = c("per_acre", "total", "area"), ...){
  type = match.arg(type)
  if(!pred_col %in% names(df_pop)) stop("pred_col '", pred_col, "' not found in df_pop")
  if(!resid_col %in% names(df_sample)) stop("resid_col '", resid_col, "' not found in df_sample")

  pred_est  = fia_estimate(df_pop,    vars = pred_col,  by = by, type = type, ...)
  resid_est = fia_estimate(df_sample, vars = resid_col, by = by, type = type, ...)

  #align on the domain columns and combine (GREG = T(pred) + T(resid))
  m = merge(pred_est[, c(by, "estimate", "n_plots")],
            resid_est[, c(by, "estimate", "variance", "se", "n_plots")],
            by = by, suffixes = c("_pred", "_resid"), all = TRUE)
  m$estimate_pred  [is.na(m$estimate_pred)]  = 0
  m$estimate_resid [is.na(m$estimate_resid)] = 0
  m$variance       [is.na(m$variance)]       = 0

  out = data.frame(
    m[, by, drop = FALSE],
    variable  = pred_col,
    type      = paste0("greg_", type),
    estimate  = m$estimate_pred + m$estimate_resid,
    variance  = m$variance,                         # GREG var ~ design var of residuals
    se        = sqrt(pmax(0, m$variance)),
    n_pop     = m$n_plots_pred,
    n_sample  = m$n_plots_resid,
    stringsAsFactors = FALSE
  )
  out$se_pct = ifelse(is.finite(out$estimate) & out$estimate != 0,
                      100 * out$se / abs(out$estimate), NA_real_)
  rownames(out) = NULL
  out
}
