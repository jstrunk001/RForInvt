#'@name fia_estimate
#'
#'@title Post-stratified FIA estimation (Phase 3 estimation layer)
#'
#'@description
#'  Design-based, post-stratified estimators for compiled FIA plot data,
#'  following Bechtold & Patterson (2005). Given the per-plot frame from
#'  \code{fia_compile_plots()} (per-acre attributes plus the stratum / estimation
#'  unit / area design columns), these produce totals, per-acre means, and area
#'  with their sampling variances, by domain (e.g. county).
#'
#'  \itemize{
#'    \item \code{fia_estimate()} - totals, per-acre ratios, or area for one or
#'          more compiled attributes, by domain.
#'    \item \code{fia_components_long()} - tidy a wide species / diameter-class
#'          component estimate back to long form.
#'    \item \code{fia_estimate_annual()} - run \code{fia_estimate()} across
#'          several annual evaluations and stack the results into a trend frame.
#'    \item \code{fia_estimate_change()} - paired-plot (\code{"remeas"}) or
#'          growth-removal-mortality (\code{"grm"}) change estimation.
#'  }
#'
#'@details
#'  Within an estimation unit with post-strata \eqn{h}, stratum weights
#'  \eqn{W_h = A_h / A} (area share) and per-acre plot values \eqn{z_{hi}}, the
#'  post-stratified mean and its variance are (Bechtold & Patterson 2005, eq.
#'  4.3):
#'  \deqn{\bar z_{pst} = \sum_h W_h \bar z_h}
#'  \deqn{v(\bar z_{pst}) = \frac{1}{n}\sum_h W_h s^2_{zh} +
#'        \frac{1}{n^2}\sum_h (1 - W_h) s^2_{zh}}
#'  with \eqn{n} the estimation-unit plot count. The total is
#'  \eqn{\hat\tau = A\,\bar z_{pst}} (identically \eqn{\sum_h EXPNS_h \sum_i
#'  z_{hi}}), and \eqn{v(\hat\tau) = A^2 v(\bar z_{pst})}. Estimation units are
#'  independent, so totals and variances add across them.
#'
#'  \emph{Per-acre} estimates are ratios of two totals (attribute over forest
#'  area) using the combined-ratio variance with the post-stratified covariance.
#'  \emph{Domains} (the \code{by} grouping) are handled by zeroing the response
#'  outside the domain within each estimation unit, so stratum plot counts and
#'  weights stay population-based.
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
#'1.0 \tab 6/12/2026 Created (Phase 3 FIA estimation layer) \cr
#'}
#'
#'@author Jacob Strunk <someone@@somewhere.com>
#'
#'@param df a compiled per-plot frame from \code{fia_compile_plots()} (one row
#'  per plot) carrying the metric columns to estimate plus the design columns
#'  named by \code{strata_col} / \code{eu_col} / \code{area_used_col} and the
#'  forest-area column \code{area_var}.
#'@param vars metric column(s) to estimate. Exact names, or regular-expression
#'  patterns matched against \code{names(df)} (e.g. \code{"^vol_cf_net_SPCD_"} to
#'  grab every species component). Ignored when \code{type = "area"}.
#'@param by character vector of domain (grouping) columns; default
#'  \code{"COUNTYCD"}.
#'@param type \code{"per_acre"} (attribute per forest acre, a ratio),
#'  \code{"total"} (EXPNS-weighted total), or \code{"area"} (forest area).
#'@param area_var per-plot forested-proportion column used as the area / ratio
#'  denominator; default \code{"CONDPROP_FOR"} (from \code{fia_compile_plots()}).
#'@param strata_col,eu_col,area_used_col design columns: post-stratum id,
#'  estimation-unit id, and the stratum area (acres) used for the stratum weight.
#'@param var_method variance estimator; currently \code{"BP2005"}.
#'@param db,statecd,years,eval_type,vol_source arguments for
#'  \code{fia_estimate_annual()}: the database, state, target years, evaluation
#'  type, and volume source threaded into the compile step.
#'@param evalid_t1,evalid_t2 prior and current EVALIDs for
#'  \code{fia_estimate_change()}.
#'@param mode change mode: \code{"remeas"} (paired remeasured plots) or
#'  \code{"grm"} (growth-removal-mortality component tables).
#'@param df_est a long estimate frame (from \code{fia_estimate()}) for
#'  \code{fia_components_long()}.
#'@param ... forwarded to the compile/estimate steps.
#'
#'@return
#'  A long data.frame with the \code{by} columns plus \code{variable},
#'  \code{type}, \code{estimate}, \code{variance}, \code{se}, \code{se_pct},
#'  \code{n_plots}, and \code{n_nonzero}. \code{fia_estimate_annual()} adds
#'  \code{EVALID}/\code{year}; \code{fia_components_long()} adds parsed
#'  \code{SPCD}/\code{dbcl} columns.
#'
#'@examples
#'\donttest{
#'  db_path <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
#'  if (nzchar(db_path)) {
#'    db <- fia_db(db_path)
#'    ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
#'    trc <- fia_compile_trees(fia_trees(db, evalid = ev), vol_source = "fiadb")
#'    plc <- fia_compile_plots(trc, fia_plots(db, evalid = ev))
#'
#'    #net cubic volume per forest acre, and total, by county
#'    fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "per_acre")
#'    fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "total")
#'    fia_estimate(plc, by = "COUNTYCD", type = "area")
#'  }
#'}
#'
#'@import plyr
#'
#'@seealso \code{\link{fia_compile_plots}}\cr \code{\link{fia_trees}}\cr
NULL

# resolve `vars` (exact names or regex patterns) to actual columns of df
.fia_resolve_vars = function(df, vars){
  if(is.null(vars)) return(character(0))
  out = character(0)
  for(v in vars){
    if(v %in% names(df)) out = c(out, v)
    else out = c(out, grep(v, names(df), value = TRUE))
  }
  unique(out)
}

# post-stratified totals/variances for ONE estimation unit.
# z = per-acre response (already zeroed outside domain), x = per-acre forest
# area indicator (ditto), strata = stratum id per plot, area_h = named vector of
# stratum area (acres) keyed by stratum id. Returns the unit's total and
# variance for z and x, and their covariance (for ratios).
.fia_ps_eu = function(z, x, strata, area_h){
  strata = as.character(strata)
  n = length(z)
  us = unique(strata)
  A_h = area_h[us]
  A_eu = sum(A_h, na.rm = TRUE)
  W = A_h / A_eu

  zbar = xbar = s2z = s2x = szx = setNames(numeric(length(us)), us)
  for(h in us){
    idx = strata == h
    nh  = sum(idx)
    zh  = z[idx]; xh = x[idx]
    zbar[h] = mean(zh); xbar[h] = mean(xh)
    if(nh > 1){
      s2z[h] = stats::var(zh)
      s2x[h] = stats::var(xh)
      szx[h] = stats::cov(zh, xh)
    } else {
      s2z[h] = 0; s2x[h] = 0; szx[h] = 0   # single-plot stratum: no within var
    }
  }

  zbar_pst = sum(W * zbar); xbar_pst = sum(W * xbar)
  vz  = sum(W * s2z) / n + sum((1 - W) * s2z) / n^2
  vx  = sum(W * s2x) / n + sum((1 - W) * s2x) / n^2
  czx = sum(W * szx) / n + sum((1 - W) * szx) / n^2

  list(
    T_z = A_eu * zbar_pst, V_z = A_eu^2 * vz,
    T_x = A_eu * xbar_pst, V_x = A_eu^2 * vx,
    C   = A_eu^2 * czx,
    n   = n
  )
}

# estimate one (domain, variable) cell across the estimation units it touches
.fia_estimate_cell = function(df, var, area_var, type, by_cols, dom_vals,
                              strata_col, eu_col, area_used_col){
  #domain membership
  in_dom = rep(TRUE, nrow(df))
  for(b in by_cols) in_dom = in_dom & (df[[b]] == dom_vals[[b]])

  eus = unique(df[[eu_col]][in_dom])
  acc = list(T_z = 0, V_z = 0, T_x = 0, V_x = 0, C = 0)
  for(eu in eus){
    sub = df[df[[eu_col]] == eu, , drop = FALSE]
    dom = rep(TRUE, nrow(sub))
    for(b in by_cols) dom = dom & (sub[[b]] == dom_vals[[b]])
    z = if(is.na(var)) rep(0, nrow(sub)) else ifelse(dom, sub[[var]], 0)
    x = ifelse(dom, sub[[area_var]], 0)
    area_h = tapply(sub[[area_used_col]], as.character(sub[[strata_col]]),
                    function(v) v[1])
    ps = .fia_ps_eu(z, x, sub[[strata_col]], area_h)
    acc$T_z = acc$T_z + ps$T_z; acc$V_z = acc$V_z + ps$V_z
    acc$T_x = acc$T_x + ps$T_x; acc$V_x = acc$V_x + ps$V_x
    acc$C   = acc$C   + ps$C
  }

  if(type == "total"){ est = acc$T_z; v = acc$V_z }
  if(type == "area"){  est = acc$T_x; v = acc$V_x }
  if(type == "per_acre"){
    est = if(acc$T_x != 0) acc$T_z / acc$T_x else NA_real_
    v   = if(acc$T_x != 0) (acc$V_z + est^2 * acc$V_x - 2 * est * acc$C) / acc$T_x^2 else NA_real_
  }

  dom_rows = df[in_dom, , drop = FALSE]
  zvals = if(is.na(var)) dom_rows[[area_var]] else dom_rows[[var]]
  data.frame(
    variable  = if(is.na(var)) area_var else var,
    type      = type,
    estimate  = est,
    variance  = v,
    se        = sqrt(pmax(0, v)),
    se_pct    = ifelse(is.finite(est) & est != 0, 100 * sqrt(pmax(0, v)) / abs(est), NA_real_),
    n_plots   = nrow(dom_rows),
    n_nonzero = sum(zvals > 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

#'@export
#'@rdname fia_estimate
fia_estimate = function(df, vars = NULL, by = "COUNTYCD",
                        type = c("per_acre", "total", "area"),
                        area_var = "CONDPROP_FOR",
                        strata_col = "STRATUM_CN", eu_col = "ESTN_UNIT",
                        area_used_col = "AREA_USED",
                        var_method = "BP2005", ...){
  type = match.arg(type)
  df = as.data.frame(df)

  #checks
  need = c(strata_col, eu_col, area_used_col, by)
  miss = need[!need %in% names(df)]
  if(length(miss)) stop("df is missing required column(s): ", paste(miss, collapse = ", "))
  if(type %in% c("area", "per_acre") && !area_var %in% names(df))
    stop("type = '", type, "' needs the area column '", area_var, "' in df")
  if(type %in% c("total", "per_acre")){
    vcols = .fia_resolve_vars(df, vars)
    if(length(vcols) == 0) stop("no metric columns matched 'vars'")
  } else {
    vcols = NA   # area: single pass, no response var
  }

  #domains
  doms = unique(df[, by, drop = FALSE])
  doms = doms[do.call(order, doms[, by, drop = FALSE]), , drop = FALSE]

  out = list()
  for(i in seq_len(nrow(doms))){
    dom_vals = as.list(doms[i, by, drop = FALSE])
    for(var in vcols){
      cell = .fia_estimate_cell(df, var, area_var, type, by, dom_vals,
                                strata_col, eu_col, area_used_col)
      out[[length(out) + 1]] = data.frame(doms[i, by, drop = FALSE], cell,
                                          row.names = NULL, stringsAsFactors = FALSE)
    }
  }
  res = do.call(rbind, out)
  rownames(res) = NULL
  res
}

#'@export
#'@rdname fia_estimate
fia_components_long = function(df_est, ...){
  df = as.data.frame(df_est)
  v  = df$variable
  #parse "<base>_SPCD_<spcd>" and/or "_dbcl_<dbcl>" suffixes
  df$SPCD = suppressWarnings(as.integer(sub(".*_SPCD_([0-9]+).*", "\\1",
                                            ifelse(grepl("_SPCD_", v), v, NA))))
  df$dbcl = suppressWarnings(as.numeric(sub(".*_dbcl_([0-9.]+).*", "\\1",
                                            ifelse(grepl("_dbcl_", v), v, NA))))
  df$attribute = sub("_SPCD_[0-9]+", "", v)
  df$attribute = sub("_dbcl_[0-9.]+", "", df$attribute)
  df
}

#'@export
#'@rdname fia_estimate
fia_estimate_annual = function(db, statecd, years, vars, by = "COUNTYCD",
                               type = c("per_acre", "total", "area"),
                               eval_type = "EXPVOL", vol_source = "fiadb", ...){
  type = match.arg(type)
  out = list()
  for(yr in years){
    ev = fia_evalid(db, statecd = statecd, years = yr, eval_type = eval_type)
    if(nrow(ev) == 0){ warning("no ", eval_type, " evaluation for year ", yr); next }
    trc = fia_compile_trees(fia_trees(db, evalid = ev), vol_source = vol_source)
    plc = suppressWarnings(fia_compile_plots(trc, fia_plots(db, evalid = ev)))
    est = fia_estimate(plc, vars = vars, by = by, type = type, ...)
    est = data.frame(EVALID = ev$EVALID[1], year = yr, est,
                     row.names = NULL, stringsAsFactors = FALSE)
    out[[length(out) + 1]] = est
  }
  res = do.call(rbind, out)
  rownames(res) = NULL
  res
}

#'@export
#'@rdname fia_estimate
fia_estimate_change = function(db, statecd, evalid_t1, evalid_t2,
                               mode = c("remeas", "grm"),
                               vars, by = "COUNTYCD",
                               type = c("per_acre", "total"),
                               vol_source = "fiadb", ...){
  mode = match.arg(mode)
  type = match.arg(type)

  if(mode == "grm")
    stop("mode = 'grm' needs TREE_GRM_COMPONENT/TREE_GRM_MIDPT tables and is ",
         "not yet implemented; use mode = 'remeas' for paired-plot change")

  #compile both evaluations to per-plot frames
  t1 = suppressWarnings(fia_compile_plots(
    fia_compile_trees(fia_trees(db, evalid = evalid_t1), vol_source = vol_source),
    fia_plots(db, evalid = evalid_t1)))
  t2 = suppressWarnings(fia_compile_plots(
    fia_compile_trees(fia_trees(db, evalid = evalid_t2), vol_source = vol_source),
    fia_plots(db, evalid = evalid_t2)))

  vcols = .fia_resolve_vars(t2, vars)
  if(length(vcols) == 0) stop("no metric columns matched 'vars'")
  if(!"PREV_PLT_CN" %in% names(t2))
    stop("current frame lacks PREV_PLT_CN; cannot pair plots for change")

  #pair current plots to their previous measurement
  prev = t1[, c("PLT_CN", vcols), drop = FALSE]
  names(prev) = c("PREV_PLT_CN", paste0(vcols, "_t1"))
  paired = merge(t2, prev, by = "PREV_PLT_CN")
  if(nrow(paired) == 0) stop("no plots could be paired between the two evaluations")

  #difference each attribute; estimate the difference with t2's design
  for(v in vcols) paired[[paste0(v, "_chg")]] = paired[[v]] - paired[[paste0(v, "_t1")]]

  est = fia_estimate(paired, vars = paste0(vcols, "_chg"), by = by, type = type, ...)
  est$variable = sub("_chg$", "", est$variable)
  est
}
