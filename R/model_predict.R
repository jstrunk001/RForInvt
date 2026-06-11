# Revision history:
# 2025-09-19 — JLS — initial closed-form per-row prediction via .fn_predict()
# 2026-05-22 — JLS — dispatch on params$model_class; non-closed-form classes
#                    load fit_object_path RDS and call predict() instead of
#                    evaluating formula + b*. Closed-form path unchanged.
# 2026-06-08 — JLS — Pass-1 refactor: per-class predict adapters (nlme/lme4/
#                    randomForest/xgboost/systemfit/lavaan), optional
#                    predictor-spec transforms on the fit path, archive-root
#                    resolution, b0..b8 default, namespaced data.table calls.

#' @title Apply Archived Models to Data
#'
#' @description
#' Apply one or more models from a model archive to a `data.table`, appending
#' one prediction column per model. Dispatches per row on `params$model_class`.
#'
#' @details
#' Dispatch:
#' \itemize{
#'   \item `"closed_form"` (or blank, for legacy rows) — substitutes `b0..b8`
#'         into `formula` and evaluates against the (renamed) input data.
#'   \item Any other class — loads the fit object referenced by
#'         `fit_object_path` and predicts via a class-specific adapter:
#'         \describe{
#'           \item{`nlme`, `randomForest`, and the default}{`predict(fit, newdata = data)`}
#'           \item{`lme4`}{`predict(fit, newdata = data, allow.new.levels = TRUE)`}
#'           \item{`xgboost`}{coerces the predictor columns to a numeric matrix (no `newdata` argument)}
#'           \item{`systemfit`}{predicts all equations, then selects the column for this row's `response`}
#'           \item{`lavaan`}{`lavaan::lavPredict(fit, newdata = data, type = "yhat")`}
#'         }
#' }
#'
#' Input column names are renamed to the canonical names declared in
#' `nms_dat_predict` before dispatch and reverted on the way out. On the
#' serialized-fit path, if a predictor spec is available (via
#' `predictor_spec_path`) its declared `transform`s (`log`, `sqrt`,
#' `standardize`) are applied to the named predictor columns before the fit is
#' evaluated. Closed-form formulas bake in their own transforms and are not
#' touched.
#'
#' Backward compatibility: when `params` has no `model_class` column, or the
#' value is blank / `NA`, the row is treated as `closed_form` and the historical
#' behavior is preserved exactly.
#'
#' @author Jacob Strunk \email{jacob.strunk@@potlatchdeltic.com}
#'
#' @param dat A `data.frame` / `data.table` of inputs.
#' @param nms_dat_predict Named character vector mapping canonical predictor
#'   names (the names) to columns in `dat` (the values). E.g.,
#'   `c(tpa1 = "TPA_ALL")` renames `TPA_ALL` to `tpa1` before prediction and
#'   restores it on return.
#' @param params `data.frame` / `data.table` of one or more registry rows.
#'   Should include the columns named in `nms_params` plus `response`. May
#'   include `model_class`, `fit_object_path`, and `predictor_spec_path`.
#' @param nms_params Named list with `formula` (name of the formula column,
#'   default `"formula"`) and `params` (parameter columns to substitute,
#'   default `paste0("b", 0:8)`).
#' @param suffix_predict Reserved for future use (prediction column suffix);
#'   currently not appended.
#' @param archive_dir Optional archive root for resolving `fit_object_path` /
#'   `predictor_spec_path`. When `NULL`, the shipped `extdata/` of `package`
#'   is used.
#' @param package Package whose installed `extdata/` holds sidecars when
#'   `archive_dir` is `NULL`. Defaults to `"RForInvt"`.
#' @param ... Reserved for future use.
#'
#' @return `dat` (as `data.table`) with one prediction column per row of
#'   `params`, named by `params$response`.
#'
#' @examples
#' \dontrun{
#'   params_in = model_archive_list(
#'     filter = quote(spp == "pinus.taeda" & region_code == "lcp")
#'   )
#'   dat_out = model_predict(
#'     dat = my_stand_data,
#'     nms_dat_predict = c(tpa1 = "TPA_ALL", hd1 = "Height", age1 = "Stand_Age"),
#'     params = params_in
#'   )
#' }
#'
#' @importFrom data.table data.table as.data.table setnames
#' @importFrom stats predict sd
#' @export
model_predict = function(
                        dat
                        , nms_dat_predict = c(tpa1="tpa",hd1="ht",age1="age")
                        , params
                        , nms_params = list(formula = "formula", params = paste0("b", 0:8))
                        , suffix_predict = ".pd"
                        , archive_dir = NULL
                        , package = "RForInvt"
                        , ...
                        ){

  # 1. coerce inputs and resolve archive root (lazy: only needed for fit rows)
  dat_in    = data.table::as.data.table(dat)
  params_in = data.table::as.data.table(params)
  nms_pd    = params_in[["response"]]

  # 2. rename columns to canonical predictor names
  data.table::setnames(dat_in, old = c(nms_dat_predict), new = names(c(nms_dat_predict)), skip_absent = TRUE)

  # 3. loop over models — dispatch on model_class
  for (i in seq_len(nrow(params_in))){

    model_i = params_in[i, ]
    class_i = .model_predict_class(model_i)

    if (class_i == "closed_form"){
      pdi = try(.fn_predict(
        dat    = dat_in,
        formi  = model_i[[nms_params[["formula"]]]],
        params = .model_predict_bvals(model_i, nms_params[["params"]])
      ), silent = TRUE)

    } else {
      archive_root = .model_archive_root(archive_dir = archive_dir, package = package)
      pdi = try(.model_predict_from_fit(
        dat          = dat_in,
        model_row    = model_i,
        archive_root = archive_root
      ), silent = TRUE)
    }

    if (!inherits(pdi, "try-error")) dat_in[[nms_pd[i]]] = pdi
    else dat_in[[nms_pd[i]]] = NA
  }

  # 4. revert column names
  data.table::setnames(dat_in, new = nms_dat_predict, old = names(nms_dat_predict), skip_absent = TRUE)

  return(dat_in)

}

#' @keywords internal
#' @noRd
# Extract the b* parameter columns present in a registry row as a one-row
# data.table for substitution into the closed-form formula.
.model_predict_bvals = function(model_row, b_names){

  b_present = intersect(b_names, names(model_row))
  return(model_row[, b_present, with = FALSE])

}

#' @keywords internal
#' @noRd
.fn_predict = function(dat, formi, params){

  # populate formula with parameters then evaluate against the data
  form_text  = paste0("substitute(", formi, ")")
  form_parse = parse(text = form_text)
  form_in    = eval(form_parse, envir = params)

  if (is.call(form_in)) res = eval(form_in, envir = dat)
  else res = eval(parse(text = form_in), envir = dat)

  res

}

#' @keywords internal
#' @noRd
.model_predict_class = function(model_row){

  if (!("model_class" %in% names(model_row))) return("closed_form")
  class_val = model_row$model_class[1]
  if (is.na(class_val) || !nzchar(class_val)) return("closed_form")
  return(as.character(class_val))

}

#' @keywords internal
#' @noRd
.model_predict_from_fit = function(dat, model_row, archive_root){

  # 1. resolve and load the fit object
  if (!("fit_object_path" %in% names(model_row))){
    stop("model_predict(): non-closed-form row requires 'fit_object_path' column.")
  }
  fit_full = .model_archive_sidecar_full(model_row$fit_object_path[1], archive_root)
  if (is.na(fit_full)){
    stop("model_predict(): fit object not found for model_id ", model_row$model_id[1])
  }
  .model_archive_check_pkg(model_row)
  fit_in = .model_archive_read_fit(fit_full)

  # 2. apply declared predictor-spec transforms (if any) on a copy of the data
  dat_use = data.table::as.data.table(dat)
  spec = .model_predict_spec(model_row, archive_root)
  dat_use = .model_predict_apply_transforms(dat_use, spec)

  # 3. class-specific prediction adapter
  class_i = .model_predict_class(model_row)
  res = switch(class_i,
    lme4      = .mp_predict_lme4(fit_in, dat_use),
    xgboost   = .mp_predict_xgboost(fit_in, dat_use, spec),
    systemfit = .mp_predict_systemfit(fit_in, dat_use, model_row),
    lavaan    = .mp_predict_lavaan(fit_in, dat_use, model_row),
    .mp_predict_generic(fit_in, dat_use)
  )

  return(as.numeric(res))

}

#' @keywords internal
#' @noRd
.model_predict_spec = function(model_row, archive_root){

  if (!("predictor_spec_path" %in% names(model_row))) return(NULL)
  spec_full = .model_archive_sidecar_full(model_row$predictor_spec_path[1], archive_root)
  if (is.na(spec_full)) return(NULL)
  return(jsonlite::read_json(spec_full, simplifyVector = FALSE))

}

#' @keywords internal
#' @noRd
.model_predict_apply_transforms = function(dat, predictor_spec){

  if (is.null(predictor_spec) || is.null(predictor_spec$predictors)) return(dat)
  for (p in predictor_spec$predictors){
    tf = p$transform
    nm = p$name
    if (is.null(tf) || is.na(tf) || !nzchar(tf)) next
    if (is.null(nm) || !(nm %in% names(dat))) next
    x = dat[[nm]]
    dat[[nm]] = switch(tf,
      log         = log(x),
      sqrt        = sqrt(x),
      standardize = (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE),
      {
        warning("model_predict(): unknown transform '", tf, "' for predictor '", nm, "'; left unchanged.")
        x
      }
    )
  }
  return(dat)

}

#' @keywords internal
#' @noRd
.mp_predict_generic = function(fit, dat){
  stats::predict(fit, newdata = as.data.frame(dat))
}

#' @keywords internal
#' @noRd
.mp_predict_lme4 = function(fit, dat){
  stats::predict(fit, newdata = as.data.frame(dat), allow.new.levels = TRUE)
}

#' @keywords internal
#' @noRd
.mp_predict_xgboost = function(fit, dat, spec){
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("xgboost not installed.")
  cols = .mp_xgb_feature_names(fit, spec, dat)
  mat  = as.matrix(as.data.frame(dat)[, cols, drop = FALSE])
  stats::predict(fit, newdata = mat)
}

#' @keywords internal
#' @noRd
# Determine the feature column order for an xgboost booster: prefer the
# booster's stored feature_names, then the predictor spec, then numeric
# columns of the data.
.mp_xgb_feature_names = function(fit, spec, dat){

  fn = tryCatch(fit$feature_names, error = function(e) NULL)
  if (!is.null(fn) && length(fn) > 0) return(fn)

  if (!is.null(spec) && !is.null(spec$predictors)){
    nm = vapply(spec$predictors, function(p) as.character(p$name), character(1))
    nm = nm[nm %in% names(dat)]
    if (length(nm) > 0) return(nm)
  }

  is_num = vapply(as.data.frame(dat), is.numeric, logical(1))
  return(names(dat)[is_num])

}

#' @keywords internal
#' @noRd
.mp_predict_systemfit = function(fit, dat, model_row){
  pd  = stats::predict(fit, newdata = as.data.frame(dat))
  eqn = model_row$response[1]
  col = grep(paste0("^", eqn, "\\."), names(pd), value = TRUE)
  if (length(col) == 0) col = grep(eqn, names(pd), value = TRUE)
  if (length(col) == 0) col = names(pd)[1]
  pd[[col[1]]]
}

#' @keywords internal
#' @noRd
.mp_predict_lavaan = function(fit, dat, model_row){
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("lavaan not installed.")
  yhat = lavaan::lavPredict(fit, newdata = as.data.frame(dat), type = "yhat")
  eqn  = model_row$response[1]
  if (eqn %in% colnames(yhat)) return(as.numeric(yhat[, eqn]))
  return(as.numeric(yhat[, 1]))
}
