# Revision history:
# 2026-06-10 — JLS — initial DLL-backed model-archive class "nvel": predict
#                    adapter, DLL-availability probe, and a row-registration
#                    convenience. NVEL volume equations are Fortran profile
#                    models (not closed-form), so they ride in the archive as a
#                    stored equation/species selector rather than a serialized
#                    fit object.

#' @title Register an NVEL Volume Equation in a Model Archive
#'
#' @description
#' Append a National Volume Estimator Library (NVEL) volume equation to a model
#' archive registry as a `model_class = "nvel"` row. Unlike `closed_form` rows
#' (a formula + `b*` coefficients) or serialized-fit rows (`fit_object_path`),
#' an `nvel` row stores only the inputs needed to drive `NVEL_volume()` at
#' predict time: the species code, USFS region/forest/district, an optional
#' forced `voleq`, and which NVEL output column to return.
#'
#' @details
#' At prediction time `model_predict()` dispatches `nvel` rows to an adapter that
#' rebuilds a one-species tree list from the canonical `dbh`/`ht` predictor
#' columns plus the stored `spcd`/`region`/`forest`/`district`, calls
#' `NVEL_volume()` (loading the bundled `vollib` DLL if needed), and returns the
#' column named by `nvel_out` (default `"TCFV"`, total cubic-foot volume). When
#' `voleq` is `NA`, `NVEL_volume()` looks up the equation from
#' region/forest/district/species itself.
#'
#' @author Jacob Strunk \email{jacob.strunk@@potlatchdeltic.com}
#'
#' @param archive_dir Writable archive root directory (passed through to
#'   `model_archive_save()`).
#' @param spcd USFS species code (integer).
#' @param region USFS region number.
#' @param forest,district USFS forest / district codes (character). Default
#'   `"01"`.
#' @param spp Species label stored in the `spp` registry column (e.g.
#'   `"pinus.taeda"`).
#' @param region_code Region bucket stored in `region_code` (e.g. `"se"`,
#'   `"pnw"`) for catalog filtering.
#' @param voleq Optional forced volume equation code. `NA` (default) lets
#'   `NVEL_volume()` select the equation.
#' @param nvel_out NVEL output column to return as the prediction. Default
#'   `"TCFV"`. See `NVEL_volume()` for the 15 available columns.
#' @param response Prediction column name. Default `"vol_tcf"`.
#' @param model_id Optional registry id. Default `paste0("NVEL_", spcd, "_r", region)`.
#' @param registry Registry filename. Default `"models.csv"`.
#' @param overwrite Passed to `model_archive_save()`. Default `FALSE`.
#' @param ... Reserved.
#'
#' @return The saved registry row (invisibly), as returned by
#'   `model_archive_save()`.
#'
#' @seealso \code{\link{NVEL_volume}}, \code{\link{model_predict}},
#'   \code{\link{model_archive_save}}
#'
#' @examples
#' \dontrun{
#'   arch = file.path(tempdir(), "nvel_archive")
#'   nvel_volume_register(archive_dir = arch, spcd = 131, region = 8,
#'                        spp = "pinus.taeda", region_code = "se")
#'   row = model_archive_load(model_id = "NVEL_131_r8", archive_dir = arch,
#'                            package = NULL)[[1]]$row
#'   model_predict(dat = data.frame(dbh = c(8, 12), ht = c(50, 70)),
#'                 nms_dat_predict = c(dbh = "dbh", ht = "ht"), params = row,
#'                 archive_dir = arch, package = NULL)
#' }
#'
#' @importFrom data.table data.table
#' @export
nvel_volume_register = function(archive_dir, spcd, region, forest = "01",
                                district = "01", spp, region_code,
                                voleq = NA, nvel_out = "TCFV",
                                response = "vol_tcf", model_id = NULL,
                                registry = "models.csv", overwrite = FALSE, ...){

  if (is.null(model_id)) model_id = paste0("NVEL_", spcd, "_r", region)
  row = data.table::data.table(
    model_id    = model_id,
    model_class = "nvel",
    response    = response,
    spp         = spp,
    region_code = region_code,
    region      = as.character(region),
    forest      = as.character(forest),
    district    = as.character(district),
    spcd        = as.integer(spcd),
    voleq       = as.character(voleq),
    nvel_out    = nvel_out,
    data        = "NVEL",
    status      = "active"
  )
  model_archive_save(fit = NULL, registry_row = row, registry = registry,
                     archive_dir = archive_dir, overwrite = overwrite)

}

#' @keywords internal
#' @noRd
# TRUE if the architecture-appropriate vollib DLL is loaded or loadable.
# Used by model_predict()'s nvel adapter and to gate DLL-dependent tests.
.nvel_dll_available = function(){

  if ("vollib" %in% names(getLoadedDLLs())) return(TRUE)
  paths  = .nvel_dll_paths()
  arch   = R.Version()$arch
  target = if (arch == "x86_64") paths$dll_64 else paths$dll_32
  if (is.null(target) || !nzchar(target) || !file.exists(target)) return(FALSE)
  ok = tryCatch({ dyn.load(target); TRUE }, error = function(e) FALSE)
  return(ok)

}

#' @keywords internal
#' @noRd
# Predict adapter for model_class == "nvel". `dat` has already been renamed to
# canonical predictor names (expects 'dbh' and 'ht'). `model_row` carries the
# stored spcd / region / forest / district / voleq / nvel_out.
.mp_predict_nvel = function(dat, model_row){

  if (!.nvel_dll_available()){
    stop("model_predict(nvel): NVEL 'vollib' DLL is not available.")
  }
  if (!all(c("dbh", "ht") %in% names(dat))){
    stop("model_predict(nvel): requires canonical 'dbh' and 'ht' predictor columns.")
  }

  get1 = function(col, default = NA){
    if (col %in% names(model_row)) model_row[[col]][1] else default
  }
  # Zero-pad forest/district to 2 chars: a registry column of "01"/"02" reads
  # back as integer 1/2 once written to CSV, but NVEL expects 2-char codes.
  pad2 = function(x){
    x = as.character(x)
    if (is.na(x)) return("01")
    if (grepl("^[0-9]+$", x)) return(formatC(as.integer(x), width = 2, flag = "0"))
    x
  }
  spcd_i     = suppressWarnings(as.integer(get1("spcd")))
  region_i   = suppressWarnings(as.integer(get1("region")))
  if (is.na(region_i)) region_i = 1L
  forest_i   = pad2(get1("forest"))
  district_i = pad2(get1("district"))
  voleq_i    = as.character(get1("voleq"))
  out_col    = as.character(get1("nvel_out")); if (is.na(out_col) || !nzchar(out_col)) out_col = "TCFV"

  tl = data.frame(
    region   = region_i,
    forest   = forest_i,
    district = district_i,
    spcd     = spcd_i,
    dbh      = as.numeric(dat[["dbh"]]),
    ht       = as.numeric(dat[["ht"]])
  )

  res = NVEL_volume(
    dfTL        = tl,
    voleq       = if (is.na(voleq_i) || !nzchar(voleq_i)) NA else voleq_i,
    region      = region_i,
    forest      = forest_i,
    district    = district_i,
    spcdNm      = "spcd",
    dbhNm       = "dbh",
    htNm        = "ht",
    vol2biomass = FALSE,
    load_dll    = TRUE
  )

  if (!out_col %in% names(res)) out_col = "TCFV"
  return(as.numeric(res[[out_col]]))

}
