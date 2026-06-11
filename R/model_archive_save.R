# Revision history:
# 2026-05-22 — JLS — initial coordinator for model_archive_save()
# 2026-06-08 — JLS — Pass-1 refactor: explicit archive_dir (no implicit
#                    default), model_id uniqueness guard, atomic registry +
#                    sidecar writes, root-relative sidecar paths. Legacy
#                    registry_path= accepted with a deprecation warning.

#' @title Save a Model to a Model Archive
#'
#' @description
#' Append a fitted model to a registry CSV in a writable archive directory,
#' optionally serializing the fit object, variance-covariance matrix, and typed
#' predictor specification as sidecar files.
#'
#' @details
#' One call writes one equation. Joint multivariate systems (SUR / SEM)
#' are saved by calling `model_archive_save()` once per equation, sharing
#' the same `system_id` and (typically) the same `vcov_path` and
#' `predictor_spec_path`.
#'
#' \strong{Archive location.} `archive_dir` is required and names a writable
#' directory; the registry CSV and the `fits/`, `vcov/`, `predictor_specs/`
#' sidecar sub-directories live under it. There is no implicit default — this
#' function never writes into an installed package's directory. (The legacy
#' `registry_path` argument is still accepted with a deprecation warning; when
#' supplied, `archive_dir` defaults to its directory.)
#'
#' \strong{Dispatch for `fit`.} If `registry_row$model_class` is `""` or
#' `"closed_form"`, `fit` is ignored (the closed-form `formula` and `b*`
#' columns carry everything needed). Otherwise `fit` is serialized to
#' `<archive_dir>/fits/<model_id>.rds` and that root-relative path is written
#' to `registry_row$fit_object_path`.
#'
#' \strong{`vcov`.} When non-NULL and `registry_row$system_id` is non-empty, the
#' object is serialized to `<archive_dir>/vcov/<system_id>.rds`. Repeated calls
#' with the same `system_id` overwrite (the system vcov is shared).
#'
#' \strong{`predictor_spec`.} When non-NULL the list is written as JSON to
#' `<archive_dir>/predictor_specs/<spec_id>.json`. `spec_id` is taken from
#' `predictor_spec$spec_id` if present, else `paste0("model_", model_id)`.
#'
#' \strong{Integrity.} A duplicate `model_id` in the target registry is an error
#' unless `overwrite = TRUE` (which replaces the existing row). The registry is
#' written atomically (temp file then rename) so an interrupted write leaves the
#' previous registry intact. Schema is additive: missing columns on either side
#' are filled with `NA`.
#'
#' @author Jacob Strunk \email{jacob.strunk@@potlatchdeltic.com}
#'
#' @param fit Fitted model object. Pass `NULL` for closed-form rows.
#' @param registry_row A single-row `data.table` or named list of registry
#'   columns. At minimum `model_id`, `model_class`, `response`, `spp`,
#'   `region_code` must be set.
#' @param predictor_spec Optional list to be serialized as JSON. Should
#'   match the schema in `inst/docs/model_archive_spec.md`.
#' @param vcov Optional variance-covariance object (matrix, list, etc.)
#'   for SUR / SEM systems.
#' @param registry Registry filename (resolved under `archive_dir`) or a full
#'   path. Defaults to `"models.csv"`.
#' @param archive_dir Writable archive root directory. Required (created if it
#'   does not exist). No implicit default.
#' @param overwrite If `TRUE`, replace an existing row with the same
#'   `model_id`. Defaults to `FALSE` (duplicate is an error).
#' @param lighten If `TRUE`, reduce the fit with `butcher::butcher()` before
#'   serialization. Applied only to a predict-safe allowlist (`lme4`, `nlme`)
#'   and only when `butcher` is installed; a no-op otherwise. Defaults to
#'   `FALSE`. The outcome is recorded in the `lightened` registry column.
#' @param registry_path \strong{Deprecated.} Full path to a registry CSV; use
#'   `registry` / `archive_dir` instead.
#'
#' @return The `registry_row` (as a single-row `data.table`) after path
#'   columns have been populated. Invisibly.
#'
#' @examples
#' \dontrun{
#'   arch = file.path(tempdir(), "rforinvt_archive")
#'   row_in = data.table::data.table(
#'     model_id    = 9999,
#'     model_class = "closed_form",
#'     response    = "ba1",
#'     spp         = "pinus.taeda",
#'     region_code = "test",
#'     formula     = "b0 + b1 * age1",
#'     b0          = 1.0,
#'     b1          = 0.5
#'   )
#'   model_archive_save(fit = NULL, registry_row = row_in, archive_dir = arch)
#' }
#'
#' @importFrom data.table as.data.table copy fread fwrite rbindlist setcolorder
#' @importFrom jsonlite write_json
#' @importFrom utils packageVersion
#' @export
model_archive_save = function(fit, registry_row, predictor_spec = NULL,
                              vcov = NULL, registry = "models.csv",
                              archive_dir = NULL, overwrite = FALSE,
                              lighten = FALSE, registry_path = NULL){

  # 1. validate inputs and resolve archive root + registry path
  ctx = .model_archive_save_validate(registry_row, registry, archive_dir, registry_path)

  # 2. serialize sidecars (fit, vcov, predictor_spec) and patch path columns
  row_with_paths = .model_archive_save_sidecars(fit, ctx$registry_row, predictor_spec, vcov, ctx$archive_root, lighten)

  # 3. append the row to the registry CSV (uniqueness guard + atomic write)
  .model_archive_save_append(row_with_paths, ctx$registry_full_path, overwrite)

  invisible(row_with_paths)

}

#' @keywords internal
#' @noRd
.model_archive_save_validate = function(registry_row, registry, archive_dir, registry_path){

  # 1. coerce to single-row data.table
  row_in = data.table::as.data.table(as.list(registry_row))
  if (nrow(row_in) != 1){
    stop("model_archive_save(): registry_row must contain exactly one row.")
  }

  # 2. minimal required identity columns
  cols_required = c("model_id", "model_class", "response", "spp", "region_code")
  cols_missing  = setdiff(cols_required, names(row_in))
  if (length(cols_missing) > 0){
    stop("model_archive_save(): registry_row is missing required columns: ", paste(cols_missing, collapse = ", "))
  }
  if (is.na(row_in$model_id) || row_in$model_id == ""){
    stop("model_archive_save(): registry_row$model_id must be set.")
  }

  # 3. normalize empty model_class to closed_form
  if (is.na(row_in$model_class) || row_in$model_class == ""){
    row_in[, model_class := "closed_form"]
  }

  # 4. legacy registry_path shim
  if (!is.null(registry_path)){
    warning("model_archive_save(): 'registry_path' is deprecated; use 'registry' / 'archive_dir'.")
    registry = registry_path
    if (is.null(archive_dir)) archive_dir = dirname(registry_path)
  }

  # 5. resolve writable archive root and registry path; seed if absent
  archive_root  = .model_archive_root(archive_dir = archive_dir, must_exist = TRUE, for_write = TRUE)
  registry_full = .model_archive_registry_path(registry, archive_root)
  .model_archive_seed_registry(registry_full)

  return(list(registry_row = row_in, registry_full_path = registry_full, archive_root = archive_root))

}

#' @keywords internal
#' @noRd
.model_archive_save_sidecars = function(fit, registry_row, predictor_spec, vcov, archive_root, lighten = FALSE){

  row_out = data.table::copy(registry_row)
  model_id_chr = as.character(row_out$model_id[1])

  # 1. fit object (non-closed-form only): optional butcher lightening,
  #    class-appropriate serializer + provenance columns
  is_closed = identical(row_out$model_class[1], "closed_form")
  if (!is_closed && !is.null(fit)){
    model_class_chr = as.character(row_out$model_class[1])
    lit     = .model_archive_lighten_fit(fit, model_class_chr, lighten)
    written = .model_archive_write_fit(lit$fit, file.path(archive_root, "fits"), model_id_chr, model_class_chr)
    pkg_chr = .model_archive_fit_pkg(model_class_chr)
    pkg_ver = if (!is.na(pkg_chr) && requireNamespace(pkg_chr, quietly = TRUE)){
      as.character(utils::packageVersion(pkg_chr))
    } else NA_character_
    row_out[, `:=`(
      fit_object_path = written$rel,
      serializer      = written$serializer,
      pkg             = pkg_chr,
      pkg_version     = pkg_ver,
      r_version       = as.character(getRversion()),
      lightened       = lit$lightened
    )]
  }

  # 2. vcov sidecar (system-level)
  has_system = (!is.null(row_out$system_id)) && !is.na(row_out$system_id[1]) && nzchar(as.character(row_out$system_id[1]))
  if (has_system && !is.null(vcov)){
    system_id_chr = as.character(row_out$system_id[1])
    dir_vcov = file.path(archive_root, "vcov")
    dir.create(dir_vcov, showWarnings = FALSE, recursive = TRUE)
    vcov_file = file.path(dir_vcov, paste0(system_id_chr, ".rds"))
    .model_archive_atomic_saveRDS(vcov, vcov_file)
    row_out[, vcov_path := file.path("vcov", paste0(system_id_chr, ".rds"))]
  }

  # 3. predictor-spec JSON
  if (!is.null(predictor_spec)){
    spec_id_chr = predictor_spec$spec_id
    if (is.null(spec_id_chr) || !nzchar(spec_id_chr)){
      spec_id_chr = paste0("model_", model_id_chr)
    }
    dir_specs = file.path(archive_root, "predictor_specs")
    dir.create(dir_specs, showWarnings = FALSE, recursive = TRUE)
    spec_file = file.path(dir_specs, paste0(spec_id_chr, ".json"))
    spec_tmp = paste0(spec_file, ".tmp")
    jsonlite::write_json(predictor_spec, spec_tmp, pretty = TRUE, auto_unbox = TRUE, null = "null")
    file.rename(spec_tmp, spec_file)
    row_out[, predictor_spec_path := file.path("predictor_specs", paste0(spec_id_chr, ".json"))]
  }

  return(row_out)

}

#' @keywords internal
#' @noRd
.model_archive_save_append = function(row_with_paths, registry_full_path, overwrite){

  # 1. read existing registry preserving column order
  existing = data.table::fread(registry_full_path, na.strings = c("", "NA"))

  # 2. uniqueness guard on model_id
  model_id_new = row_with_paths$model_id[1]
  if (nrow(existing) > 0 && "model_id" %in% names(existing) && model_id_new %in% existing$model_id){
    if (!overwrite){
      stop("model_archive_save(): model_id ", model_id_new,
           " already exists in ", basename(registry_full_path),
           ". Pass overwrite = TRUE to replace it.")
    }
    existing = existing[existing$model_id != model_id_new, ]
  }

  # 3. union columns: fill missing on each side with NA
  cols_only_in_new = setdiff(names(row_with_paths), names(existing))
  cols_only_in_old = setdiff(names(existing), names(row_with_paths))
  if (length(cols_only_in_new) > 0){
    for (col_add in cols_only_in_new) existing[, (col_add) := NA]
  }
  row_fill = data.table::copy(row_with_paths)
  if (length(cols_only_in_old) > 0){
    for (col_add in cols_only_in_old) row_fill[, (col_add) := NA]
  }

  # 4. align column order on the existing header
  data.table::setcolorder(row_fill, names(existing))

  # 5. rbind and write back atomically (temp then rename)
  combined = data.table::rbindlist(list(existing, row_fill), use.names = TRUE, fill = TRUE)
  tmp_path = paste0(registry_full_path, ".tmp")
  data.table::fwrite(combined, tmp_path)
  ok = file.rename(tmp_path, registry_full_path)
  if (!ok){
    if (file.exists(tmp_path)) file.remove(tmp_path)
    stop("model_archive_save(): atomic rename failed for ", registry_full_path)
  }

  invisible(NULL)

}
