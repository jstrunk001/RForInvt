# Revision history:
# 2026-05-22 — JLS — initial coordinator for model_archive_load()
# 2026-06-08 — JLS — Pass-1 refactor: resolve sidecars against a resolved
#                    archive root (archive_dir or a package's shipped extdata),
#                    tolerant of legacy "extdata/..." sidecar paths.

#' @title Load Models from a Model Archive
#'
#' @description
#' Look up models in a registry CSV and return a fully hydrated list per
#' match: the registry row, the deserialized fit object (if any), the
#' joint-system variance-covariance (if any), and the parsed predictor
#' spec (if any).
#'
#' @details
#' Lookup keys: pass either `model_id` (one or more ids) or a `filter`
#' expression (an unquoted `data.table` `i`-expression evaluated against the
#' registry). When both are `NULL` all rows are returned. The `filter`
#' expression is evaluated in the registry's data context, so it may reference
#' any registry column directly.
#'
#' Sidecar paths in the registry are resolved against the archive root
#' (`archive_dir`, or the installed `extdata/` of `package`). Root-relative
#' paths (`fits/9999.rds`), legacy `extdata/`-prefixed paths, and absolute
#' paths are all accepted. Missing or absent sidecars produce `NULL` slots
#' rather than errors — caller code is expected to check.
#'
#' @author Jacob Strunk \email{jacob.strunk@@potlatchdeltic.com}
#'
#' @param model_id Optional id (or vector of ids) of `model_id` values to load.
#' @param filter Optional `data.table` `i`-expression (e.g.,
#'   `quote(spp == "pinus.taeda" & region_code == "lcp")`) used to subset
#'   the registry. Use `quote()` to defer evaluation.
#' @param registry Registry filename (resolved under the archive root) or full
#'   path. Defaults to `"models.csv"`.
#' @param archive_dir Optional archive root directory. When `NULL`, the
#'   shipped `extdata/` of `package` is used.
#' @param package Package whose installed `extdata/` holds the archive when
#'   `archive_dir` is `NULL`. Defaults to `"RForInvt"`.
#' @param registry_path \strong{Deprecated.} Full path to a registry CSV; use
#'   `registry` / `archive_dir` instead.
#'
#' @return A list of length `nrow(matches)`. Each element has slots
#'   `row` (single-row `data.table`), `fit` (deserialized fit or `NULL`),
#'   `vcov` (matrix / list / `NULL`), and `predictor_spec` (parsed list
#'   or `NULL`).
#'
#' @examples
#' \dontrun{
#'   hits1 = model_archive_load(model_id = c(1, 2))
#'   hits2 = model_archive_load(
#'     filter = quote(spp == "pinus.taeda" & region_code == "lcp")
#'   )
#' }
#'
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#' @export
model_archive_load = function(model_id = NULL, filter = NULL, registry = NULL,
                              archive_dir = NULL, package = "RForInvt",
                              registry_path = NULL){

  # 1. resolve registry path + archive root and read
  ctx = .model_archive_load_resolve(registry, archive_dir, package, registry_path)

  # 2. select rows matching model_id or filter
  matches = .model_archive_load_filter(ctx$registry, model_id, filter)

  # 3. hydrate sidecars for each matching row
  out_list = .model_archive_load_hydrate(matches, ctx$archive_root)

  return(out_list)

}

#' @keywords internal
#' @noRd
.model_archive_load_resolve = function(registry, archive_dir, package, registry_path){

  # 1. legacy registry_path shim
  if (!is.null(registry_path)){
    warning("model_archive_load(): 'registry_path' is deprecated; use 'registry' / 'archive_dir'.")
    registry = registry_path
    if (is.null(archive_dir)) archive_dir = dirname(registry_path)
  }

  # 2. resolve archive root (read: archive_dir or package's extdata)
  archive_root = .model_archive_root(archive_dir = archive_dir, package = package)

  # 3. registry default
  if (is.null(registry)) registry = "models.csv"
  registry_full = .model_archive_registry_path(registry, archive_root)
  if (!file.exists(registry_full)){
    stop("model_archive_load(): registry file not found: ", registry_full)
  }
  registry_in = data.table::fread(registry_full, na.strings = c("", "NA"))

  return(list(registry = registry_in, archive_root = archive_root))

}

#' @keywords internal
#' @noRd
.model_archive_load_filter = function(registry_in, model_id, filter){

  rows_out = registry_in

  if (!is.null(model_id)){
    rows_out = rows_out[rows_out$model_id %in% model_id, ]
  }

  if (!is.null(filter)){
    rows_out = rows_out[eval(filter, envir = rows_out), ]
  }

  return(rows_out)

}

#' @keywords internal
#' @noRd
.model_archive_load_hydrate = function(matches, archive_root){

  if (nrow(matches) == 0){
    return(list())
  }

  out_list = vector("list", nrow(matches))
  for (row_idx in seq_len(nrow(matches))){
    row_i = matches[row_idx, ]
    fit_i  = .model_archive_load_fit(row_i, archive_root)
    vcov_i = .model_archive_load_rds(row_i, archive_root, col_name = "vcov_path")
    spec_i = .model_archive_load_json(row_i, archive_root, col_name = "predictor_spec_path")
    out_list[[row_idx]] = list(
      row             = row_i,
      fit             = fit_i,
      vcov            = vcov_i,
      predictor_spec  = spec_i
    )
  }

  return(out_list)

}

#' @keywords internal
#' @noRd
.model_archive_load_fit = function(row_i, archive_root){

  if (!("fit_object_path" %in% names(row_i))) return(NULL)
  full_path = .model_archive_sidecar_full(row_i[["fit_object_path"]][1], archive_root)
  if (is.na(full_path)) return(NULL)
  .model_archive_check_pkg(row_i)
  return(.model_archive_read_fit(full_path))

}

#' @keywords internal
#' @noRd
.model_archive_load_rds = function(row_i, archive_root, col_name){

  if (!(col_name %in% names(row_i))) return(NULL)
  full_path = .model_archive_sidecar_full(row_i[[col_name]][1], archive_root)
  if (is.na(full_path)) return(NULL)
  return(readRDS(full_path))

}

#' @keywords internal
#' @noRd
.model_archive_load_json = function(row_i, archive_root, col_name){

  if (!(col_name %in% names(row_i))) return(NULL)
  full_path = .model_archive_sidecar_full(row_i[[col_name]][1], archive_root)
  if (is.na(full_path)) return(NULL)
  return(jsonlite::read_json(full_path, simplifyVector = FALSE))

}
