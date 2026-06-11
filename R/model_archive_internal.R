# Revision history:
# 2026-06-08 — JLS — Pass-1 archive refactor: shared internal helpers for
#                    locating an archive root, seeding a registry, resolving
#                    sidecar paths, and writing files atomically.
# 2026-06-08 — JLS — Pass-2: native xgboost (de)serialization, fit provenance,
#                    package-version checks, optional butcher lightening.

# Resolve the archive root directory.
# Writes (for_write = TRUE) require an explicit archive_dir — there is no
# implicit default. Reads accept either an explicit archive_dir or a `package`
# whose installed extdata/ is used as a read-only shipped archive.
#' @keywords internal
#' @noRd
.model_archive_root = function(archive_dir = NULL, package = NULL,
                               must_exist = FALSE, for_write = FALSE){

  # 1. explicit directory always wins
  if (!is.null(archive_dir)){
    root_out = archive_dir

  } else if (!for_write && !is.null(package)){
    # 2. reads may fall back to a package's shipped extdata/
    root_out = system.file("extdata", package = package)
    if (!nzchar(root_out)){
      stop(".model_archive_root(): package '", package, "' has no installed extdata/.")
    }

  } else {
    # 3. no implicit default (D1)
    stop(".model_archive_root(): 'archive_dir' is required",
         if (!for_write) " (or pass 'package' for a shipped read-only archive)." else ".")
  }

  if (must_exist && !dir.exists(root_out)){
    dir.create(root_out, recursive = TRUE, showWarnings = FALSE)
  }
  return(root_out)

}

# Resolve a registry argument to a full path under an archive root. A bare
# filename is placed under archive_root; a path with a separator is used as-is.
#' @keywords internal
#' @noRd
.model_archive_registry_path = function(registry, archive_root){

  if (grepl("[/\\\\]", registry)){
    return(registry)
  }
  return(file.path(archive_root, registry))

}

# Core registry column order used when seeding a brand-new registry CSV. The
# append step unions columns, so this only needs the common identity /
# provenance / closed-form / sidecar columns; others are added on demand.
#' @keywords internal
#' @noRd
.model_archive_core_columns = function(){
  c("model_id", "system_id", "model_class", "response", "spp",
    "region_code", "region", "data", "year",
    "authors", "compiler", "notes", "status", "test date", "tester", "data_ref",
    "formula", paste0("b", 0:8),
    "n", "r2_pd", "syx_pd", "r2_proj", "syx_proj",
    "x1", "x2", "x3", "x4", "x5", "predictor_spec_path",
    "fit_object_path", "vcov_path",
    "serializer", "pkg", "pkg_version", "r_version", "lightened",
    # NBEL biomass provenance (closed_form rows)
    "comp_token", "bias_flag", "ht_unit", "source_id",
    # NVEL volume selectors (model_class == "nvel" rows)
    "voleq", "spcd", "forest", "district", "nvel_out")
}

# Create an empty registry CSV with the core header if it does not yet exist.
#' @keywords internal
#' @noRd
.model_archive_seed_registry = function(registry_full_path){

  if (file.exists(registry_full_path)) return(invisible(FALSE))
  dir.create(dirname(registry_full_path), recursive = TRUE, showWarnings = FALSE)
  cols_core = .model_archive_core_columns()
  cols_list = rep(list(character(0)), length(cols_core))
  names(cols_list) = cols_core
  header_dt = data.table::as.data.table(cols_list)
  data.table::fwrite(header_dt, registry_full_path)
  return(invisible(TRUE))

}

# Resolve a (root-relative or legacy) sidecar path to a full path. Accepts
# current root-relative paths ("fits/9999.rds"), legacy "extdata/..." paths,
# and absolute paths. Returns NA_character_ when nothing resolves.
#' @keywords internal
#' @noRd
.model_archive_sidecar_full = function(rel_path, archive_root){

  if (is.na(rel_path) || !nzchar(rel_path)) return(NA_character_)

  # 1. absolute / already-resolvable path
  if (file.exists(rel_path)) return(rel_path)

  # 2. root-relative
  cand_root = file.path(archive_root, rel_path)
  if (file.exists(cand_root)) return(cand_root)

  # 3. legacy "extdata/..." prefix
  rel_strip = sub("^extdata[/\\\\]", "", rel_path)
  cand_strip = file.path(archive_root, rel_strip)
  if (file.exists(cand_strip)) return(cand_strip)

  return(NA_character_)

}

# Write obj to path atomically (write to path.tmp, then rename).
#' @keywords internal
#' @noRd
.model_archive_atomic_saveRDS = function(obj, path){

  tmp_path = paste0(path, ".tmp")
  saveRDS(obj, tmp_path)
  ok = file.rename(tmp_path, path)
  if (!ok){
    if (file.exists(tmp_path)) file.remove(tmp_path)
    stop(".model_archive_atomic_saveRDS(): atomic rename failed for ", path)
  }
  return(invisible(path))

}

# Map a model_class to its originating R package (for provenance / require).
#' @keywords internal
#' @noRd
.model_archive_fit_pkg = function(model_class){
  switch(as.character(model_class),
    nlme         = "nlme",
    lme4         = "lme4",
    randomForest = "randomForest",
    xgboost      = "xgboost",
    systemfit    = "systemfit",
    lavaan       = "lavaan",
    nvel         = "RForInvt",
    NA_character_
  )
}

# Optionally reduce a fit with butcher before serialization. Restricted to a
# predict-safe allowlist (lme4, nlme); a no-op for every other class, when
# lighten is FALSE, or when butcher is not installed. Returns the (possibly
# lightened) fit and a logical flag.
#' @keywords internal
#' @noRd
.model_archive_lighten_fit = function(fit, model_class, lighten){

  allow = c("lme4", "nlme")
  if (isTRUE(lighten) && as.character(model_class) %in% allow &&
      requireNamespace("butcher", quietly = TRUE)){
    return(list(fit = butcher::butcher(fit), lightened = TRUE))
  }
  return(list(fit = fit, lightened = FALSE))

}

# Serialize a fit to the fits/ directory using a class-appropriate format,
# written atomically. xgboost boosters use the native xgb.save (.ubj), which is
# version-stable; everything else uses RDS. Returns the root-relative path and
# the serializer tag.
#' @keywords internal
#' @noRd
.model_archive_write_fit = function(fit, dir_fits, model_id_chr, model_class){

  dir.create(dir_fits, showWarnings = FALSE, recursive = TRUE)

  if (identical(as.character(model_class), "xgboost")){
    if (!requireNamespace("xgboost", quietly = TRUE)){
      stop(".model_archive_write_fit(): xgboost not installed; cannot serialize booster.")
    }
    full_path = file.path(dir_fits, paste0(model_id_chr, ".ubj"))
    tmp_path  = file.path(dir_fits, paste0(model_id_chr, "_tmp.ubj"))
    xgboost::xgb.save(fit, tmp_path)
    ok = file.rename(tmp_path, full_path)
    if (!ok){
      if (file.exists(tmp_path)) file.remove(tmp_path)
      stop(".model_archive_write_fit(): atomic rename failed for ", full_path)
    }
    return(list(rel = file.path("fits", basename(full_path)), serializer = "xgb.ubj"))
  }

  full_path = file.path(dir_fits, paste0(model_id_chr, ".rds"))
  .model_archive_atomic_saveRDS(fit, full_path)
  return(list(rel = file.path("fits", basename(full_path)), serializer = "rds"))

}

# Read a fit sidecar, dispatching on file extension: .ubj/.json -> xgb.load,
# otherwise readRDS.
#' @keywords internal
#' @noRd
.model_archive_read_fit = function(full_path){

  if (grepl("\\.(ubj|json)$", full_path, ignore.case = TRUE)){
    if (!requireNamespace("xgboost", quietly = TRUE)){
      stop(".model_archive_read_fit(): xgboost not installed; cannot read ", basename(full_path))
    }
    return(xgboost::xgb.load(full_path))
  }
  return(readRDS(full_path))

}

# Warn when the installed version of a fit's originating package differs from
# the version recorded at save time (or the package is missing).
#' @keywords internal
#' @noRd
.model_archive_check_pkg = function(row_i){

  if (!all(c("pkg", "pkg_version") %in% names(row_i))) return(invisible(NULL))
  pkg = row_i[["pkg"]][1]
  ver = row_i[["pkg_version"]][1]
  if (is.na(pkg) || !nzchar(pkg)) return(invisible(NULL))
  if (!requireNamespace(pkg, quietly = TRUE)){
    warning("model archive: package '", pkg, "' (needed to use this fit) is not installed.")
    return(invisible(NULL))
  }
  if (!is.na(ver) && nzchar(ver)){
    ver_now = as.character(utils::packageVersion(pkg))
    if (!identical(ver_now, ver)){
      warning("model archive: fit was saved with ", pkg, " ", ver,
              " but ", ver_now, " is installed; predictions may differ.")
    }
  }
  return(invisible(NULL))

}
