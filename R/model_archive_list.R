# Revision history:
# 2026-05-22 — JLS — initial coordinator for model_archive_list()
# 2026-06-08 — JLS — Pass-1 refactor: generic registry-file `pattern`,
#                    archive_dir/package root resolution, syntactic
#                    `registry_file` provenance column.

#' @title List Models in a Model Archive
#'
#' @description
#' Return registry rows from one or more CSV registries without loading
#' any sidecar fit objects, variance-covariance matrices, or predictor
#' specs. Intended for fast catalog inspection.
#'
#' @details
#' By default scans every CSV matching `pattern` in the archive root and
#' `rbind`s them with `fill = TRUE` so registries with slightly different
#' columns coexist. Pass `registry` (filename or path) to restrict to one
#' file. Pass `filter` to subset the result.
#'
#' @author Jacob Strunk \email{jacob.strunk@@potlatchdeltic.com}
#'
#' @param filter Optional `data.table` `i`-expression (e.g.,
#'   `quote(model_class == "closed_form")`) used to subset the combined
#'   registry. Use `quote()` to defer evaluation.
#' @param registry Optional single registry filename (resolved under the
#'   archive root) or full path. When `NULL`, all files matching `pattern`
#'   in the archive root are read and combined.
#' @param pattern Regular expression matched against registry file names when
#'   `registry` is `NULL`. Defaults to `"\\.csv$"` (any CSV in the archive root).
#' @param archive_dir Optional archive root directory. When `NULL`, the
#'   shipped `extdata/` of `package` is used.
#' @param package Package whose installed `extdata/` holds the registries when
#'   `archive_dir` is `NULL`. Defaults to `"RForInvt"`.
#'
#' @return A `data.table` of registry rows. An additional column
#'   `registry_file` records which CSV each row came from.
#'
#' @examples
#' \dontrun{
#'   cat_full = model_archive_list()
#'   cat_lobl = model_archive_list(
#'     filter = quote(spp == "pinus.taeda" & model_class == "closed_form")
#'   )
#' }
#'
#' @importFrom data.table fread rbindlist
#' @export
model_archive_list = function(filter = NULL, registry = NULL,
                              pattern = "\\.csv$",
                              archive_dir = NULL, package = "RForInvt"){

  # 1. resolve registry file(s)
  files_to_read = .model_archive_list_files(registry, pattern, archive_dir, package)

  # 2. read and combine
  combined = .model_archive_list_combine(files_to_read)

  # 3. optionally filter
  rows_out = .model_archive_list_filter(combined, filter)

  return(rows_out)

}

#' @keywords internal
#' @noRd
.model_archive_list_files = function(registry, pattern, archive_dir, package){

  archive_root = .model_archive_root(archive_dir = archive_dir, package = package)

  # 1. single explicit registry
  if (!is.null(registry)){
    registry_full = .model_archive_registry_path(registry, archive_root)
    if (!file.exists(registry_full)){
      stop("model_archive_list(): registry file not found: ", registry_full)
    }
    return(registry_full)
  }

  # 2. scan the archive root for matching files
  files_csv = list.files(archive_root, pattern = pattern, full.names = TRUE)
  if (length(files_csv) == 0){
    stop("model_archive_list(): no files matching '", pattern, "' found in ", archive_root)
  }
  return(files_csv)

}

#' @keywords internal
#' @noRd
.model_archive_list_combine = function(files_to_read){

  rows_per_file = lapply(files_to_read, function(file_path){
    rows_in = data.table::fread(file_path, na.strings = c("", "NA"))
    rows_in[, registry_file := basename(file_path)]
    rows_in
  })
  combined_out = data.table::rbindlist(rows_per_file, use.names = TRUE, fill = TRUE)

  return(combined_out)

}

#' @keywords internal
#' @noRd
.model_archive_list_filter = function(combined_in, filter){

  if (is.null(filter)) return(combined_in)
  rows_out = combined_in[eval(filter, envir = combined_in), ]
  return(rows_out)

}
