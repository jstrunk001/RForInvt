#'@title
#'  Archive a data object to multiple formats (robust to per-format failures)
#'
#'@description
#'  Saves a given data object to specified formats (CSV, RDS, SQLite, XLSX) with versioned filenames.
#'  Each format write is isolated so one failure does not stop others. Detailed
#'  per-format status is returned (success/path/error).
#'
#'@details
#'
#'  - CSV and RDS files are written using `write.csv()` and `saveRDS()`.
#'  - XLSX export uses `openxlsx::write.xlsx()` with sheet name = `table_nm` clipped to 30 chars.
#'  - SQLite export uses `sf::st_write()` (requires `data` to be an `sf` object).
#'  - If `table_nm = NA`, the base name of `path_out` is used for sheet/table names.
#'  - Row names are excluded by default in CSV and XLSX outputs (`row.names = FALSE`).
#'  - Filenames are versioned using `file_version()`.
#'  - If `use_subdirs = TRUE`, files are placed in subdirectories named after their format (e.g., `csv/`, `rds/`), which are created if needed.
#'  - Column names are normalized for duplicate detection (case-insensitive). If duplicates exist after converting to lowercase, numeric suffixes (.2, .3, ...) are appended.
#'  - Each writer uses `tryCatch()` to capture and report failures without stopping the function unless `stop_on_error = TRUE`.
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.8 \tab 2025-11-03 Add per-format tryCatch, stop_on_error, and fixed subdir pathing. \cr
#'1.7 \tab 2025-11-03 Added sheet name clipping to 30 characters for XLSX. \cr
#'1.6 \tab 2025-11-03 Added duplicate column name handling logic. \cr
#'}
#'
#'@author
#'  Jacob Strunk <your.email@domain.com>
#'
#'@param data Data frame or `sf` object to archive.
#'@param path_out Character. Base path (without extension) for output files.
#'@param extensions Named character vector of file extensions. Defaults to
#'   `c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx")`.
#'@param table_nm Character or NA. Table name for SQLite export and sheet name for XLSX export.
#'   If `NA`, defaults to the base name of `path_out`.
#'@param do_csv Logical. If `TRUE`, write CSV file (default = `TRUE`).
#'@param do_rds Logical. If `TRUE`, write RDS file (default = `TRUE`).
#'@param do_sqlite Logical. If `TRUE`, write SQLite file (default = `TRUE`).
#'@param do_xlsx Logical. If `TRUE`, write XLSX file (default = `TRUE`).
#'@param increment Logical. If `TRUE`, increment version numbers in filenames (default = `TRUE`).
#'@param use_subdirs Logical. If `TRUE`, create subdirectories for each format (default = `FALSE`).
#'@param row.names Logical. If `TRUE`, include row names in CSV and XLSX outputs (default = `FALSE`).
#'@param stop_on_error Logical. If `TRUE`, rethrow on first write error; otherwise collect errors and continue (default = `FALSE`).
#'
#'@return Invisibly returns a named list per format with `success`, `path` (if any), and `error` (if any).
#'
#'@examples
#'  \dontrun{
#'    # Archive all formats in subdirectories, using base name for sheet/table
#'    archive_table(mtcars, "output/mydata", table_nm = NA, use_subdirs = TRUE)
#'
#'    # Archive only CSV and XLSX with row names included; continue on errors
#'    archive_table(mtcars, "output/mydata", do_rds = FALSE, do_sqlite = FALSE, row.names = TRUE)
#'
#'    # Fail fast on first error
#'    archive_table(mtcars, "output/mydata", stop_on_error = TRUE)
#'  }
#'
#'@importFrom sf st_write
#'@importFrom openxlsx write.xlsx
#'@export
archive_table = function(data,
                         path_out,
                         extensions = c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx"),
                         table_nm = c(NA, "r_data"),
                         do_csv = TRUE,
                         do_rds = TRUE,
                         do_sqlite = TRUE,
                         do_xlsx = TRUE,
                         increment = TRUE,
                         use_subdirs = FALSE,
                         row.names = FALSE,
                         stop_on_error = FALSE) {

  # Optional: only require namespaces when needed to avoid hard dependency on load
  # (We'll check again inside SQLite/XLSX blocks)
  suppressWarnings({
    have_sf = requireNamespace("sf", quietly = TRUE)
    have_openxlsx = requireNamespace("openxlsx", quietly = TRUE)
  })

  table_nm = table_nm[1]

  # ---- Internal Argument Checks ----
  if (!is.data.frame(data) && !inherits(data, "sf")) stop("`data` must be a data.frame or an `sf` object.")
  if (!is.character(path_out) || length(path_out) != 1) stop("`path_out` must be a single character scalar.")
  if (!is.logical(do_csv) || !is.logical(do_rds) || !is.logical(do_sqlite) || !is.logical(do_xlsx) ||
      !is.logical(increment) || !is.logical(use_subdirs) || !is.logical(stop_on_error)) {
    stop("Flags do_csv, do_rds, do_sqlite, do_xlsx, increment, use_subdirs, stop_on_error must be logical.")
  }
  if (!is.character(extensions) || is.null(names(extensions))) stop("`extensions` must be a *named* character vector.")

  # ---- Normalize column names and handle duplicates ----
  normalize_colnames = function(names_vec) {
    lower_names = tolower(names_vec)
    dup_count = integer(length(lower_names))
    seen = list()
    for (i in 1:length(lower_names)) {
      nm = lower_names[i]
      if (!(nm %in% names(seen))) {
        seen[[nm]] = 1
      } else {
        seen[[nm]] = seen[[nm]] + 1
        dup_count[i] = seen[[nm]]
      }
    }
    new_names = names_vec
    for (i in 1:length(new_names)) {
      if (dup_count[i] > 0) {
        new_names[i] = paste0(names_vec[i], ".", dup_count[i])
      }
    }
    new_names
  }
  colnames(data) = normalize_colnames(colnames(data))

  # ---- Strip known extensions from path_out ----
  path_out = gsub("[.]csv$|[.]RDS$|[.]db$|[.]sqlite$|[.]xlsx$", "", path_out, ignore.case = TRUE)

  # ---- Derive table/sheet name ----
  if (is.na(table_nm)) table_nm = basename(path_out)
  sheet_name = substr(table_nm, 1, 30)

  # ---- Helpers ----
  # Build base path for a given format, with optional subdir creation
  make_path = function() { if (use_subdirs) file.path(path_out, basename(path_out)) else path_out}

  # Safe writer wrapper
  safe_write = function(expr, fmt_name) {
    tryCatch(
      {
        value = eval(expr)
        list(success = TRUE, error = NULL)
      },
      error = function(e) {
        if (stop_on_error) stop(sprintf("[%s] %s", fmt_name, conditionMessage(e)), call. = FALSE)
        list(success = FALSE, error = conditionMessage(e))
      },
      warning = function(w) {
        # Treat warnings as non-fatal; still mark success but attach warning text
        invokeRestart("muffleWarning")
      }
    )
  }

  # ---- Outputs ----
  results = list(csv = NULL, rds = NULL, sqlite = NULL, xlsx = NULL)

  #
  base_path = make_path()

  # ---- CSV ----
  if (isTRUE(do_csv) && "csv" %in% names(extensions) && !is.na(extensions[["csv"]])) {
    path_out_csv = file_version(paste0(base_path, extensions[["csv"]]), increment = increment)
    res = safe_write(quote(write.csv(data, path_out_csv, row.names = row.names)), "csv")
    res$path = if (isTRUE(res$success)) path_out_csv else NULL
    results$csv = res
  }

  # ---- RDS ----
  if (isTRUE(do_rds) && "rds" %in% names(extensions) && !is.na(extensions[["rds"]])) {
    path_out_rds = file_version(paste0(base_path, extensions[["rds"]]), increment = increment)
    res = safe_write(quote(saveRDS(data, path_out_rds)), "rds")
    res$path = if (isTRUE(res$success)) path_out_rds else NULL
    results$rds = res
  }

  # ---- SQLite (sf only) ----
  if (isTRUE(do_sqlite) && "sqlite" %in% names(extensions) && !is.na(extensions[["sqlite"]])) {
      # accept .db or .sqlite in extensions
      path_out_sql = file_version(paste0(base_path, extensions[["sqlite"]]), increment = increment)
      res = safe_write(quote(sf::st_write(data, path_out_sql, table_nm, driver = "SQLite", delete_dsn = FALSE)), "sqlite")
      res$path = if (isTRUE(res$success)) path_out_sql else NULL
      results$sqlite = res
  }

  # ---- XLSX ----
  if (isTRUE(do_xlsx) && "xlsx" %in% names(extensions) && !is.na(extensions[["xlsx"]])) {
    browser()
    if (!have_openxlsx) {
      msg = "Package `openxlsx` not available for XLSX export."
      if (stop_on_error) stop(paste0("[xlsx] ", msg), call. = FALSE)
      results$xlsx = list(success = FALSE, path = NULL, error = msg)
    } else {
      path_out_xlsx = file_version(paste0(base_path, extensions[["xlsx"]]), increment = increment, purge_missing_versions = T)
      res = safe_write(quote(openxlsx::write.xlsx(data, file = path_out_xlsx, sheetName = sheet_name, rowNames = row.names)), "xlsx")
      res$path = if (isTRUE(res$success)) path_out_xlsx else NULL
      results$xlsx = res
    }
  }

  openxlsx::write.xlsx(as.data.frame(data), file = path_out_xlsx, sheetName = sheet_name, rowNames = row.names)

  invisible(results)
}
