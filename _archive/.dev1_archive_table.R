#' @title Archive a data object to multiple formats (robust to per-format failures)
#'
#' @description
#' Saves a given data object to specified formats (CSV, RDS, SQLite, XLSX) using
#' versioned filenames. Each format write is isolated with `tryCatch` so a failure
#' in one format does not stop others. A structured per-format result is returned.
#'
#' @details
#' * **CSV** and **RDS** are written via base R.
#' * **XLSX** uses `openxlsx::write.xlsx()` with:
#'   - Sheet name clipped to `xlsx_sheetname_max`
#'   - Optional geometry drop for `sf` objects
#'   - Controlled handling of list and matrix columns
#'   - Optional date-time column styling via Excel number formats
#' * **SQLite** uses `sf::st_write()` (no class checks; data.frame or sf are both allowed).
#' * If `table_nm = NA`, the base name of `path_out` is used for sheet/table names.
#' * Row names are excluded by default in CSV and XLSX (`row.names = FALSE`).
#' * Filenames are versioned using `file_version()` (assumed to also create paths).
#' * If `use_subdirs = TRUE`, paths resolve to `<path_out>/<basename(path_out)>.*`
#'   (the original `make_path` logic, computed once as `base_path`).
#' * Column names are normalized (case-insensitive duplicate handling) by appending
#'   `.2`, `.3`, ... to later duplicates.
#'
#' This program is free software but it is provided WITHOUT WARRANTY and with
#' ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose; you can
#' redistribute it and/or modify it under the terms of the GNU General Public
#' License as published by the Free Software Foundation; either version 2 of the
#' License, or (at your option) any later version.
#'
#' \cr
#' **Revision History**
#'
#' \tabular{ll}{
#' 1.10 \tab 2025-11-03 Refactor per-format write logic into helpers; compute base_path once. \cr
#' 1.9  \tab 2025-11-03 Add XLSX options (geometry drop, list/matrix handling, TZ/format, sheetname length). \cr
#' 1.8  \tab 2025-11-03 Add per-format tryCatch, stop_on_error, and robust pathing. \cr
#' 1.7  \tab 2025-11-03 Added sheet name clipping (configurable) for XLSX. \cr
#' 1.6  \tab 2025-11-03 Added duplicate column name handling logic. \cr
#' }
#'
#' @param data Data frame or `sf` object to archive.
#' @param path_out Character. Base path (without extension) for output files.
#' @param extensions Named character vector of file extensions.
#'   Defaults to `c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx")`.
#' @param table_nm Character or `NA`. Table name for SQLite and sheet name for XLSX.
#'   If `NA`, defaults to the base name of `path_out`.
#' @param do_csv,do_rds,do_sqlite,do_xlsx Logical flags to control which formats are written.
#' @param increment Logical. If `TRUE`, increment version numbers in filenames (default `TRUE`).
#' @param use_subdirs Logical. If `TRUE`, base path becomes `<path_out>/<basename(path_out)>`.
#' @param row.names Logical. Include row names in CSV and XLSX (default `FALSE`).
#' @param stop_on_error Logical. If `TRUE`, rethrow on first write error; otherwise collect and continue.
#'
#' @param xlsx_drop_geometry Logical. If `TRUE` and `data` is `sf`, drop geometry before Excel export (default `TRUE`).
#' @param xlsx_collapse_lists Character. How to handle list-columns for Excel:
#'   `"auto"` (default), `"comma"`, `"json"`, `"unlist"`, or `"error"`.
#' @param xlsx_matrix_collapse Character. How to handle matrix-columns row-wise:
#'   `"comma"` (default), `"json"`, or `"error"`.
#' @param xlsx_time_tz Character. Time zone when converting POSIXlt → POSIXct for Excel. `""` keeps original TZ.
#' @param xlsx_datetime_format Character or `NA`. If non-`NA`, apply Excel number format
#'   (e.g., `"yyyy-mm-dd hh:mm"`) to POSIXct columns after writing.
#' @param xlsx_sheetname_max Integer. Maximum sheet name length (default `30`).
#'
#' @return Invisibly returns a named list with elements `csv`, `rds`, `sqlite`, `xlsx`.
#' Each is either `NULL` (not requested) or a list with elements:
#'   * `success` (logical)
#'   * `path` (character file path when success)
#'   * `error` (NULL or character message)
#'
#' @examples
#' \dontrun{
#' # Basic usage (all formats if packages present)
#' archive_table(mtcars, "out/mydata")
#'
#' # CSV + RDS only
#' archive_table(mtcars, "out/mydata", do_sqlite = FALSE, do_xlsx = FALSE)
#'
#' # Strict XLSX handling with fail-fast
#' archive_table(mtcars, "out/mydata",
#'               do_csv = FALSE, do_rds = FALSE, do_xlsx = TRUE,
#'               xlsx_collapse_lists = "error", xlsx_matrix_collapse = "error",
#'               stop_on_error = TRUE)
#'
#' # Apply an Excel datetime format
#' archive_table(data.frame(t = as.POSIXct(Sys.time()), x = 1),
#'               "out/time_demo",
#'               do_csv = FALSE, do_rds = FALSE, do_xlsx = TRUE,
#'               xlsx_datetime_format = "yyyy-mm-dd hh:mm")
#' }
#'
#' @importFrom sf st_write st_drop_geometry
#' @importFrom openxlsx write.xlsx loadWorkbook createStyle addStyle saveWorkbook
#' @export
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
                         stop_on_error = FALSE,
                         # ---- XLSX options ----
                         xlsx_drop_geometry = TRUE,
                         xlsx_collapse_lists = "auto",    # auto|comma|json|unlist|error
                         xlsx_matrix_collapse = "comma",  # comma|json|error
                         xlsx_time_tz = "",
                         xlsx_datetime_format = NA_character_,
                         xlsx_sheetname_max = 30
                         ) {

  suppressWarnings({
    have_sf = requireNamespace("sf", quietly = TRUE)
    have_openxlsx = requireNamespace("openxlsx", quietly = TRUE)
  })

  table_nm = table_nm[1]

  # ---- Argument checks ----
  if (!is.data.frame(data) && !inherits(data, "sf")) stop("`data` must be a data.frame or an `sf` object.")
  if (!is.character(path_out) || length(path_out) != 1) stop("`path_out` must be a single character scalar.")
  if (!is.logical(do_csv) || !is.logical(do_rds) || !is.logical(do_sqlite) || !is.logical(do_xlsx) ||
      !is.logical(increment) || !is.logical(use_subdirs) || !is.logical(stop_on_error)) {
    stop("Flags do_* , increment, use_subdirs, stop_on_error must be logical.")
  }
  if (!is.character(extensions) || is.null(names(extensions))) stop("`extensions` must be a *named* character vector.")

  # Strip known extensions from path_out
  path_out = gsub("[.]csv$|[.]RDS$|[.]db$|[.]sqlite$|[.]xlsx$", "", path_out, ignore.case = TRUE)

  # Normalize column names (case-insensitive duplicate handling)
  colnames(data) = .normalize_colnames(names_vec = colnames(data))

  # Names: table/sheet
  if (is.na(table_nm)) table_nm = basename(path_out)
  sheet_name = substr(table_nm, 1, max(1L, as.integer(xlsx_sheetname_max)))

  # Compute base_path once (inlined make_path logic)
  base_path = if (use_subdirs) file.path(path_out, basename(path_out)) else path_out

  # Results container
  results = list(csv = NULL, rds = NULL, sqlite = NULL, xlsx = NULL)

  # CSV
  if (isTRUE(do_csv) && "csv" %in% names(extensions) && !is.na(extensions[["csv"]])) {
    results$csv = .write_csv(data        = data,
                             base_path   = base_path,
                             ext         = extensions[["csv"]],
                             row_names   = row.names,
                             increment   = increment,
                             stop_on_error = stop_on_error)
  }

  # RDS
  if (isTRUE(do_rds) && "rds" %in% names(extensions) && !is.na(extensions[["rds"]])) {
    results$rds = .write_rds(data        = data,
                             base_path   = base_path,
                             ext         = extensions[["rds"]],
                             increment   = increment,
                             stop_on_error = stop_on_error)
  }

  # SQLite (uses sf::st_write; no data class checks)
  if (isTRUE(do_sqlite) && "sqlite" %in% names(extensions) && !is.na(extensions[["sqlite"]])) {
    results$sqlite = .write_sqlite(data        = data,
                                   base_path   = base_path,
                                   ext         = extensions[["sqlite"]],
                                   table_nm    = table_nm,
                                   increment   = increment,
                                   stop_on_error = stop_on_error)
  }

  # XLSX
  if (isTRUE(do_xlsx) && "xlsx" %in% names(extensions) && !is.na(extensions[["xlsx"]])) {
    results$xlsx = .write_xlsx(data            = data,
                               base_path       = base_path,
                               ext             = extensions[["xlsx"]],
                               sheet_name      = sheet_name,
                               row_names       = row.names,
                               drop_geometry   = xlsx_drop_geometry,
                               collapse_lists  = xlsx_collapse_lists,
                               matrix_collapse = xlsx_matrix_collapse,
                               time_tz         = xlsx_time_tz,
                               datetime_format = xlsx_datetime_format,
                               increment       = increment,
                               stop_on_error   = stop_on_error)
  }

  invisible(results)
}

# ---- Helpers (after main; dot-prefixed) --------------------------------------

#' @keywords internal
.safe_write = function(expr, fmt_name, stop_on_error) {
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
      invokeRestart("muffleWarning")
    }
  )
}

#' @keywords internal
.normalize_colnames = function(names_vec) {
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
    if (dup_count[i] > 0) new_names[i] = paste0(names_vec[i], ".", dup_count[i])
  }
  new_names
}

#' @keywords internal
.collapse_vec = function(x, how) {
  if (how == "comma") return(paste(x, collapse = ","))
  if (how == "json")  return(paste0("c(", paste(x, collapse = ", "), ")"))
  stop("Unsupported collapse method: ", how)
}

#' @keywords internal
.prepare_for_xlsx = function(df,
                             drop_geometry,
                             collapse_lists,
                             matrix_collapse,
                             time_tz) {
  have_sf = requireNamespace("sf", quietly = TRUE)

  if (drop_geometry && inherits(df, "sf")) {
    if (!have_sf) stop("`sf` not available to drop geometry.")
    df = sf::st_drop_geometry(df)
  }

  df = as.data.frame(df, stringsAsFactors = FALSE)

  for (i in 1:ncol(df)) {
    x = df[[i]]

    # POSIXlt -> POSIXct
    if (inherits(x, "POSIXlt")) {
      if (is.character(time_tz) && nchar(time_tz) > 0) df[[i]] = as.POSIXct(x, tz = time_tz)
      else df[[i]] = as.POSIXct(x)
      next
    }

    # Matrix column -> collapse rowwise
    if (is.matrix(x)) {
      if (matrix_collapse == "error") stop("XLSX: matrix column encountered; set xlsx_matrix_collapse to 'comma' or 'json'.")
      df[[i]] = apply(x, 1, function(r) .collapse_vec(x = r, how = matrix_collapse))
      next
    }

    # List columns
    if (is.list(x)) {
      if (collapse_lists == "error") stop("XLSX: list column encountered; set xlsx_collapse_lists to 'auto'|'comma'|'json'|'unlist'.")
      len = vapply(x, length, integer(1))

      if (collapse_lists == "unlist") {
        if (!all(len == 1)) stop("XLSX: list column length != 1 with xlsx_collapse_lists='unlist'.")
        df[[i]] = unlist(x)
        next
      }

      if (collapse_lists == "auto") {
        if (all(len == 1)) df[[i]] = unlist(x)
        else df[[i]] = vapply(x, function(el) .collapse_vec(x = el, how = "comma"), character(1))
        next
      }

      if (collapse_lists == "comma" || collapse_lists == "json") {
        df[[i]] = vapply(x, function(el) .collapse_vec(x = el, how = collapse_lists), character(1))
        next
      }

      stop("Unsupported xlsx_collapse_lists: ", collapse_lists)
    }

    # Non-atomic safeguard
    if (!is.atomic(x)) df[[i]] = as.character(x)
  }

  # Excel-safe column name length
  names(df) = substr(names(df), 1, 31)

  df
}

#' @keywords internal
.write_csv = function(data, base_path, ext, row_names, increment, stop_on_error) {
  target = paste0(base_path, ext)
  path   = file_version(path = target, increment = increment)
  res    = .safe_write(expr = quote(utils::write.csv(data, path, row.names = row_names)),
                       fmt_name = "csv",
                       stop_on_error = stop_on_error)
  res$path = if (isTRUE(res$success)) path else NULL
  res
}

#' @keywords internal
.write_rds = function(data, base_path, ext, increment, stop_on_error) {
  target = paste0(base_path, ext)
  path   = file_version(path = target, increment = increment)
  res    = .safe_write(expr = quote(saveRDS(data, path)),
                       fmt_name = "rds",
                       stop_on_error = stop_on_error)
  res$path = if (isTRUE(res$success)) path else NULL
  res
}

#' @keywords internal
.write_sqlite = function(data, base_path, ext, table_nm, increment, stop_on_error) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    msg = "Package `sf` not available for SQLite export."
    if (stop_on_error) stop(paste0("[sqlite] ", msg), call. = FALSE)
    return(list(success = FALSE, path = NULL, error = msg))
  }
  target = paste0(base_path, ext)
  path   = file_version(path = target, increment = increment)
  # No class checks on `data` (as requested)
  res    = .safe_write(expr = quote(sf::st_write(obj = data,
                                                 dsn = path,
                                                 layer = table_nm,
                                                 driver = "SQLite",
                                                 delete_dsn = FALSE)),
                       fmt_name = "sqlite",
                       stop_on_error = stop_on_error)
  res$path = if (isTRUE(res$success)) path else NULL
  res
}

#' @keywords internal
.write_xlsx = function(data, base_path, ext,
                       sheet_name, row_names,
                       drop_geometry, collapse_lists, matrix_collapse,
                       time_tz, datetime_format,
                       increment, stop_on_error) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    msg = "Package `openxlsx` not available for XLSX export."
    if (stop_on_error) stop(paste0("[xlsx] ", msg), call. = FALSE)
    return(list(success = FALSE, path = NULL, error = msg))
  }

  target    = paste0(base_path, ext)
  path      = file_version(path = target, increment = increment)
  data_xlsx = .prepare_for_xlsx(df = data,
                                drop_geometry   = drop_geometry,
                                collapse_lists  = collapse_lists,
                                matrix_collapse = matrix_collapse,
                                time_tz         = time_tz)

  res = .safe_write(expr = quote(openxlsx::write.xlsx(x = data_xlsx,
                                                      file = path,
                                                      sheetName = sheet_name,
                                                      rowNames = row_names)),
                    fmt_name = "xlsx",
                    stop_on_error = stop_on_error)

  # Optional: apply datetime style
  if (isTRUE(res$success) && !is.na(datetime_format)) {
    wb = openxlsx::loadWorkbook(file = path)
    posix_cols = integer(0)
    for (j in 1:ncol(data_xlsx)) if (inherits(data_xlsx[[j]], "POSIXct")) posix_cols = c(posix_cols, j)
    if (length(posix_cols) > 0) {
      st = openxlsx::createStyle(numFmt = datetime_format)
      nrows = nrow(data_xlsx) + 1
      for (c in posix_cols) {
        openxlsx::addStyle(wb, sheet = sheet_name, style = st,
                           rows = 2:nrows, cols = c, gridExpand = TRUE, stack = TRUE)
      }
      openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
    }
  }

  res$path = if (isTRUE(res$success)) path else NULL
  res
}
