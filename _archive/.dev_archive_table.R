#'@title
#'  Archive a data object to multiple formats (robust to per-format failures)
#'
#'@description
#'  Saves a given data object to specified formats (CSV, RDS, SQLite, XLSX) with versioned filenames.
#'  Each format write is isolated so one failure does not stop others. Detailed per-format status
#'  is returned (success/path/error).
#'
#'@details
#'  - CSV and RDS files are written using base R functions.
#'  - XLSX export uses `openxlsx::write.xlsx()` with sheet name clipped to `xlsx_sheetname_max`.
#'  - SQLite export uses `sf::st_write()`; package `sf` must be available.
#'  - If `table_nm = NA`, the base name of `path_out` is used for sheet/table names.
#'  - Row names are excluded by default in CSV and XLSX (`row.names = FALSE`).
#'  - Filenames are versioned using `file_version()`. (Assumed to handle directory creation.)
#'  - If `use_subdirs = TRUE`, final base path is `<path_out>/<basename(path_out)>` via `make_path()`.
#'  - Column names are normalized for duplicate detection (case-insensitive). If duplicates exist
#'    after converting to lowercase, numeric suffixes (.2, .3, ...) are appended.
#'  - XLSX leg includes options to drop sf geometry, control list/matrix handling, and format datetimes.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.9 \tab 2025-11-03 Add XLSX options (geometry drop, list/matrix handling, TZ/format, sheetname length). \cr
#'1.8 \tab 2025-11-03 Add per-format tryCatch, stop_on_error, and robust pathing. \cr
#'1.7 \tab 2025-11-03 Added sheet name clipping (configurable) for XLSX. \cr
#'1.6 \tab 2025-11-03 Added duplicate column name handling logic. \cr
#'}
#'
#'@param data Data frame or `sf` object to archive.
#'@param path_out Character. Base path (without extension) for output files.
#'@param extensions Named character vector of file extensions. Defaults to
#'   `c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx")`.
#'@param table_nm Character or NA. Table name for SQLite and sheet name for XLSX.
#'   If `NA`, defaults to the base name of `path_out`.
#'@param do_csv,do_rds,do_sqlite,do_xlsx Logical flags to control which formats are written.
#'@param increment Logical. If `TRUE`, increment version numbers in filenames (default = `TRUE`).
#'@param use_subdirs Logical. If `TRUE`, base path becomes `<path_out>/<basename(path_out)>`.
#'@param row.names Logical. Include row names in CSV and XLSX (default = `FALSE`).
#'@param stop_on_error Logical. If `TRUE`, rethrow on first write error; otherwise collect and continue.
#'
#'@param xlsx_drop_geometry Logical. If `TRUE` and `data` is `sf`, drop geometry before Excel export (default `TRUE`).
#'@param xlsx_collapse_lists Character. How to handle list-columns for Excel:
#'   `"auto"` (default), `"comma"`, `"json"`, `"unlist"`, or `"error"`.
#'@param xlsx_matrix_collapse Character. How to handle matrix-columns row-wise:
#'   `"comma"` (default), `"json"`, or `"error"`.
#'@param xlsx_time_tz Character. TZ to use when converting POSIXlt → POSIXct for Excel. `""` keeps original TZ.
#'@param xlsx_datetime_format Character or `NA`. If non-`NA`, apply an Excel number format
#'   (e.g., `"yyyy-mm-dd hh:mm"`) to POSIXct columns.
#'@param xlsx_sheetname_max Integer. Maximum sheet name length (default `30`).
#'
#'@return Invisibly returns a named list per format with `success`, `path` (if any), and `error` (if any).
#'
#'@importFrom sf st_write st_drop_geometry
#'@importFrom openxlsx write.xlsx loadWorkbook createStyle addStyle saveWorkbook
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
                         stop_on_error = FALSE,
                         # ---- XLSX options ----
                         xlsx_drop_geometry = TRUE,
                         xlsx_collapse_lists = "auto",    # auto|comma|json|unlist|error
                         xlsx_matrix_collapse = "comma",  # comma|json|error
                         xlsx_time_tz = "",
                         xlsx_datetime_format = NA_character_,
                         xlsx_sheetname_max = 30) {

  suppressWarnings({
    have_sf = requireNamespace("sf", quietly = TRUE)
    have_openxlsx = requireNamespace("openxlsx", quietly = TRUE)
  })

  table_nm = table_nm[1]

  # ---- Checks ----
  if (!is.data.frame(data) && !inherits(data, "sf")) stop("`data` must be a data.frame or an `sf` object.")
  if (!is.character(path_out) || length(path_out) != 1) stop("`path_out` must be a single character scalar.")
  if (!is.logical(do_csv) || !is.logical(do_rds) || !is.logical(do_sqlite) || !is.logical(do_xlsx) ||
      !is.logical(increment) || !is.logical(use_subdirs) || !is.logical(stop_on_error)) {
    stop("Flags do_* , increment, use_subdirs, stop_on_error must be logical.")
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

  # ---- Names: table/sheet ----
  if (is.na(table_nm)) table_nm = basename(path_out)
  sheet_name = substr(table_nm, 1, max(1L, as.integer(xlsx_sheetname_max)))

  # ---- Results container ----
  results = list(csv = NULL, rds = NULL, sqlite = NULL, xlsx = NULL)

  # ---- Your requested make_path() (no fmt arg) ----
  make_path = function() { if (use_subdirs) file.path(path_out, basename(path_out)) else path_out }

  # ---- Safe writer ----
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
        invokeRestart("muffleWarning")
      }
    )
  }

  # ---- XLSX preparation helpers ----
  .collapse_vec = function(x, how) {
    if (how == "comma") return(paste(x, collapse = ","))
    if (how == "json")  return(paste0("c(", paste(x, collapse = ", "), ")"))
    stop("Unsupported collapse method: ", how)
  }

  .prepare_for_xlsx = function(df,
                               drop_geometry,
                               collapse_lists,
                               matrix_collapse,
                               time_tz) {
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
        df[[i]] = apply(x, 1, function(r) .collapse_vec(r, matrix_collapse))
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
          else df[[i]] = vapply(x, function(el) .collapse_vec(el, "comma"), character(1))
          next
        }

        if (collapse_lists == "comma" || collapse_lists == "json") {
          df[[i]] = vapply(x, function(el) .collapse_vec(el, collapse_lists), character(1))
          next
        }

        stop("Unsupported xlsx_collapse_lists: ", collapse_lists)
      }

      # Non-atomic safeguard
      if (!is.atomic(x)) df[[i]] = as.character(x)
    }

    # Keep column name length safe for Excel
    names(df) = substr(names(df), 1, 31)

    df
  }

  # ======================
  # ====  CSV WRITE  ====
  # ======================
  if (isTRUE(do_csv) && "csv" %in% names(extensions) && !is.na(extensions[["csv"]])) {
    base_path = make_path()
    target = paste0(base_path, extensions[["csv"]])
    path_out_csv = file_version(target, increment = increment)
    res = safe_write(quote(utils::write.csv(data, path_out_csv, row.names = row.names)), "csv")
    res$path = if (isTRUE(res$success)) path_out_csv else NULL
    results$csv = res
  }

  # =====================
  # ====  RDS WRITE  ====
  # =====================
  if (isTRUE(do_rds) && "rds" %in% names(extensions) && !is.na(extensions[["rds"]])) {
    base_path = make_path()
    target = paste0(base_path, extensions[["rds"]])
    path_out_rds = file_version(target, increment = increment)
    res = safe_write(quote(saveRDS(data, path_out_rds)), "rds")
    res$path = if (isTRUE(res$success)) path_out_rds else NULL
    results$rds = res
  }

  # =========================
  # ====  SQLITE WRITE  ====
  # =========================
  if (isTRUE(do_sqlite) && "sqlite" %in% names(extensions) && !is.na(extensions[["sqlite"]])) {
    if (!have_sf) {
      msg = "Package `sf` not available for SQLite export."
      if (stop_on_error) stop(paste0("[sqlite] ", msg), call. = FALSE)
      results$sqlite = list(success = FALSE, path = NULL, error = msg)
    } else {
      base_path = make_path()
      target = paste0(base_path, extensions[["sqlite"]])
      path_out_sql = file_version(target, increment = increment)
      # No class checks on `data` here; user requested
      res = safe_write(quote(sf::st_write(data, path_out_sql, table_nm, driver = "SQLite", delete_dsn = FALSE)), "sqlite")
      res$path = if (isTRUE(res$success)) path_out_sql else NULL
      results$sqlite = res
    }
  }

  # ======================
  # ====  XLSX WRITE  ====
  # ======================
  if (isTRUE(do_xlsx) && "xlsx" %in% names(extensions) && !is.na(extensions[["xlsx"]])) {
    if (!have_openxlsx) {
      msg = "Package `openxlsx` not available for XLSX export."
      if (stop_on_error) stop(paste0("[xlsx] ", msg), call. = FALSE)
      results$xlsx = list(success = FALSE, path = NULL, error = msg)
    } else {
      base_path = make_path()
      target = paste0(base_path, extensions[["xlsx"]])
      path_out_xlsx = file_version(target, increment = increment)

      data_xlsx = .prepare_for_xlsx(df = data,
                                    drop_geometry = isTRUE(xlsx_drop_geometry),
                                    collapse_lists = xlsx_collapse_lists,
                                    matrix_collapse = xlsx_matrix_collapse,
                                    time_tz = xlsx_time_tz)

      res = safe_write(
        quote(openxlsx::write.xlsx(data_xlsx, file = path_out_xlsx,
                                   sheetName = sheet_name, rowNames = row.names)),
        "xlsx"
      )

      # Optional: style datetime columns
      if (isTRUE(res$success) && !is.na(xlsx_datetime_format)) {
        wb = openxlsx::loadWorkbook(path_out_xlsx)
        posix_cols = integer(0)
        for (j in 1:ncol(data_xlsx)) if (inherits(data_xlsx[[j]], "POSIXct")) posix_cols = c(posix_cols, j)
        if (length(posix_cols) > 0) {
          st = openxlsx::createStyle(numFmt = xlsx_datetime_format)
          nrows = nrow(data_xlsx) + 1
          for (c in posix_cols) {
            openxlsx::addStyle(wb, sheet = sheet_name, style = st,
                               rows = 2:nrows, cols = c, gridExpand = TRUE, stack = TRUE)
          }
          openxlsx::saveWorkbook(wb, path_out_xlsx, overwrite = TRUE)
        }
      }

      res$path = if (isTRUE(res$success)) path_out_xlsx else NULL
      results$xlsx = res
    }
  }

  invisible(results)
}
``
