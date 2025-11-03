#'@title
#'  Archive a data object to multiple formats
#'
#'@description
#'  Saves a given data object to specified formats (CSV, RDS, SQLite, XLSX) with versioned filenames.
#'
#'@details
#'
#'  - CSV and RDS files are written using `write.csv()` and `saveRDS()`.
#'  - XLSX export uses `openxlsx::write.xlsx()` with sheet name = `table_nm`.
#'  - SQLite export uses `sf::st_write()` (requires `data` to be an `sf` object).
#'  - Filenames are versioned using `file_version()`.
#'  - If `use_subdirs = TRUE`, files are placed in subdirectories named after their format (e.g., `csv/`, `rds/`).
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
#'1.4 \tab 2025-11-03 Added `use_subdirs` argument for organizing outputs. \cr
#'1.3 \tab 2025-11-03 Added internal argument type checks. \cr
#'}
#'
#'@author
#'  Jacob Strunk <your.email@domain.com>
#'
#'@param data Data frame or `sf` object to archive.
#'@param path_out Character. Base path (without extension) for output files.
#'@param extensions Named character vector of file extensions. Defaults to
#'   `c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx")`.
#'@param table_nm Character. Table name for SQLite export and sheet name for XLSX export (default = `"some_table"`).
#'@param do_csv Logical. If `TRUE`, write CSV file (default = `TRUE`).
#'@param do_rds Logical. If `TRUE`, write RDS file (default = `TRUE`).
#'@param do_sqlite Logical. If `TRUE`, write SQLite file (default = `TRUE`).
#'@param do_xlsx Logical. If `TRUE`, write XLSX file (default = `TRUE`).
#'@param increment Logical. If `TRUE`, increment version numbers in filenames (default = `TRUE`).
#'@param use_subdirs Logical. If `TRUE`, create subdirectories for each format (default = `FALSE`).
#'
#'@return Invisibly returns a list of file paths created.
#'
#'@examples
#'  \dontrun{
#'    # Archive all formats in subdirectories
#'    archive_table(mtcars, "output/mydata", use_subdirs = TRUE)
#'  }
#'
#'@importFrom sf st_write
#'@importFrom openxlsx write.xlsx
#'@export
archive_table <- function(data,
                          path_out,
                          extensions = c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx"),
                          table_nm = "r_data",
                          do_csv = TRUE,
                          do_rds = TRUE,
                          do_sqlite = TRUE,
                          do_xlsx = TRUE,
                          increment = TRUE,
                          use_subdirs = FALSE
                          ) {

  requireNamespace("sf", quietly = TRUE)
  requireNamespace("openxlsx", quietly = TRUE)

  # ---- Internal Argument Checks ----
  stopifnot(is.data.frame(data) || inherits(data, "sf"))
  stopifnot(is.character(path_out), length(path_out) == 1)
  stopifnot(is.character(table_nm), length(table_nm) == 1)
  stopifnot(is.logical(do_csv), is.logical(do_rds), is.logical(do_sqlite), is.logical(do_xlsx), is.logical(increment), is.logical(use_subdirs))
  stopifnot(is.character(extensions), !is.null(names(extensions)))

  # Strip known extensions
  path_out <- gsub("[.]csv$|[.]RDS$|[.]db$|[.]xlsx$", "", path_out, ignore.case = TRUE)

  out_files <- list()

  # Helper to build path with optional subdir
  make_path <- function(fmt) {
    dir_path <- if (use_subdirs) file.path(dirname(path_out), fmt) else dirname(path_out)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    file.path(dir_path, basename(path_out))
  }

  # CSV
  if (do_csv && "csv" %in% names(extensions) && !is.na(extensions[["csv"]])) {
    base_path <- make_path("csv")
    path_out_csv <- file_version(paste0(base_path, extensions[["csv"]]), increment = increment)
    write.csv(data, path_out_csv, row.names=F)
    out_files$csv <- path_out_csv
  }

  # RDS
  if (do_rds && "rds" %in% names(extensions) && !is.na(extensions[["rds"]])) {
    base_path <- make_path("rds")
    path_out_rds <- file_version(paste0(base_path, extensions[["rds"]]), increment = increment)
    saveRDS(data, path_out_rds)
    out_files$rds <- path_out_rds
  }

  # SQLite
  if (do_sqlite && "sqlite" %in% names(extensions) && !is.na(extensions[["sqlite"]])) {
    base_path <- make_path("sqlite")
    path_out_sql <- file_version(paste0(base_path, extensions[["sqlite"]]), increment = increment)
    sf::st_write(data, path_out_sql, table_nm, driver = "SQLite")
    out_files$sqlite <- path_out_sql
  }

  # XLSX
  if (do_xlsx && "xlsx" %in% names(extensions) && !is.na(extensions[["xlsx"]])) {
    base_path <- make_path("xlsx")
    path_out_xlsx <- file_version(paste0(base_path, extensions[["xlsx"]]), increment = increment)
    openxlsx::write.xlsx(data, file = path_out_xlsx, sheetName = table_nm, rowNames = FALSE)
    out_files$xlsx <- path_out_xlsx
  }

  invisible(out_files)
}
