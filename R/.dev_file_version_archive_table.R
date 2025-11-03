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
#'  - Filenames are versioned using `RForInvt::file_version()`.
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
#'1.2 \tab 2025-11-03 Added `increment` argument for versioning. \cr
#'1.1 \tab 2025-11-03 Added binary switches for each file type. \cr
#'1.0 \tab 2025-11-03 Initial version created. \cr
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
#'
#'@return
#'  Invisibly returns a list of file paths created.
#'
#'@examples
#'
#'  \dontrun{
#'    # Archive all formats with versioning
#'    archive_table(mtcars, "output/mydata", table_nm = "CarsData", increment = TRUE)
#'
#'    # Archive only CSV and XLSX without versioning
#'    archive_table(mtcars, "output/mydata", do_rds = FALSE, do_sqlite = FALSE, increment = FALSE)
#'  }
#'
#'@importFrom sf st_write
#'@importFrom openxlsx write.xlsx
#'
#'@export
#'
#'@seealso \code{\link{file_version}}\cr
archive_table <- function(data,
                          path_out,
                          extensions = c(csv = ".csv", rds = ".RDS", sqlite = ".db", xlsx = ".xlsx"),
                          table_nm = "some_table",
                          do_csv = TRUE,
                          do_rds = TRUE,
                          do_sqlite = TRUE,
                          do_xlsx = TRUE,
                          increment = TRUE) {
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("openxlsx", quietly = TRUE)

  # Strip known extensions
  path_out <- gsub("[.]csv$|[.]RDS$|[.]db$|[.]xlsx$", "", path_out, ignore.case = TRUE)

  out_files <- list()

  # CSV
  if (do_csv && "csv" %in% names(extensions) && !is.na(extensions[["csv"]])) {
    path_out_csv <- RForInvt::file_version(paste0(path_out, extensions[["csv"]]), increment = increment)
    write.csv(data, path_out_csv)
    out_files$csv <- path_out_csv
  }

  # RDS
  if (do_rds && "rds" %in% names(extensions) && !is.na(extensions[["rds"]])) {
    path_out_rds <- RForInvt::file_version(paste0(path_out, extensions[["rds"]]), increment = increment)
    saveRDS(data, path_out_rds)
    out_files$rds <- path_out_rds
  }

  # SQLite
  if (do_sqlite && "sqlite" %in% names(extensions) && !is.na(extensions[["sqlite"]])) {
    path_out_sql <- RForInvt::file_version(paste0(path_out, extensions[["sqlite"]]), increment = increment)
    sf::st_write(data, path_out_sql, table_nm)
    out_files$sqlite <- path_out_sql
  }

  # XLSX
  if (do_xlsx && "xlsx" %in% names(extensions) && !is.na(extensions[["xlsx"]])) {
    path_out_xlsx <- RForInvt::file_version(paste0(path_out, extensions[["xlsx"]]), increment = increment)
    openxlsx::write.xlsx(data, file = path_out_xlsx, sheetName = table_nm)
    out_files$xlsx <- path_out_xlsx
  }

  invisible(out_files)
}
