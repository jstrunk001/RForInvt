#'@title
#'  Get green/dry weight factors for Region, Forest, Tree species from the National Volume Estimator Library (NVEL)
#'
#'@description
#'  Look up the NVEL green and dry weight factors (pounds per cubic foot) for each tree, given its
#'  region, forest, and USFS species code. These factors convert cubic-foot volume to biomass.
#'
#'@details
#'  Calls the NVEL `getwtfactor`-style routine in vollib.dll once per tree and returns the
#'  green (`WFGRN_LBSCFT`) and dry (`WFDRY_LBSCFT`) weight factors. Region/forest/species may be
#'  supplied either as columns of `dfTL` (via the `*Nm` arguments) or as scalar `region`/`forest`/`spcd`.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/19/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param dfTL data.frame/data.table with tree records (region, forest, species columns)
#'@param region (optional) scalar region; supercedes the dfTL regionNm column
#'@param forest (optional) scalar forest; supercedes the dfTL forestNm column
#'@param spcd (optional) scalar USFS species code; supercedes the dfTL spcdNm column
#'@param regionNm column name in dfTL holding the region
#'@param forestNm column name in dfTL holding the forest
#'@param spcdNm (required) column name in dfTL holding the USFS species code
#'@param dll_64  path to the 64-bit vollib.dll
#'@param dll_32  path to the 32-bit vollib.dll
#'@param load_dll T/F should the dll be loaded (skip if already loaded)
#'@param dll_func_wtfactor name of the weight-factor function in the NVEL .dll
#'
#'@return
#'  the input tree list (as a data.table) with two appended columns: `WFGRN_LBSCFT` (green weight
#'  factor, lbs/ft^3) and `WFDRY_LBSCFT` (dry weight factor, lbs/ft^3)
#'
#'@examples
#'\donttest{
#'  # scalar lookup
#'  NVEL_wtfactor(region = 6, forest = "01", spcd = 202)
#'
#'  # column-based lookup over a small tree list
#'  trees = data.frame(region = 6, forest = "01", spcd = c(202, 122, 242))
#'  NVEL_wtfactor(dfTL = trees, spcdNm = "spcd")
#'}
#'
#'
#
#'@seealso \code{\link{NVEL_volume}}\cr \code{\link{NVEL_biomass}}\cr

#Desired upgrades to this function:
#
#
#' @export
NVEL_wtfactor = function(
  dfTL = NULL
  , region = NA
  , forest = NA
  , spcd = NA
  , regionNm = "region"
  , forestNm = "forest"
  , spcdNm = "spcd"
  , dll_64 = system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package="RForInvt")
  , dll_32 = system.file('lib/VolLibDll20231106/vollib-32bits/vollib.dll', package="RForInvt")
  , dll_func_wtfactor = "getwtfactor_r"
  , load_dll = TRUE
){

  # 1. Ensure data.table is available
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for this function.")
  }

  # 2. Internal conversion to data.table
  # If dfTL is NULL or logical (NA), build the table from individual arguments
  if (is.null(dfTL) || is.logical(dfTL)) {
    if (any(is.na(c(region[1], forest[1], spcd[1])))) {
      warning("dfTL not provided, and missing region, forest, or spcd arguments.")
    }
    dt_internal <- data.table::data.table(
      region = region,
      forest = forest,
      spcd = spcd
    )
    regionNm <- "region"
    forestNm <- "forest"
    spcdNm <- "spcd"
  } else {
    # Convert input to data.table (creates a copy if it was a data.frame)
    dt_internal <- data.table::as.data.table(dfTL)
  }

  # 3. Load NVEL DLL
  if (load_dll) {
    .nvel_load_dll(dll_64, dll_32)
  }

  # 4. Execute DLL calls
  # Suppress portability warnings for character vectors passed to Fortran
  defaultW <- getOption("warn")
  options(warn = -1)

  # mapply logic: passing vectors extracted via [[ to the Fortran wrapper
  res_list <- mapply(
    .fn_fortran_wtf,
    region  = dt_internal[[regionNm]],
    forest  = dt_internal[[forestNm]],
    species = dt_internal[[spcdNm]],
    MoreArgs = list(dll_func_wtfactor = dll_func_wtfactor),
    SIMPLIFY = FALSE
  )

  options(warn = defaultW)

  # 5. Recombine results using data.table tools
  # rbindlist is the data.table replacement for plyr::ldply / rbind.fill
  res_dt <- data.table::rbindlist(res_list)

  # 6. Final Column Bind (Returns a data.table)
  return(cbind(dt_internal, res_dt))
}

# Wrapper for the Fortran DLL call
.fn_fortran_wtf = function(region, forest, species, dll_func_wtfactor){
  res_wf0 = .Fortran(
    dll_func_wtfactor,
    as.integer(region),
    as.character(forest),
    as.integer(species),
    as.double(0), # Output 4: Green Weight Factor
    as.double(0), # Output 5: Dry Weight Factor
    PACKAGE = "vollib"
  )
  # Return a named list which rbindlist handles efficiently
  list(WFGRN_LBSCFT = res_wf0[[4]], WFDRY_LBSCFT = res_wf0[[5]])
}

