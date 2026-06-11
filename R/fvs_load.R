#'@title load FVS scripts from subversion project
#'
#'@description
#'load rFVS scripts using source
#'
#'@details
#'load rFVS scripts using source
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 Oct 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param webpath where to download new package
#'@param package name of this package
#'@param force_update force download of new set of rFVS code
#'
#'@return
#'  NA
#'
#'@examples
#'\dontrun{
#' #downloads the rFVS scripts over the network and sources them into R
#' #(writes into the installed package directory)
#'  fvs_load()
#'}
#'
#'@export
#
#'@seealso \code{\link{fvs_run}}\cr \code{\link{fvs_make_keyfiles}}\cr \code{\link{fvs_prototype_keyfile}}\cr \code{\link{fvs_prototype_params}}\cr
#'
fvs_load=function(
                        webpath = c("https://sourceforge.net/code-snapshots/svn/o/op/open-fvs/code/open-fvs-code-r3840-rFVS-R.zip")
                       , package = "RForInvt"
                       , force_update = F
                       ){

  #find location of package
  dir_pkg = find.package(package)
  dir_rfvs = file.path(dir_pkg,"rfvs")

  #create rfvs directory if needed
  if(!dir.exists(dir_rfvs)) dir.create(dir_rfvs, recursive = TRUE)

  #see if zip files present (use list.files directly: sapply over a length-1
  #directory returns a list of length 1, so length(zips)==0 was never TRUE and
  #the first-run auto-download never fired)
  zips = list.files(dir_rfvs, pattern="\\.zip$", full.names=TRUE)

  #download files to package directory if necessary
  if(length(zips)==0 | force_update){
    download.file(webpath, file.path(dir_rfvs, basename(webpath)))
    zips = list.files(dir_rfvs, pattern="\\.zip$", full.names=TRUE)
  }

  #find most recent zip
  max_zip = if(length(zips) > 0) zips[which.max(file.mtime(zips))] else character(0)

  #source files
  if(length(max_zip)==1){
    files_zip = file.path(dir_rfvs,unzip( max_zip , list=TRUE)$Name[-1])
    files_unzip = unzip(max_zip, exdir= file.path(dir_rfvs) )
    res_src = sapply(files_unzip,source)
    res_ulink = unlink(dirname(files_unzip[1]),recursive=T)
  }
  cat("completed load of rRFVS functions \n")
}


