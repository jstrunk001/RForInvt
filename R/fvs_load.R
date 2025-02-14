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
#'
#' #this brings in functions to run FVS inside R
#'  fvs_load()
#'
#'
#'
#'@export
#
#'@seealso \code{\link{fvs_run}}\cr \code{\link{fvs_make_keyfiles}}\cr \code{\link{fvs_keyfile_prototype}}\cr \code{\link{fvs_param_prototype}}\cr
#'
fvs_load=function(
                        webpath = c("https://sourceforge.net/code-snapshots/svn/o/op/open-fvs/code/open-fvs-code-r3840-rFVS-R.zip")
                       , package = "RForBio"
                       , force_update = F
                       ){

  #find location of package
  dir_pkg = find.package(package)
  dir_rfvs = file.path(dir_pkg,"rfvs")
  #dir_temp = file.path(dir_rfvs,"temp")
  zips = sapply(dir_rfvs, list.files , pattern="[.]zip", full.names=T)

  #create rfvs directory if needed
  res_dir = sapply(dir_rfvs,function(x)if(!dir.exists(x)) dir.create(x))

  #see if zip files present
  if(length(zips)>0){
    zip_time = file.mtime(zips)
    max_zip = zips[which.max(zip_time)]
  }

  #download files to package directory if necessary
  if(length(zips)==0 | force_update){
    if(!dir.exists(dir_rfvs)) dir.create(dir_rfvs)
    file_path = download.file(webpath, dir_rfvs)
  }
  #find most recent zip
  zips = sapply(dir_rfvs, list.files , pattern="[.]zip", full.names=T)
  zip_time = file.mtime(zips)
  max_zip = zips[which.max(zip_time)]

  #source files
  if(length(max_zip)==1){
    files_zip = file.path(dir_rfvs,unzip( max_zip , list=TRUE)$Name[-1])
    files_unzip = unzip(max_zip, exdir= file.path(dir_rfvs) )
    res_src = sapply(files_unzip,source)
    res_ulink = unlink(dirname(files_unzip[1]),recursive=T)
  }
  cat("completed load of rRFVS functions /n")
}


