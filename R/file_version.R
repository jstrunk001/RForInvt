#'@title Create versioned file names for data
#'
#'@description
#'
#'  Create versioned file names for data
#'  places files in a directory that looks like a file
#'
#'  e.g.
#'    "c:/temp/data.txt" is a folder
#'    "c:/temp/data.txt/data._VS_0001_DT_20230530_152025.txt" is a version of the data in this folder
#'    "c:/temp/data.txt/data._VS_0002_DT_20230530_175025.txt is an updated version of the data in this folder
#'
#'
#'
#'@details
#'
#'  A convenient / simple way to track versions of data files. Simply
#'  provide the base file name and let this function increment in a folder
#'  based on the original file name. All parts and versions of files are visible
#'  in the folder with the original extension, but with appended versions. For
#'  most data iterations the version details are unneeded, but available easily
#'  in the folder if warranted. Just use  path = file_version("c:/somepath/somedata.csv")
#'  and then something like write.csv(data, path) ... Then to read the data,
#'  use the same approach. path = file_version("c:/somepath/somedata.csv") then
#'  dat = read.csv(path). If you make a change to the data and want to save it
#'  off, then simply path = file_version("c:/somepath/somedata.csv",increment=T)
#'  and a new dat = read.csv(path) will provide the updated version and data.
#'
#'  Previously I found myself manually creation versions, vs_here = "V1_202300530"
#'  but this was fairly unreliable, and I had references to versions scattered
#'  everywhere.
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
#'1.0 \tab 5/30/2023 created / implemented \cr
#'1.1 \tab 6/14/2023 add support for folder versioning \cr
#1.2 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#' @param path name of data file
#' @param increment (optional) T/F should function increment to new version
#' @param note (optional) string giving details of increment - only with increment =T
#' @param return_all_versions (optional) return a data.frame with all versions and details
#' @param purge_missing_versions (optional) T/F Delete version information if there is no accompanying files
#' @param version_markers (optional) names to use for version number and date in verion
#' @param version_sep  (optional) character to use in separating version components
#' @param version_digits  (optional) number of zeros to pad version id
#' @param version_stamp  (optional) data stamp format codes (or other) to use in version
#' @param internal_simple  (optional) T/F label internal object using only the version to shorten file length
#'
#'@return
#'  There are two possibilities
#'  1. a character string giving the path to a new file name
#'  2. a data.frame with all versioning details in the version tracking sheet
#'
#'@examples
#'
#'
#'  #edit on the same file over and over
#'  vs_test1 = file_version("c:/temp/dataNoIncrement.txt" , note="editing on single version", increment=F); vs_test1
#'  writeLines(letters,vs_test1)
#'  file_version("c:/temp/dataNoIncrement.txt" , increment=F , return_all_versions = T)
#'
#'
#'  #update version every time
#'  vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T); vs_test2
#'  writeLines(letters,vs_test2)
#'  file_version("c:/temp/dataIncrement.txt" , increment=F , return_all_versions = T)
#'
#'
#' #create folder version instead of file version
#' #update every time - into new folder
#' #first time
#' vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T); vs_test2
#' dir.create(vs_test2)
#' writeLines(letters,file.path(vs_test2,"letters.txt"))
#'
#' #second time, but use simple internal naming convention
#' vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T, internal_simple=T); vs_test2
#' dir.create(vs_test2)
#' writeLines(letters,file.path(vs_test2,"letters.txt"))
#'
#' #view results
#' file_version("c:/temp/FVSTest" , increment=F , return_all_versions = T , purge_missing_versions = F)
#'
#'
#'@export
#
#'@seealso \code{\link{filestamp}}\cr

#the code below with a single comment symbol is not part of the official roxygen code
#
#Desired upgrades to this function (keep track of planned updates):
#1. Add version tracking spreadsheet
# A. Enable writing a comment for each version to spreadsheet
#2. Integrate better with filestamp
#3. Add option to wipe versions
#4. Add better logic to test_increment including new note, wipe old increment, new date

file_version = function(
      path
    , increment = F
    , note = ""
    , return_all_versions = F

    , purge_missing_versions = ifelse(increment,T,F)
    , version_markers = c(version="VS",date="")
    , version_sep = "_"
    , version_digits = 3
    , version_stamp = format(Sys.time(), "%Y%m%d%H%M%S")
    , internal_simple = F

  ){

    # check if storage exists and create a new one if not
      exist_file = dir.exists(path)

    # add more checks on integrity of version archive
      #current version present
      #last version present
      #all prior versions present

    # create storage structure if does not exist
      if(!exist_file){
        #create file directory
        dir.create(path,recursive=T)

        #force creation of new version
        increment = T
      }

    # get / make current version
      #version_in = .version_get( path , increment = increment, digits = version_digits ,  stamp = version_stamp , markers = version_markers , sep = version_sep , all_versions = return_all_versions)

    # get / make / update tracking spreadsheet
      version_in = .tracking_get(
                                path_file = path
                                , increment = increment
                                , note = note
                                , digits = version_digits
                                , stamp = version_stamp
                                , markers = version_markers
                                , sep = version_sep
                                , all_versions = return_all_versions
                                , purge = purge_missing_versions
                                , nm_simple = internal_simple
                               )

    # return version
      return(version_in)

}

.version_name = function( path_file , v_num="1", digits = 4 ,  stamp="some_date" , markers =c(version="VS_",date="DT_") , sep ="_" , nm_simple = F){

  #get the file extension of the file that is to be versioned
  ext_in = tools::file_ext(path_file)

  #prepare version
  version_in = paste0(markers[["version"]] , stringr::str_pad(v_num, width = digits, pad= "0"), sep, markers[["date"]],as.character(stamp))

  #make path
  file_in = paste0(gsub(gsub(".","[.]",paste0(ext_in,"$"),fixed=T) , "", basename(path_file)),sep)
  if(nm_simple)   file_in = ""

  #make final version
  if(ext_in != "") version_return = paste0(path_file,"/",file_in,version_in,".", ext_in)
  if(ext_in == "") version_return = paste0(path_file,"/",file_in,version_in)

  #return the version
  version_return

}


.tracking_get = function( path_file , version,   increment , note , digits = 4 , stamp , markers  , sep , all_versions , purge , nm_simple ){

  #build path to trackign file
  path_trk = file.path(path_file,"_tracking.csv")

  #get tracking file, if exists
  if(file.exists(path_trk)){

    trk_in = read.csv(path_trk)

    #purge unused versions and overwrite
    if(purge){

      versions_exist = file.exists(trk_in[,"version"])
      trk_in = trk_in[versions_exist,]
      last_id = max(trk_in$id)

      write.csv(trk_in, path_trk , row.names=F)

    }

    #get previous max id
    if(nrow(trk_in) > 0 ) last_id = max(trk_in$id)
    if(nrow(trk_in) == 0 ){
      last_id = 0
    }

  }

  #initiate tracking file for new file
  if(!file.exists(path_trk)){
    increment = T
    last_id = 0
    trk_in = read.csv(text="file")
  }

  #increment if desired - must increment for new file
  if(increment){

    #update version
    this_id = last_id + 1
    this_version = .version_name(
                                path_file
                                , v_num = this_id
                                , digits = digits
                                , stamp = stamp
                                , markers = markers
                                , sep = sep
                                , nm_simple = nm_simple
                                  )

    #build record for single increment
    df_increment = data.frame(
          file = path_file
        , version = this_version
        , exist = F
        , id = this_id
        , date = stamp
        , npad = digits
        , pad = "0"
        , markerVS = markers[["version"]]
        , markerDT = markers[["date"]]
        , sep = sep
        , note = note
      )

    #combine with previous
    trk_update = plyr::rbind.fill( df_increment , trk_in )
    trk_update$exist = file.exists(trk_update$version)

    #write to file
    err = try(write.csv(trk_update, path_trk,row.names=F))
    if(class(err) == "try-error") warning("please close ", path_trk, " before creating a new version")

    #update internal tracking object
    trk_in = trk_update
  }

  #return version
  if(all_versions) return(trk_in)
  #return full versioning information
  if(!all_versions) return(trk_in[which.max(trk_in$id),"version"])

}



#Testing code
if(F){

  #reset for experimenting
  if(F){
    unlink("c:/temp/dataNoIncrement.txt",recursive = T)
    unlink("c:/temp/dataIncrement.txt",recursive = T)
  }

  if(F){

    #edit on the same file over and over
    vs_test1 = file_version("c:/temp/dataNoIncrement.txt" , note="editing on single version", increment=F); vs_test1
    writeLines(letters,vs_test1)
    file_version("c:/temp/dataNoIncrement.txt" , increment=F , return_all_versions = T)
  }
  if(F){

    #update version every time
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T); vs_test2
    writeLines(letters,vs_test2)
    file_version("c:/temp/dataIncrement.txt" , increment=F , return_all_versions = T , purge_missing_versions = T)
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T); vs_test2
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T, purge_missing_versions = F); vs_test2
    writeLines(letters,vs_test2)
    file_version("c:/temp/dataIncrement.txt" , increment=F , return_all_versions = T , purge_missing_versions = F)

  }

  #increment folder instead of file
  if(F){

    #update version every time
    vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T); vs_test2
    dir.create(vs_test2)
    writeLines(letters,file.path(vs_test2,"letters.txt"))

    vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T); vs_test2
    dir.create(vs_test2)
    writeLines(letters,file.path(vs_test2,"letters.txt"))

    file_version("c:/temp/FVSTest" , increment=F , return_all_versions = T , purge_missing_versions = F)

  }

  #only label file with version
  if(F){

    #update version every time
    vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T, internal_simple=T); vs_test2
    dir.create(vs_test2)
    writeLines(letters,file.path(vs_test2,"letters.txt"))

    vs_test2 = file_version("c:/temp/FVSTest" , note="new version and file every time",  increment=T, internal_simple=T); vs_test2
    dir.create(vs_test2)
    writeLines(letters,file.path(vs_test2,"letters.txt"))

    file_version("c:/temp/FVSTest" , increment=F , return_all_versions = T , purge_missing_versions = F)

  }


}

