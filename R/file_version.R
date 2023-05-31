#'@title
#'
#'  Create versioned file names for data
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
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#' @param path name of data file
#' @param increment T/F should function increment to new version
#' @param note (optional) string giving details of increment - only with increment =T
#' @param return_all_versions (optional) return a data.frame with all versions and details
#'
#' @param DEFAULTS: (the following are defaults and wouldn't usually be changed)
#' @param purge_missing_versions T/F Delete version information if there is no accompanying files
#' @param version_markers (optional)
#' @param version_sep  (optional)
#' @param version_digits  (optional)
#' @param version_stamp  (optional)
#'
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
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

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
    , version_markers = c(version="VS",date="DT")
    , version_sep = "_"
    , version_digits= 4
    , version_stamp = format(Sys.time(), "%Y%m%d_%H%M%S")

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
                               )

    # return version
      return(version_in)

}

.version_name = function( path_file , v_num="1", digits = 4 ,  stamp="some_date" , markers =c(version="VS",date="DT") , sep ="_"){

  ext_in = tools::file_ext(path_file)

  #prepare version
  version_in = paste0(sep,markers[["version"]],sep , stringr::str_pad(v_num, width = digits, pad= "0"), sep, markers[["date"]],sep,as.character(stamp))

  #make path
  file_in = gsub(gsub(".","[.]",paste0(ext_in,"$"),fixed=T) , "", basename(path_file))
  paste0(path_file,"/",file_in,version_in,".", ext_in)

}

#.version_name("c:/temp/test_version.txt")
#deprecated, not used
.version_get = function( path_file ,  increment , digits = 4 ,  stamp="some_date" , markers =c(version="VS",date="DT") , sep ="_" , all_versions = F){

  ext_in = tools::file_ext(path_file)

  #figure version length
  version_01 = .version_name(path_file, "1", digits = 4 ,  stamp , markers , sep  )
  vs_len = nchar(version_01)

  #get all versioned files
  files_vs = list.files( path_file , pattern = ext_in , full.names=T)

  #get file base names minus extensions
  if(length(files_vs) > 0){

    #get list of files
    basenames_vs = gsub(gsub(".","[.]",paste0(ext_in,"$"),fixed=T) , "", files_vs)

    #get versions / ids from file names
    ids_in = as.numeric(gsub(paste0(".*",markers[["version"]],sep,"|",sep,markers[["date"]],".*"),"", basenames_vs ))
    id_max = which.max(ids_in)
    version_in = files_vs[id_max]

    #make increment id if needed
    if(increment){
      version_in = .version_name(path_file, id_max + 1, digits = 4 ,  stamp , markers , sep  )
    }

  }

  #make first version of files
  if(length(files_vs)==0) version_in =  version_01

  if(all_versions) version_in = list(this_version = version_in, all_versions = files_vs)

  #return file with version
  return(version_in)


}

.tracking_get = function( path_file , version,   increment , note , digits = 4 , stamp , markers  , sep , all_versions , purge ){

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

if(T){

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
  if(T){

    #update version every time
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T); vs_test2
    writeLines(letters,vs_test2)
    file_version("c:/temp/dataIncrement.txt" , increment=F , return_all_versions = T , purge_missing_versions = T)
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T); vs_test2
    vs_test2 = file_version("c:/temp/dataIncrement.txt" , note="new version and file every time",  increment=T, purge_missing_versions = F); vs_test2
    writeLines(letters,vs_test2)
    file_version("c:/temp/dataIncrement.txt" , increment=F , return_all_versions = T , purge_missing_versions = F)

  }

}
