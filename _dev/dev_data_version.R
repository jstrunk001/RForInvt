#'@title
#'
#'  Create versions of data in a directory
#'
#'@description
#'
#'  places files in a directory that looks like a file
#'
#'  e.g.
#'    "c:/temp/data.txt" is a folder
#'    "c:/temp/data.txt/data._VS_0001_DT_20230530_152025.txt" is a version of the data in this folder
#'
#'
#'@details
#'
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
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#' @param path name of data file
#' @param increment T/F should function increment to new version
#' @param version_markers (optional)
#' @param version_sep  (optional)
#' @param version_digits  (optional)
#' @param version_stamp  (optional)
#' @param return_all_versions  (optional)
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'    #continue revisions on same data file
#'      vs_test1 = file_version("c:/temp/dataNoIncrement.txt" , increment=F); vs_test1
#'      writeLines(letters,vs_test1)
#'
#'    #update version every time
#'      vs_test2 = file_version("c:/temp/dataIncrement.txt" , increment=T); vs_test2
#'      writeLines(letters,vs_test2)
#'
#'@import some_package some_package2
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#the code below with a single comment symbol is not part of the official roxygen code
#
#Desired upgrades to this function (keep track of planned updates):
#1.
#2.
#3.
#4.
#

#Help to set up param documentation above
# 1. Select function parameters, in this example case just "x,y,...", arguments should be on separate line
# 2. select the code starting with writeClipboard(...) below without comment (hash symbol) and run
# 3. paste parameters into @param section above


file_version = function(
      path
    , increment = F

    , version_markers = c(version="VS",date="DT")
    , version_sep = "_"
    , version_digits= 4
    , version_stamp = format(Sys.time(), "%Y%m%d_%H%M%S")

    , return_all_versions = F

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
      version_in = .version_get( path , increment = increment, digits = version_digits ,  stamp = version_stamp , markers = version_markers , sep = version_sep , all_versions = return_all_versions)

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



   #continue revisions on same data file
     vs_test1 = file_version("c:/temp/data.txt" , increment=F); vs_test1
     writeLines(letters,vs_test1)
