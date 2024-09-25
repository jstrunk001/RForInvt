
#'@title
#'search for string in text files in a directory
#'

#'@description
#'
#'search for string in text files in a directory

#'@details
#'
#'search for string in text files in a directory
#'
#'\cr

#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 June 19 Created \cr
#'}
#'

#'@author
#'Jacob L Strunk

#'@param match_string string to find within searched files
#'@param directory where to search files
#'@param file single file
#'@param pattern pattern to search for files
#'@param pattern_omit pattern to omit searched files
#'@param recursive look in subfolders?
#'@param clus number of cores, or a cluster object

#'@return
#' two column dataframe like:
#'  [1] file    matches
#' <0 rows> (or 0-length row.names)
#'



#'@seealso \code{\link{makeCluster}}\cr \code{\link{list.files}}\cr  \code{\link{readLines}}\cr \code{\link{grepl}}\cr
#'

#Desired upgrades to this function:
#
#
#'@examples
#'test=search_text(directory="//Dnrfsoly107/fr_data/forest_info_1/fris3/R/functions",pattern="[.]r",pattern_omit="dev_",match_string="require(plyr)")
#'print(test)
#'test=search_text(directory="//Dnrfsoly107/fr_data/forest_info_1/fris3/R/functions",pattern="[.]r",pattern_omit="dev_",match_string="library(plyr)")
#'print(test)
#'test=search_text(directory="//Dnrfsoly107/fr_data/forest_info_1/fris3/R/functions",pattern="[.]r",pattern_omit="dev_",match_string="data.table")
#'print(test)
#'test=search_text("c:\\temp\\process_gridmetrics",pattern="2015_Wed_Jul_29_17p01p50.*[.]bat$",match_string = "46896")
#'print(test)
#'
#'@import plyr
#'
#'
#'@export
search_text=function(
  match_string=""
  ,directory=NA
  ,file=NA
  ,pattern=".r"
  ,pattern_omit=c(NA,"dev_")
  ,recursive=T
  ,clus=NA
){
  require(plyr)


  if(is.na(file)){

    #get files if file not provide
    files=list.files(directory,pattern=pattern,recursive=recursive,full.names=T, ignore.case=T)
    if(length(files)==0) stop("no files with the designated pattern found")
    if(!is.na(pattern_omit[1])) files=grep(pattern_omit[1],files,invert=TRUE,value=T)
    if(length(files)==0) stop("no files without the designated pattern_omit found")

    if(is.na(clus[1])){

      res=mapply(search_text,file=files,pattern=pattern,match_string=match_string,recursive=recursive)

    }else{

      require(parallel)
      if(is.numeric(clus)) clus_in=makeCluster(clus)
      else clus_in=clus

      res=clusterMap(clus_in,search_text,pattern=pattern,match_string=match_string,recursive=recursive,SIMPLIFY=F)
      if(is.numeric(clus)) stopCluster(clus_in)
    }

      res=rbind.fill(res)
      if(is.null(res[1])) res=empty_df(c("file","matches"))

  }else{

    text=suppressWarnings(readLines(file))
    matches_in=which(grepl(match_string,text))
    res=NULL
    if(length(matches_in)>0){
      res=data.frame(file=rep(file,length(matches_in)),matches=matches_in)
      #res[1:length(matches_in),"matches"]=matches_in
    }
  }

  return(res)
}


