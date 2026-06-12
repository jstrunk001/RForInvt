
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
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
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
#'  #create a small temporary directory with a couple of text files to search
#'  tmp_dir <- file.path(tempdir(), "search_text_demo")
#'  dir.create(tmp_dir, showWarnings = FALSE)
#'  writeLines(c("x <- 1", "require(plyr)"),        file.path(tmp_dir, "a.R"))
#'  writeLines(c("y <- 2", "library(data.table)"),  file.path(tmp_dir, "b.R"))
#'
#'  #find the .R files (and line numbers) containing "plyr"
#'  test <- search_text(directory = tmp_dir, pattern = "[.]R$", match_string = "plyr")
#'  print(test)
#'
#'  #clean up
#'  unlink(tmp_dir, recursive = TRUE)
#'
#'@import plyr
#'
#'
#'@export
search_text=function(
  match_string=""
  ,directory=NA
  ,file=NA
  ,pattern="\\.[rR]$"
  ,pattern_omit=c(NA,"dev_")
  ,recursive=T
  ,clus=NA
){


  if(is.na(file)){

    #get files if file not provide
    files=list.files(directory,pattern=pattern,recursive=recursive,full.names=T, ignore.case=T)
    if(length(files)==0) stop("no files with the designated pattern found")
    if(!is.na(pattern_omit[1])) files=grep(pattern_omit[1],files,invert=TRUE,value=T)
    if(length(files)==0) stop("no files without the designated pattern_omit found")

    if(is.na(clus[1])){

      res=mapply(search_text,file=files,pattern=pattern,match_string=match_string,recursive=recursive,SIMPLIFY=FALSE)

    }else{

      if(is.numeric(clus)) clus_in=makeCluster(clus)
      else clus_in=clus

      #map over the FILES (the previous call never passed file=files, so every
      #worker re-entered the directory branch with file=NA and errored)
      res=clusterMap(clus_in,search_text,file=files
                     ,MoreArgs=list(pattern=pattern,match_string=match_string,recursive=recursive)
                     ,SIMPLIFY=F)
      if(is.numeric(clus)) stopCluster(clus_in)
    }

      res=rbind.fill(res)
      if(is.null(res[1])) res=read.csv(text=c("file,matches"),header=T)#empty_df(c("file","matches"))

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


