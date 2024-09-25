#'@title
#'  compute distances between two vectors of points
#'
#'@description
#'  compute distances between two vectors of points
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
#'1.0 \tab 12/7/2023 Created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param idxy1 first vector of coordinates
#'@param idxy2 second vector of coordinates
#'@param row_id should row ids be returned
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  dat1 = data.frame(id=1:25, x=25:49, y=61:85)
#'  dat2 = data.frame(id=LETTERS[1:10], x=rnorm(10), y=runif(10))
#'  dist_vec(dat1, dat2)
#'
#'
#'@export
#
#'@seealso \code{\link{dist}}\cr \code{\link{sweep}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))
#
#various helpers:
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}

dist_vec = function(
              idxy1
              ,idxy2
              ,row_id=T
  ){

  #get distances for single vector
  if(is.null(dim(idxy1)[1])){
    if(row_id){
      di = data.frame(id=idxy2[1],apply(sweep(as.matrix(idxy2[,-1]),2,as.matrix(as.numeric(idxy1[-1]))),1,function(x)sqrt(sum(x^2))))
      names(di) = c(names(idxy2)[1],idxy1[1])
    }
    if(!row_id){
      di = data.frame(V1=apply(sweep(as.matrix(idxy2[,-1]),2,as.matrix(as.numeric(idxy1[-1]))),1,function(x)sqrt(sum(x^2))))
      names(di)[1] = idxy1[1]
    }
  }
  #get distances for two vectors
  if(!is.null(dim(idxy1)[1])){
    di = data.frame(id=idxy2[1],do.call(cbind,apply(idxy1,1,dist_vec, idxy2,row_id=F)  ))
    names(di) = c(names(idxy1)[1], idxy1[,1])
  }
  return(di)
}

if(F){
  dat1 = data.frame(id=1:25, x=25:49, y=61:85)
  dat2 = data.frame(id=LETTERS[1:10], x=rnorm(10), y=runif(10))
  dist_vec(dat1, dat2)
}
