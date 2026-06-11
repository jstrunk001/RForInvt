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
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param idxy1 first vector of coordinates
#'@param idxy2 second vector of coordinates
#'@param row_id should row ids be returned
#'
#'@return
#'  a data.frame whose first column is the id column of \code{idxy2} and whose
#'  remaining columns (one per row of \code{idxy1}, named by \code{idxy1}'s ids)
#'  hold the Euclidean distances from each \code{idxy1} point to every
#'  \code{idxy2} point
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

  #coords of the second set (numeric matrix, id column dropped)
  coords2 = as.matrix(idxy2[, -1, drop = FALSE])
  storage.mode(coords2) = "double"

  #single reference point supplied as a (named) vector: distances from it to
  #every row of idxy2. Keep coordinates numeric - the previous apply()-over-rows
  #form round-tripped mixed id+coord rows through character.
  if(is.null(dim(idxy1))){
    p = as.numeric(idxy1[-1])
    d = sqrt(rowSums(sweep(coords2, 2, p)^2))
    if(row_id){
      di = data.frame(idxy2[[1]], d, stringsAsFactors = FALSE, check.names = FALSE)
      names(di) = c(names(idxy2)[1], as.character(idxy1[1]))
    } else {
      di = data.frame(d, check.names = FALSE)
      names(di)[1] = as.character(idxy1[1])
    }
    return(di)
  }

  #two point sets: one distance column per row of idxy1, keyed by idxy2's id
  coords1 = as.matrix(idxy1[, -1, drop = FALSE])
  storage.mode(coords1) = "double"
  d_mat = sapply(seq_len(nrow(idxy1)), function(i) sqrt(rowSums(sweep(coords2, 2, coords1[i, ])^2)))
  d_mat = as.data.frame(d_mat, check.names = FALSE)
  names(d_mat) = as.character(idxy1[[1]])
  di = data.frame(idxy2[[1]], d_mat, stringsAsFactors = FALSE, check.names = FALSE)
  names(di)[1] = names(idxy1)[1]
  return(di)
}
