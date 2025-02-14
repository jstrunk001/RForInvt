#'
#'@title
#' same as duplicated but argument to show all duplicates
#'
#'@description
#' same as duplicated but argument to show all duplicates
#'
#'@details
#' same as duplicated but argument to show all duplicates
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2023 May 12 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param ... regular arguments to duplicated
#'@param all T/F return all duplicates
#'
#'@return
#'  vector of duplicates
#'
#'@examples
#' something
#'
#'@export
#
#'@seealso \code{\link{duplicated}}\cr

duplicated2 = function(
  ...
  ,all = T
){

  if(all){

    duplicated(...) | duplicated(...,fromLast=T)

  }else{

    duplicated(...)

  }

}

