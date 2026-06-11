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
#'  logical vector flagging duplicated elements
#'
#'@examples
#'  x <- c("a", "b", "a", "c", "b")
#'
#'  # base duplicated() flags only the 2nd+ occurrence
#'  duplicated2(x, all = FALSE)
#'
#'  # all = TRUE flags every element that has a duplicate
#'  duplicated2(x, all = TRUE)
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

