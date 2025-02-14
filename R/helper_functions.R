#'@title help with path slashes and data.frame name vectors
#'
#'@description
#'helper functions - path slashes and data.frame name vectors
#'
#'@details
#'helper functions - path slashes and data.frame name vectors
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 Oct 28 Created\cr
#'1.0 \tab 2024 Oct 01 add nms_vec example, move to underscores from camel calse\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'@param x (optional) path string
#'
#'@return
#' either an R compatible path
#'
#' or, in the case of nms_vec, a vector of R compatible data.frame column names
#'
#'@examples
#'
#'#these take a path on the clipboard (ctrl+C) and makes them R compatible
#' bs()
#' fs()
#' dftest=as.data.frame(t(1:30))
#' nms_vec(dftest)
#'
#'
#'@export
#
#'@seealso \code{\link{readClipboard}}\cr \code{\link{writeClipboard}}\cr \code{\link{shQuote}}\cr
#'
#helper for writing paths to clipboard
write_clipboard = function(x) writeClipboard(charToRaw(paste0(x, ' ')))

#generic function to slash direction from clipboard
reslash<- function(x=NA, slash=c("back","forward")){
  if(is.na(x)) x = readClipboard()
  if(slash[1]=="back") path <- shQuote(gsub("/","\\\\",gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE))
  if(slash[1]=="forward") path <- shQuote(gsub("\\", "/", x, fixed = TRUE))
  write_clipboard(path)
  return(path)
}

#'@export
#backslash a path on clipboard
bs=function(x=NA){reslash(x,slash="back")}

#'@export
#forwardslash a path on clipboard
fs=function(x=NA){reslash(x,slash="forward")}

#'@export
#get the names of a data.frame as a quoted string vector
nms_vec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep=""); write_clipboard(x);return(x)}
