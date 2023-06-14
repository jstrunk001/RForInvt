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
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param x (optional) path string
#'
#'@return
#'  NA
#'
#'@examples
#'
#'#these take a path on the clipboard (ctrl+C) and makes them R compatible
#' bs()
#' fs()
#'
#'
#'@export
#
#'@seealso \code{\link{readClipboard}}\cr \code{\link{writeClipboard}}\cr \code{\link{shQuote}}\cr
#'
#helper for writing paths to clipboard
writeClipboard2 = function(x) writeClipboard(charToRaw(paste0(x, ' ')))

#generic function to slash direction from clipboard
reslash<- function(x=NA, slash=c("back","forward")){
  if(is.na(x)) x = readClipboard()
  if(slash[1]=="back") path <- shQuote(gsub("/","\\\\",gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE))
  if(slash[1]=="forward") path <- shQuote(gsub("\\", "/", x, fixed = TRUE))
  writeClipboard2(path)
  return(path)
}

#'@export
#
#backslash a path on clipboard
bs=function(x=NA){reslash(x,slash="back")}

#'@export
#
#forwardslash a path on clipboard
fs=function(x=NA){reslash(x,slash="forward")}

#'@export
#
#get the names of a data.frame as a quoted string vector
nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep=""); writeClipboard2(x);return(x)}
