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
#' These clipboard helpers (\code{bs}, \code{fs}, \code{write_clipboard}) are
#' Windows-only: they call \code{utils::readClipboard}/\code{writeClipboard},
#' which exist only in Windows builds of R. On other platforms \code{bs()}/
#' \code{fs()} error unless you pass \code{x} explicitly, and
#' \code{write_clipboard} warns and does nothing. \code{nms_vec} works
#' everywhere (it just can't copy to the clipboard off Windows).
#'
#'@examples
#'
#' #nms_vec returns a quoted column-name vector (and copies it on Windows)
#' dftest = as.data.frame(t(1:30))
#' nms_vec(dftest)
#'
#'\dontrun{
#' #these take a path on the clipboard (ctrl+C) and make it R compatible (Windows)
#' bs()
#' fs()
#'}
#'
#'@name helper_functions
#'@export
#
#'@seealso \code{\link{shQuote}}\cr
#'
#helper for writing paths to clipboard
write_clipboard = function(x){
  if(.Platform$OS.type != "windows"){
    warning("write_clipboard() is Windows-only; clipboard not modified")
    return(invisible(NULL))
  }
  get("writeClipboard", envir = asNamespace("utils"))(charToRaw(paste0(x, ' ')))
}

#generic function to slash direction from clipboard
reslash<- function(x=NA, slash=c("back","forward")){
  #only read the clipboard (Windows-only) when no explicit path is supplied
  if(length(x) == 1 && is.na(x)){
    if(.Platform$OS.type != "windows")
      stop("reslash()/bs()/fs() read the clipboard (Windows-only); pass x explicitly on other platforms")
    x = get("readClipboard", envir = asNamespace("utils"))()
  }
  if(slash[1]=="back") path <- shQuote(gsub("/","\\\\",gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE))
  if(slash[1]=="forward") path <- shQuote(gsub("\\", "/", x, fixed = TRUE))
  write_clipboard(path)
  return(path)
}

#'@rdname helper_functions
#'@export
#backslash a path on clipboard
bs=function(x=NA){reslash(x,slash="back")}

#'@rdname helper_functions
#'@export
#forwardslash a path on clipboard
fs=function(x=NA){reslash(x,slash="forward")}

#'@rdname helper_functions
#'@export
#get the names of a data.frame as a quoted string vector
nms_vec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep=""); write_clipboard(x);return(x)}
