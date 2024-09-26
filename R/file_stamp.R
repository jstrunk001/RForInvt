#'@title
#'  create a date based version of a file name, mostly a wrapper for timestamp
#'
#'@description
#'  create a date based version of a file name leveraging timestamp function and data format codes
#'
#'@details
#'
#' create a date based version of a file name file name leveraging existing timestamp function and data format codes
#' #'
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
#'1.0 \tab Implemented  \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#'@param path  location for file, defaults to tempfile()
#'@param stamp  how to label file - default is a custom format time
#'@param prefix  some sort of label like function name or analysis
#'@param suffix  file extension or other details
#'@param quiet  If TRUE, suppress printing file_stamp to the console
#'
#'@return
#'  character string with some new d
#'
#'@examples
#'
#' file_stamp()
#' file_stamp(format(Sys.time(), "%Y_%m_%d_%H%M%S"))
#' file_stamp("1.1")
#' file_stamp("V1.1",prefix = "c:/temp/project1/myfile_",suffix=".RDS")
#'
#'
#'@export
#
#'@seealso \code{\link{timestamp}}\cr \code{\link{format}}\cr  \code{\link{Sys.time}}\cr \code{\link{tempfile}}\cr

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
#
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


#rename function and add guts
file_stamp = function(
                      path = tempdir()
                      ,stamp = format(Sys.time(), "%Y%m%d%H%M%S")
                      ,prefix = "logfile_V"
                      ,suffix = ".txt"
                      ,quiet = T
                      )
{
  file.path(path, timestamp(stamp=stamp, prefix=prefix,suffix = suffix, quiet=quiet))
}


# file_stamp()
# file_stamp(format(Sys.time(), "%Y_%m_%d_%H%M%S"))
# file_stamp("1.1")
# file_stamp("V1.1",prefix = "c:/temp/project1/myfile_",suffix=".RDS")

