#'@title
#'  create a date versioned file name
#'
#'@description
#'  create a time stamp for use in file naming
#'
#'@details
#'
#' create a time stamp for use in file naming
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
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import some_package some_package2
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

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
fileStamp = function(
                      stamp = format(Sys.time(), "%Y%m%d%H%M%S")
                      ,prefix = "c:/temp/this_file_V"
                      ,suffix = ".csv"
                      ,quiet = FALSE
                      )
{
  timestamp(stamp=stamp, prefix=prefix,suffix = suffix, quiet=quiet)
}


fileStamp()


