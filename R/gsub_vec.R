#'@title
#'  gsub using a vector or pattern and replacement values
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/1/2020 created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'@param pattern vector of patterns
#'@param replacement vector of replacements
#'@param ... all other values as described in gsub
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@export
#
#'@seealso \code{\link{gsub}}\cr
#'



gsub_vec = function(pattern,replacement, x ,  ...){

	for(i in 1:length(pattern)){
		x = gsub( pattern[i] , replacement[i], x ,  ...)
	}
	x
}
