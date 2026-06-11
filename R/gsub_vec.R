#'@title
#'  gsub using a vector or pattern and replacement values
#'
#'@description
#'  Apply \code{\link{gsub}} repeatedly using parallel vectors of patterns and
#'  replacements, substituting each \code{pattern[i]} with \code{replacement[i]} in turn.
#'
#'@details
#'  The substitutions are applied sequentially in the order supplied, so a later
#'  pattern may match text introduced by an earlier replacement. \code{pattern}
#'  and \code{replacement} must have the same length. Any additional arguments in
#'  \code{...} (e.g. \code{fixed}, \code{ignore.case}, \code{perl}) are passed
#'  through to \code{\link{gsub}}.
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
#'@param replacement vector of replacements (same length as \code{pattern})
#'@param x a character vector where matches are sought
#'@param ... all other values as described in gsub
#'
#'@return
#'  a character vector the same length as \code{x} with all substitutions applied
#'
#'@examples
#'  # replace several codes with their labels in one pass
#'  x <- c("spp=DF ht=tall", "spp=WH ht=short")
#'  gsub_vec(
#'    pattern     = c("DF", "WH", "tall", "short"),
#'    replacement = c("Douglas-fir", "western hemlock", "120", "40"),
#'    x           = x
#'  )
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
