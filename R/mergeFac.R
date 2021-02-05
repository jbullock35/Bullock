#' Fill in missing values of one factor with corresponding values from another.
#' 
#' Given a factor of length \code{n} that contains some missing values, fill
#' in the missing values with the corresponding values of others factors.
#'
#' \code{x} and the factors named in \code{otherFactors} must all have the 
#' same length. If they do, missing values in \code{x} will be filled in with
#' the corresponding values of the first factor in \code{otherFactors}. If 
#' the corresponding values of that factor are also missing, \code{mergeFac()}
#' will look to the corresponding values of the next factor, and so{\NB}on.

#' @note
#' \code{mergeFac()} is deprecated. You should use \code{dplyr::coalesce()} 
#' instead. No problems with \code{mergeFac()} have been reported, but 
#' \code{dplyr::coalesce()} does all that \code{mergeFac()} does, is more 
#' powerful, and will be just as well maintained.
#' 
#' Merging factors as \code{mergeFac()} does is trickier than just using a 
#' command like \code{fac1[is.na(fac1)] <- fac2[is.na(fac1)]} because 
#' \code{fac1} and \code{fac2} may have different factor levels.  This 
#' command takes care of the problem by merging the levels among different 
#' factors.

#' @param x Factor variable.
#' @param ... Other factor variables.

#' @examples 
#' fac1 <- factor(c("a", NA,  "b", NA,  NA))
#' fac2 <- factor(c("y", "y", "y", NA,  NA))
#' fac3 <- factor(c(NA,  "z", "z", "z", NA))
#' mergeFac(fac1, fac2)        # [1] a    y    b    <NA> <NA>
#' mergeFac(fac1, fac2, fac3)  # [1] a    y    b    z    <NA>

#' @export 
mergeFac <- function(x, ...) {
  .Deprecated(msg = "  mergeFac() is deprecated and will be removed from the Bullock\n  package sometime after February 2022. Use dplyr::coalesce() instead.")
  stopifnot('factor' %in% class(x))
  otherFactors <- list(...)
  if (! all(sapply(otherFactors, function (x) 'factor' %in% class(x))) ) {
    stop("Every object passed to mergeFac() must be a factor.")
  }
  if (! all(lengths(otherFactors) == length(x))) {
    stop("All factors must have the same length.")
  }
  newFac <- x
  for (i in seq_along(otherFactors)) {
    replacement <- otherFactors[[i]]
    levels(newFac) <- unique(c(levels(newFac), levels(replacement)))  # merge factor levels
    newFac[is.na(newFac)] <- replacement[is.na(newFac)]
  }
  droplevels(newFac)  # drop unused levels
}
