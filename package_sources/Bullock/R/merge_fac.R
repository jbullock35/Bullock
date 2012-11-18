# merge_fac.R
# created 2011 July 17

merge_fac <- function(fac.names, ...) {
  # This function takes a vector of factor names.  All factors must be of the 
  # same length.  Missing values in the first factor are filled in with 
  # corresponding values from the second factor.  Missing values in this 
  # merged factor are filled in with corresponding values from the third 
  # factor.  And so on.  [2011 07 17]
  
  stopifnot('character'%in%class(fac.names))
  if (length(fac.names)<=1) { 
    print('Exiting: fac.names doesn\'t have more than one element, so there is nothing to do.')
    return()
  }
  newfac <- get(fac.names[1], ...)
  x <- 2
  for (i in 2:length(fac.names)) {
    nextfac <- get(fac.names[i], ...)
    levels(newfac) <- unique(c(levels(newfac), levels(nextfac)))  # merge factor levels
    newfac[is.na(newfac)] <- nextfac[is.na(newfac)]
  }
  return(newfac)
}



