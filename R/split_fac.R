# split_fac.R
# created 2011 June 16

split_fac <- function(fac, prefix=paste(deparse(substitute(NES.year.fac)), '.', sep=''), env=.GlobalEnv, ...) {
  # This function takes a factor and creates a dummy for each level of the 
  # factor in the environment of the user's choosing.  [2011 06 16]
  
  # The default prefix for the created dummy variables is the name of the 
  # factor variable, followed by a period.  [2011 06 16]
  stopifnot('factor'%in%class(fac))
  for (i in levels(fac)) {
    assign(paste(prefix, i, sep=''), fac==i, envir=env, ...)
  }
}



