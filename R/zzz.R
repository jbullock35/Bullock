# zzz.R
# Created on 2020-01-22
# Created by John Bullock

# I use this function to set the "Bullock.print.regTable.dp" option when the
# package is loaded. See https://r-pkgs.org/r.html for more on the .onAttach 
# and .onLoad functions.  [2020 01 22]


# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("Bullock package has been loaded.")
# }

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.Bullock <- list(
    Bullock.print.regTable.dp = 3L
  )
  toset <- !(names(op.Bullock) %in% names(op))
  if(any(toset)) options(op.Bullock[toset])

  invisible()
}