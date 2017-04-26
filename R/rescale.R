rescale <- function(x, newrange = c(0, 1)) {
  # stopifnot(any(class(x) %in% c('integer', 'numeric', 'tbl')))

  # if newrange has max first, reverse it
  if(newrange[1] > newrange[2]) {
    newmin <- newrange[2]
    newrange[2] <- newrange[1]
    newrange[1] <- newmin
  }

  xrange <- range(x, na.rm = TRUE)
  if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
  mfac <- (newrange[2] - newrange[1]) / (xrange[2] - xrange[1])
  invisible(newrange[1] + (x-xrange[1])*mfac)
}
