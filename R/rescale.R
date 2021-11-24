#' Rescale a vector to have a specified minimum and maximum.
#' 
#' Rescale a vector to have a specified minimum and maximum.
#'
#' @param x Numeric object.
#' @param newRange Numeric vector of length{\NB}2.
#' 
#' @examples 
#' vec <- 1:10
#' rescale(vec, c(2, 5))
#' 
#' @author 
#' Simon D. Jackman
#' 
#' @seealso
#' \linkInt{scale} and \code{\link[scales:rescale]{scales::rescale()}}
#' 
#' @export 
rescale <- function(x, newRange = c(0, 1)) {

  if (all(is.na(x))) { 
    stop("Can't rescale x; it has only missing values.")
  }
  
  
  # if newRange has max first, reverse it
  if(newRange[1] > newRange[2]) {
    newMin      <- newRange[2]
    newRange[2] <- newRange[1]
    newRange[1] <- newMin
  }

  xRange <- range(x, na.rm = TRUE)
  if(xRange[1] == xRange[2]) stop("can't rescale a constant vector!")
  mfac <- (newRange[2] - newRange[1]) / (xRange[2] - xRange[1])
  newRange[1] + (x-xRange[1])*mfac
}
