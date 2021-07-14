#' Convert a number or a string to a percentage.
#' 
#' Takes an object \code{x} of class "character" or "numeric". Returns an 
#' object of class "character" in which the number is expressed as a 
#' percentage.
#' 
#' By default, the returned string will have no decimal digits: it will be
#' "57\%", not "57.0\%". But if \code{dp} is greater than 0, the returned  
#' string will always have \code{dp} decimal places. 
#' 
#' @param x Object of class "character," "integer," or "numeric."
#' @param dp Integer specifying the number of decimal places in the returned string. 
#'
#' @return 
#' A string, e.g., "57\%".   
#' 
#' @examples
#' makePercentage(.5)            # "50%"
#' makePercentage(.555)          # "56%"
#' makePercentage(.555, dp = 4)  # "55.5000%"
#' makePercentage(5)             # "500%"
#' makePercentage(5, dp = 2)     # "500.00%"
#' 
#' makePercentage(1:3)           # "100%" "200%" "300%"
#' 
#' @export
makePercentage <- function(x, dp = 0) {
  stopifnot(inherits(x,  c("character", "integer", "numeric")))
  stopifnot(inherits(dp, c("integer",   "numeric")))
  
  if (class(x) != "numeric") x <- as.numeric(x)
  xRounded <- round(x * 100, dp)

  for (i in seq_along(xRounded)) {

    # Pad right-hand side with zeroes if there are no decimal digits
    if (dp>0 & !grepl(".", xRounded[i], fixed = TRUE)) {
      xRounded[i] <- paste0(xRounded[i], ".", paste0(rep(0, dp), collapse = ""))
    }
    
    # Pad right-hand side with zeroes if there are too few decimal digits
    else if (dp > 0) {
      nDecimals   <- nchar(sub("\\d+\\.(\\d+)", "\\1", xRounded[i]))
      xRounded[i] <- paste0(xRounded[i], paste0(rep(0, dp - nDecimals), collapse = ""))
    }
  }  
  paste0(xRounded, "%")
}
