#' Strip "0" from beginning of numbers printed by R.
#' 
#' Takes an object \code{x} of class "character" or "numeric". Returns an 
#' object of class "character" in which the leading zero has been removed. 
#' By default, it also rounds the number-string that it returns to two 
#' decimal places. Example: both 0.3445 and "0.3445" are transformed to 
#' ".34".
#' 
#' If \code{x} has no leading zero, the returned object is still rounded to
#' \code{dp} decimal places. 
#' 
#' @param x Object of class "character" or "numeric."
#' @param dp Integer specifying the number of decimal places in the returned string. Can be NULL, in which case no rounding is performed.
#'   
#' 
#' @examples
#' strip0("0.346")  #  ".35"
#' strip0(0.346)    #  ".35"
#' strip0(.346)     #  ".35"
#' strip0(5.346)    # "5.35"
#' 
#' strip0(c("0.789", ".2346", 53.3, 53.346))  # c(".79", ".23", "53.3", "53.35")
#' 
#' @export
strip0 <- function(x, dp = 2) {
  stopifnot(class(x)  %in% c("character", "numeric"))
  stopifnot(class(dp) %in% c("integer",   "numeric"))
  
  if (class(x) != "numeric") x <- as.numeric(x)
  if (!is.null(dp)) {
    xRounded <- round(x, dp)
  }
  
  gsub('^0\\.', '.', xRounded)
}
