#' Suppress specific warning messages
#'
#' Sometimes R throws warning messages that we don't want to see. The base
#' \code{suppressWarnings()} function permits one to suppress warnings, but 
#' it is tricky to selectively suppress only certain warnings on the basis 
#' of a regular expression or another condition. This function allows one 
#' to do that.
#'
#' @author 
#' The function was created by Antoine Fabri ("Moody_Mudskipper"): see 
#' \url{https://stackoverflow.com/a/55182432/697473}.
#' 
#' @note 
#' Most functions in the Bullock package have camelCase names. This one does
#' not: it would ordinarily have been called \code{suppressWarnings}, but 
#' that name is taken by a function in base{\NB}R. 
#'
#'
#' @param .expr Expression to be evaluated. 
#' @param .f String or function. If a string (possibly representing a 
#'   regular expression), any warning message generated when \code{.expr} is 
#'   evaluated will be suppressed if \code{grepl{}} finds that the string
#'   matches the warning message.\cr
#'     \indent If a function, the warning message will be passed to the 
#'   function, and the function must return \code{TRUE} or \code{FALSE}. See
#'   the examples for details.
#' @param ... Additional arguments to be passed to \code{rlang::as_function()}.
#'
#'
#' @examples
#' suppress_warnings( {sqrt(-1); warning("oops", call. = FALSE)}, startsWith, "o" )
#'   # Warning message:
#'   # In sqrt(-1) : NaNs produced
#' suppress_warnings( {sqrt(-1); warning("oops", call. = FALSE)}, ~nchar(.)>10 )
#'   # Warning message:
#'   # oops
#' suppress_warnings( {sqrt(-1); warning("ooops", call. = FALSE)}, "NaN" )
#'   # Warning message:
#'   # oops

    


#' @export
suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(
    substitute(
      withCallingHandlers( .expr, warning = function (w) {
        cm   <- conditionMessage(w)
        cond <- if (is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm, ...)
        if (cond) invokeRestart("muffleWarning")   
      })
    )
  )
}


