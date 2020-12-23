#' Detect whether a file is being sourced from another file.
#'
#' `sourcing()` returns `TRUE` if it seems that it is called from a file that 
#' is being sourced, e.g., with \linkInt{source} or \linkInt{sys.source}.
#' Otherwise, it returns `FALSE`. The command is helpful when we want a 
#' file to behave differently depending on whether it is sourced.
#' 
#' `sourcing()` is just an alias for `sys.nframe() != 1L`. 
#' @md
#' 
#' @seealso 
#' \linkInt{interactive}, \linkInt{sys.nframe}
#' 
#' The function was inspired by inspired by \code{interactive()}, which 
#' detects whether a command is being run interactively. Stack Overflow user 
#' r2evans wrote a helpful post: see <https://stackoverflow.com/a/47932989/697473>.
#' 
#' @examples 
#' if (sourcing()) {
#'   print("It seems that this code is being sourced. It's not being 
#'     executed directly from the global environment.")
#' } else {
#'   print("It seems that this code is not being sourced. It's being executed
#'     directly from the global environment.")
#' }



#' @export 
sourcing <- function () sys.nframe() != 1L