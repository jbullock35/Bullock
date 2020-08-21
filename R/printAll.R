#' Print all rows of a tibble.
#' 
#' By default, tibbles---the default data frames created by tidyverse 
#' packages---print only as many rows as your console window can display.
#' \code{printAll()} instead prints all of the tibble's rows. If it is
#' called on an object other than a tibble, it just calls \code{print()}.
#' 
#' @param x Object, perhaps of class "tbl_df"
#' 
#' @examples
#' x <- tibble::tibble(a = 1:100, b = 101:200)
#' printAll(x)
#' 
#' @export
printAll <- function(x) {
  if (inherits(x, "tbl_df")) {
    print(x, n = nrow(x))
  }
  else {
    print(x)
  }
}

