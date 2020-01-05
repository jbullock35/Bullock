#' Find modal value of a vector.
#'
#' Find modal value of a vector. If there are multiple modal values, all will
#' be returned.
#' 
#' Before this package was released, it returned only the first mode if there
#' were multiple modes. It now returns all modes. See the examples.

#' @param x Vector.
#' @param na.rm Logical variable.
#' 
#' @examples 
#' modalValue(qw("a b b"))      # [1] "b"
#' modalValue(qw("a a b b c"))  # [1] "a" "b"
#' modalValue(1:3)              # [1] 1 2 3
#' 
#' @author 
#' Ken Williams. See \url{http://stackoverflow.com/a/8189441/697473}.

modalValue <- function(x, na.rm = FALSE) {
  if (na.rm) {
    ux <- unique(x[!is.na(x)])
  } else {
    ux <- unique(x)
  }
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
