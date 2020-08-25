#' Perl-like stack utilities for R.
#' 
#' You can "pop" data off the end of a vector, one element at a time, with 
#' \code{pop()}. You can also "push" new data onto the end of the vector with 
#' \code{push()}. The analogous functions for working at the start of a
#' vector are \code{shift()} and \code{unshift()}: \code{shift()} removes the
#' first element of your vector, and \code{unshift()} prepends new elements to
#' your vector. 
#' 
#' An important and unusual feature of \code{push(), pop(), shift(),} and 
#' \code{unshift()} is that they modify objects "in place"---that is, even  
#' when no explicit assignment is done. For example, \code{pop(x)} will return 
#' the last value of \code{x}, but it will also remove the last value from the 
#' \code{x} object. The examples illustrate this point.
#' 
#' These functions are adapted from Matt Pettis's code at 
#' \url{https://gist.github.com/mpettis/b7bfeff282e3b052684f}.
#' 
#' Previous versions of these functions were adapted from Jeffrey A. Ryan's 
#' code at \url{http://www.lemnica.com/esotericR/Introducing-Closures/}. That
#' code works but is based on the creation of "stack" objects that contain 
#' their own environments. One consequence is that changing a copy of a stack
#' object changes the original stack object, and vice versa. Note too that, 
#' in Ryan's code, the traditional meanings of \code{shift()} and 
#' \code{unshift()} are reversed: he uses \code{shift()} to concatenate 
#' objects, \code{unshift()} to remove a value from an object.
#' 
#' @seealso
#' \href{https://gist.github.com/leeper/d6d085ac86d1e006167e#file-pushpop-r-L25}{Thomas Leeper's Gist}
#' describes his own implementation of \code{pop()} and \code{push()} and 
#' includes links to six other implementations. Some of these implementations
#' of \code{pop()} and \code{push()} do not have the modify-in-place 
#' characteristic of the corresponding Perl functions. This quality is also 
#' absent from the constructor function at \url{https://stackoverflow.com/a/14489296/697473}.   
#'  
#' @param x Object, typically a vector or a list.
#' @param values Object to be added to \code{x}.
#' 
#' @return 
#' \code{pop()} and \code{shift()} will return a scalar, that is, an object of 
#' length{\NB}1. \code{push()} and \code{unshift()} don't return anything.
#' 
#' 
#' @author Matt Pettis
#' @author John G. Bullock
#' 
#' 
#' @examples
#' myStack <- 1:3
#' push(myStack, 4)
#' myStack  # [1] 1 2 3 4
#' 
#' pop(myStack)    # [1] 4
#' shift(myStack)  # [1] 1
#' myStack         # [1] 2 3
#' 
#' unshift(myStack, "hello")
#' myStack         # [1] "hello" "2" "3"
#' 
#' myList <- list(1, 2, 3)  # list with three elements
#' push(myList, 4)
#' myList                   # list with 4 elements
#' shift(myList)            # returns 1
#' myList[[1]]              # first element of myList is now 2
#' 
#' 
#' @name stackUtilities
#' @rdname stackUtilities
#' @export
push <- function(x, values) {
  assign(as.character(substitute(x)), c(x, values), parent.frame())
}

#' @rdname stackUtilities
#' @export 
pop <- function(x) {
  if(length(x) == 0) return(NA)
  popret <- x[length(x)]
  assign(as.character(substitute(x)), x[-length(x)], parent.frame())
  popret
}

#' @rdname stackUtilities
#' @export
unshift <- function(x, values) {
  assign(as.character(substitute(x)), c(values, x), parent.frame())
}

#' @rdname stackUtilities
#' @export
shift <- function(x) {
  if(length(x) == 0) return(NA)
  shiftret <- x[1]
  assign(as.character(substitute(x)), x[-1], parent.frame())
  shiftret
}
