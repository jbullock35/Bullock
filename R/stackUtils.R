#' Perl-like stack utilities for R.
#' 
#' \code{newStack()} constructs a "stack"---a vector---of data. You can "push"  
#' new data onto the end of the vector with \code{push()} or "pop" data off 
#' the end of it, one at a time, with \code{pop()}. You can also \code{shift()}
#' data onto beginning of the vector or \code{unshift()} values from the 
#' beginning of the vector---that is, remove them from the vector---one at a 
#' time.
#' 
#' An important and unusual feature of \code{push(), pop(), shift(),} and 
#' \code{unshift()} is that they modify \code{stack} objects "in place"---that 
#' is, even when no explicit assignment is done. For example, \code{pop(x)} 
#' will return the last value of \code{x}, but it will also remove the last 
#' value from the \code{x} object. The examples illustrate this point.
#' 
#' Adapted from Jeffrey A. Ryan's code at \url{http://www.lemnica.com/esotericR/Introducing-Closures/}. 
#' 
#' @param x Vector.
#' @param stack Stack object.
#' 
#' @return 
#' For \code{new_stack()}, a stack object. For \code{pop()}, the last element
#' of the stack object (which will also be removed from the stack object). For
#' \code{unshift()}, the first element of the stack object (which will also 
#' be removed from the stack object).
#' 
#' 
#' @author 
#' Jeffrey A. Ryan
#' John G. Bullock
#' 
#' 
#' @examples
#' myStack <- newStack(1:3)
#' push(myStack, 4)
#' myStack$data
#' 
#' pop(myStack)      # remove last element
#' unshift(myStack)  # remove first element
#' myStack$data
#' 
#' shift(myStack, 'hello')
#' myStack$data
#' 
#' 
#' @name stackUtilities
#' @rdname stackUtilities
#' @importFrom utils head
#' @importFrom utils tail
#' @export


# NOTES:
# * On S3 methods, see https://adv-r.hadley.nz/s3.html.  [2020 01 07]
# * The simplified constructor function at 
#   https://stackoverflow.com/a/14489296/697473 is less functional. It 
#   provides pop() and push(), but these functions don't modify the original
#   object, as they do in Perl. For example, in that Stack Overflow post, 
#   y <- pop(x) creates y but doesn't modify x.  [2020 01 07] 


# TODO: 
# --Point out that the methods can modify in place, without assignment. Show 
#   how push() and pop() work together and how shift() and unshift() work 
#   together.  [2020 01 07]
#   tmp2 <- tmp
#   push(tmp, 4:5)
#   tmp$.Data
# --Write an appropriate print method for "stack" objects.  [2019 12 18]




# S3 CONSTRUCTOR FUNCTION
newStack <- function(x = NULL) { 
  stack      <- new.env()
  stack$data <- vector()

  # make value the initial value of the stack
  if (! is.null(x)) {
    stack$data <- x     
  }

  stack$push <- function(x) stack$data <<- c(stack$data, x)
  stack$pop  <- function() {
    tmp <- utils::tail(stack$data, 1)
    stack$data <<- stack$data[-length(stack$data)]
    return(tmp)
  }
  stack$shift   <- function(x) stack$data <<- c(x, stack$data)
  stack$unshift <- function() {
    tmp <- utils::head(stack$data, 1)
    stack$data <<- stack$data[-1]
    return(tmp)
  }

  environment(stack$push)    <- stack
  environment(stack$pop)     <- stack
  environment(stack$shift)   <- stack
  environment(stack$unshift) <- stack  
  
  structure(.Data = stack, class = "stack")
}
    

# CREATE NEW GENERICS
#' @rdname stackUtilities
#' @export 
push    <- function(stack, x) UseMethod("push")     # add to end of vector

#' @rdname stackUtilities
#' @export 
pop     <- function(stack)    UseMethod("pop")      # return last value from vector and remove it from vector

#' @rdname stackUtilities
#' @export 
shift   <- function(stack, x) UseMethod("shift")

#' @rdname stackUtilities
#' @export 
unshift <- function(stack)    UseMethod("unshift")



# ADD METHODS FOR THE GENERICS (FOR OBJECTS OF THE "STACK" CLASS)
push.stack    <- function(stack, x) stack$push(x)
pop.stack     <- function(stack)    stack$pop()
shift.stack   <- function(stack, x) stack$shift(x)
unshift.stack <- function(stack)    stack$unshift()
