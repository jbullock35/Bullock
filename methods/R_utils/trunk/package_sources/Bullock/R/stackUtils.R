# stackUtils.R
# created 2012 July 24

# Provides Perl-like push(), pop(), shift(), and unshift().  Adapted from
# Jeffrey A. Ryan's code at 
# http://www.lemnica.com/esotericR/Introducing-Closures/.

new_stack <- function(value = NULL) { 
  stack <- new.env()
  stack$.Data <- vector()

  # make value the initial value of the stack
  if (! is.null(value)) {
    stack$.Data <- c(stack$.Data, value)     
  }

  stack$push <- function(x) .Data <<- c(.Data, x)
  stack$pop  <- function() {
    tmp <- .Data[length(.Data)]
    .Data <<- .Data[-length(.Data)]
    return(tmp)
  }
  stack$shift   <- function(x) .Data <<- c(x, .Data)
  stack$unshift <- function() {
    tmp <- .Data[1]
    .Data <<- .Data[-1]
    return(tmp)
  }
  environment(stack$push)    <- as.environment(stack)
  environment(stack$pop)     <- as.environment(stack)
  environment(stack$shift)   <- as.environment(stack)
  environment(stack$unshift) <- as.environment(stack)
  
  class(stack) <- "stack"
  stack
}
    
push    <- function(x, value, ...) UseMethod("push")     # add to end of vector
pop     <- function(x, ...)        UseMethod("pop")      # return last value from vector and remove it from vector
shift   <- function(x, value, ...) UseMethod("shift")
unshift <- function(x, ...)        UseMethod("unshift")
push.stack    <- function(x, value, ...) x$push(value)
pop.stack     <- function(x)             x$pop()
shift.stack   <- function(x, value, ...) x$shift(value)
unshift.stack <- function(x)             x$unshift()
