#' Compute Cronbach's alpha
#' 
#' \code{alpha_cronbach()} takes a covariance matrix of data, \code{S}, 
#' that has been created from a matrix in which each column represents a 
#' variable. It returns Cronbach's alpha for the battery. It is not 
#' exported, but it is called by \linkInt{reliability}.
#' 
#' @author Joseph F. Lucke
#' 
#' @param S Covariance matrix generated by \code{var()}.
#' 
#' @return 
#' The return value is a number (double): the estimated value of Cronbach's 
#' alpha.
alpha_cronbach <- function(S) {
  d <- dim(S)[1]
  (d/(d - 1)) * (1 - sum(diag(S))/sum(S))
}


#' Compute and report Cronbach's alpha
#' 
#' Given a matrix \code{x} in which every column represents a variable,
#' this function reports (a) the estimated reliability for the battery of 
#' \code{n} variables, and (b) the estimated variability for each possible 
#' combination of \code{n - 1} variables.
#' 
#' @author Peter Ellis
#' 
#' @param x Matrix of numbers
#' @param ... additional arguments passed to \code{var()}
#' 
#' @return List with two elements. The first element, \code{alpha}, is the 
#' estimated Cronbach's alpha for the battery of \code{n} variables. The 
#' second element, \code{alpha.if.deleted}, reports the estimated 
#' reliability of each combination of \code{n - 1} variables.
#' 
#' @examples 
#' data(iris)
#' reliability(iris[, 1:4])
#' 
#' @export 
reliability <- function(x, ...) {
  j <- dim(x)[2]
  out <- numeric(j)
  for (i in 1:j){
    out[i] <- alpha_cronbach(var(x[ ,-i], ...))
  }
  list(
    alpha            = alpha_cronbach(var(x, ...)), 
    alpha.if.deleted = out)
} 