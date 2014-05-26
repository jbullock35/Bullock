\name{\%IN\%}
\alias{\%IN\%}
\title{Value matching}
\description{
  \code{\%IN\%} returns a logical vector indicating whether there is a match for its left operand.  It is like \code{\%in\%}, but it has one crucial difference: if there are \code{NA} values in the left operand, the corresponding values in the returned vector will also be \code{NA} (rather than \code{FALSE}, as with \code{\%in\%}.)  
}
\usage{
x \%IN\% table
}
\arguments{
  \item{x}{  vector or \code{NULL}: the values to be matched.}
  \item{table}{  vector or \code{NULL}: the values to be matched against.}
}
\details{}
\value{
  A logical vector of the same length as \code{x}.  It indicates whether a 
  match was found for each non-\code{NA} element of \code{x}.  \code{NA} 
  elements of \code{x} are matched by \code{NA} elements in the returned 
  vector.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{John G. Bullock}
\note{
  The ordinary binary match operator, \code{\%in\%}, can be misleading because  
  it seems more closely related to \code{==} than it is.  The problem is that 
  \code{==} will return \code{NA} in some (expected) cases, but \code{\%in\%} 
  will never return \code{NA}.  Instead, when using \code{\%in\%}, the returned
  vector will be \code{FALSE} for every \code{NA} value in the left operand.  
  
  Like \code{==}, \code{\%IN\%} will return \code{NA} when there are \code{NA}
  values in the left operand.  See below for an example.
  
  \code{\%IN\%} will always return \code{TRUE} values when \code{\%in\%} 
  would do so, and vice versa.  The two operators differ only in the sense 
  that \code{\%IN\%} returns \code{FALSE} in some cases where \code{\%in\%}
  returns \code{NA}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{match}}
}
\examples{
  tmp <- c(1, 2, 3, NA)
  tmp == 1      # TRUE FALSE FALSE NA
  tmp \%in\% 1:2  # TRUE TRUE  FALSE FALSE
  tmp \%IN\% 1:2  # TRUE TRUE  FALSE NA
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
