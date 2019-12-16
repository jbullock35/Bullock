\name{lNA}
\alias{lNA}
\alias{lNAv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate length of vector after omitting NA values}
\description{
  Calculate length of vector after omitting NA values.
}
\usage{
  lNA(x, verbose = FALSE)
  lNAv(x)
}
\arguments{
  \item{x}{A vector.}
  \item{verbose}{Logical variable. If \code{TRUE}, \code{lNA} will print the 
  lengths of \code{x} before and after \code{NA} values have been removed.}
}
\details{
  \code{lNA(x)} returns its value silently. \code{lNAv} is shorthand for 
  \code{lNA(x, verbose = TRUE)}; it returns the same value as \code{lNA(x)}  
  but also prints the lengths of the vector before and after NAs are removed.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{John G. Bullock}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  x <- c(1:3, NA, 5)
  lNA(x)
  lNAv(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
