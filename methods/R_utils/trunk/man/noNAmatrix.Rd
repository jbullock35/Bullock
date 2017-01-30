\name{noNAmatrix}
\alias{noNAmatrix}
\title{Remove rows with any NA from a matrix.}
\description{
  Performs listwise deletion on a matrix, removing all rows from which any data are missing.
}
\usage{noNAmatrix(x)}
\arguments{
  \item{x}{matrix}
}
\details{}
\value{Returned value is a matrix.}
\author{}
\note{
  \code{noNAmatrix} is deprecrated.  It remains in this package exclusively for backward compatibility.  Those wishing to perform listwise deletion on a matrix should instead use \code{na.omit(x)}.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{na.omit}}}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
