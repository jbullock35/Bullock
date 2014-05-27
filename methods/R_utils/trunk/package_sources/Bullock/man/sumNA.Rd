\name{sumNA}
\alias{sumNA}
\title{Calculate sum of vector after omitting NA values}
\description{
  Calculate sum of vector after omitting NA values.
  
  Definition is \code{function(x) \{ return(sum(x, na.rm=TRUE)) \}}.  
}
\usage{sumNA(x)}
\arguments{
  \item{x}{logical, integer, numeric, or complex vector}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The sum. If all elements of \code{x} are of type integer or logical, then the sum is an integer.  Otherwise it is a length-one numeric or complex vector.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{sum}}}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
