\name{lsos}
\alias{.ls.objects}
\title{Improved version of ls()}
\description{
  Pretty-printed version of ls() that indicates the size of every object in 
  an environment.  
}
\usage{
  .ls.objects(pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5)
  lsos(..., n = 8)

}
\arguments{
  \item{pos}{
    position of environment on search path to search       
  }
}

\details{
  \code{lsos} is a wrapper to \code{.ls.objects}.  The main use of these 
  functions is to see which objects are taking up the most memory.
}
\value{
}
\references{
  Function created by Dirk Edelbuettel and modified by JD Long.  See http://stackoverflow.com/questions/1358003/ for details.  
}

\author{Dirk Edelbuettel, JD Long}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}

  % Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
\keyword{}

