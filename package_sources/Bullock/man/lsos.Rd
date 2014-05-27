\name{lsos}
\alias{lsos}
\alias{.ls.objects}
\title{Improved version of \code{ls}}
\description{
  Pretty-printed version of \code{ls} that indicates the size of every object in 
  an environment.  
}
\usage{
  .ls.objects(pos = 1, pattern, order.by, decreasing = FALSE, head = FALSE, n=5)
  lsos(..., n = 8)
}
\arguments{
  \item{pos}{position, on the search path, of the environment to search}
  \item{pattern}{regular expression.  Only names matching \code{pattern} are returned.}
  \item{order.by}{object of \code{character} class.  Valid arguments are \code{Type}, \code{Size}, \code{Rows}, and \code{Columns}.  If argument is unspecified, information on objects will be returned in alphabetical order.}
  \item{decreasing}{logical value.  Has no effect unless \code{order.by} is specified.}
  \item{head}{logical value.  IF \code{TRUE}, information on only \code{n} objects will be returned.}
  \item{n}{number of objects for which to report information.  Has no effect unless \code{head == TRUE}.}
  \item{...}{arguments that are passed to \code{.ls.objects}.}
}

\details{
  \code{lsos} is a wrapper to \code{.ls.objects}.  The main use of these 
  functions is to see which objects are taking up the most memory.
}
\value{The returned object is a data frame.}
\references{
  Function created by Dirk Edelbuettel and modified by JD Long.  See \url{http://stackoverflow.com/questions/1358003/} for details.  
}

\author{Dirk Edelbuettel, JD Long}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ls}}
}
\examples{}

  % Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
\keyword{}

