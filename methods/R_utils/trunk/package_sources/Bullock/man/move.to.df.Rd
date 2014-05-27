\name{move.to.df}
\alias{move.to.df}
\title{Move a list of variables into a data frame.}
\description{
  Copy variables matching the pattern into a data frame,
  and perhaps delete the free-standing original variables.}
\usage{
  move.to.df(pattern = NULL, move = TRUE)
}
\arguments{
  \item{pattern}{object of class \code{character}.  Can specify a regular expression.}
  \item{move}{logical variable.}
}
\details{
  IF \code{move == TRUE}, the variables in the environment will be deleted after they are moved into the data frame.
}
\value{Returned object is a data frame.}

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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
