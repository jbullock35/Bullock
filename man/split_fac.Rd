\name{split_fac}
\alias{split_fac}
\title{Create dummy variables for each level of a factor.}
\description{Create dummy variables for each level of a factor.}
\usage{
  split_fac(
    fac, 
    prefix = paste(deparse(substitute(NES.year.fac)), '.', sep = ''), 
    env    = .GlobalEnv, 
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fac}{factor variable}
  \item{prefix}{substring that begins the name of each created dummy variable}
  \item{env}{environment in which the dummy variables are created}
  \item{...}{arguments passed to assign()}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{split_fac} returns nothing.  Instead, it creates, as a side effect, a set of logical variables -- one for each level of \code{fac}.
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
  fac <- factor(rep(1:3, each = 3))
  split_fac(fac, prefix = 'fac')  # creates logical variables fac1, fac2, and fac3 in .GlobalEnv
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
