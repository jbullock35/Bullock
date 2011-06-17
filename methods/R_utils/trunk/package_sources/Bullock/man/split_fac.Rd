\name{split_fac}
\alias{split_fac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create dummy variables for each level of a factor.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
split_fac(fac, prefix=paste(deparse(substitute(NES.year.fac)), '.', sep=''), env=.GlobalEnv, ...)
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
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
