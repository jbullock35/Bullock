\name{merge_fac}
\alias{merge_fac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge factors}
\description{Fill in missing values in one factor with missing values from another.}
\usage{merge_fac(fac.names)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fac.names}{character vector of factor names}
  \item{...}{arguments passed to \code{get()}}
}
\details{
  All factors should be of the same length.  Missing values in the first 
  factor named in \code{fac.names} are filled in with corresponding values 
  from the second factor.  Missing values in this merged factor are filled in 
  with corresponding values from the third factor.  And so on.
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
  Merging factors in this way is trickier than just using a command like 
  \code{fac1[is.na(fac1)] <- fac2[is.na(fac1)]} because fac1 and fac2 may have
  different factor levels.  This commands takes care of the problem by 
  merging the levels among different factors.
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
