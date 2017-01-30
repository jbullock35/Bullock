\name{factorToDummyMatrix}
\alias{factorToDummyMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perl-like qw() function for quoting a list of words}
\description{
  \code{factorToDummyMatrix} takes a factor of x levels and length n and 
  returns an n-by-x matrix.  The columns of the matrix have value 1, 0, or NA.   
}
\usage{
  factorToDummyMatrix(fac)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fac}{factor}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Matrix.  The column names of the matrix are the levels of the factor.
}
\references{}
\author{John G. Bullock}
\note{
  For factors that have no missing data, conversion to a matrix of dummy 
  variables can easily be accomplished by \code{model.matrix}.  But by 
  default, \code{model.matrix} omits NA values, returning a matrix that has 
  rows for only those cases that were not NA in the factor.  Moreover, 
  \code{model.matrix} does not have an ``na.action'' argument.
  
  This function temporarily changes the global na.action argument to permit
  \code{model.matrix} to return a matrix in which factor values of NA are 
  matched by NA in every column.    
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
