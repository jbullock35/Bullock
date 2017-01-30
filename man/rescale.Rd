\name{rescale}
\alias{rescale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rescale a variable}
\description{Linear rescaling of numeric vectors.  For example, a variable 
  that ranges from 1 to 7 can be rescaled to range from 0 to 1.}
\usage{rescale(x, newrange)}
\arguments{
  \item{x}{numeric object}
  \item{newrange}{two-element numeric vector}
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
\author{Simon D. Jackman}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  vec <- 1:10
  vecRescaled <- rescale(vec, c(2:5))
  range(vecRescaled)  # 2 5 
}
