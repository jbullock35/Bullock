\name{push}
\alias{new_stack}
\alias{push}
\alias{pop}
\alias{shift}
\alias{unshift}
\title{Perl-like stack utilities for R}
\description{
  Perl-like stack utilities for R: \code{new_stack, push(), pop(), shift(),} and \code{unshift().}  
}
\usage{
  new_stack(value = NULL)
  push(stack, value)
  pop(stack)
  shift(stack, value)
  unshift(stack)
}
\arguments{
  \item{stack}{
    Object of class \code{stack}, created with \code{new_stack}.       
  }
  \item{value}{
    For \code{new_stack}, the initial value of a \code{stack} object.  For 
    \code{push} and \code{shift}, something to be added to a \code{stack} 
    object.  
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{new_stack} returns an object of class \code{stack}. 
  \code{pop} and \code{unshift} return the last and first values of 
  \code{val}, respectively. 
}
\references{
  Adapted from Jeffrey A. Ryan's code at http://www.lemnica.com/esotericR/Introducing-Closures/.  
}

\author{Jeffrey A. Ryan, John G. Bullock}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  nb <- new_stack()
  push(nb, 1:3)

  nb$.Data  # [1] 1 2 3

  pop(nb)  # from the back

  unshift(nb)  # from the front

  shift(nb, 3)
  push(nb, 1)
  nb$.Data  # [1] 3 2 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stack}
\keyword{Perl}

