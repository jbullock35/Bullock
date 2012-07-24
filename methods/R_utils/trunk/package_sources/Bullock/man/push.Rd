\name{push}
\alias{new_stack}
\alias{push}
\alias{pop}
\alias{shift}
\alias{unshift}
\title{Perl-like stack utilities for R}
\description{
  Perl-like stack utilities for R: \code{push(), pop(), shift(),} and \code{unshift().}  
}
\usage{
  new_stack()
  push(stack, val)
  pop(stack)
  shift(stack, val)
  unshift(stack)
}
\arguments{
  \item{stack}{
    Object of class \code{stack}, created with \code{new_stack}.       
  }
  \item{val}{
    Something to be added to or removed from a \code{stack} object.
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
  \code{nb <- new_betterstack()}
  \code{push(nb, 1:3)}

  \code{nb$.Data}
  \code{[1] 1 2 3}

  \code{pop(nb)  # from the back}
  \code{[1] 3}

  \code{unshift(nb)  # from the front}
  \code{[1] 1}

  \code{shift(nb, 3)}
  \code{push(nb, 1)}
  \code{nb$.Data}
  \code{[1] 3 2 1}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stack}
\keyword{Perl}

