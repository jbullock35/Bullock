\name{qw}
\alias{qw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perl-like qw() function for quoting a list of words}
\description{
  \code{qw} takes a string of words separated by spaces.  It returns a vector in 
  which each element is a word.  The point of the function is to speed the 
  creation of vectors of words.
}
\usage{
qw(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character string}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Character vector.
}
\references{
  Code taken from post by Florent Delmotte (``flodel'') at \url{http://stackoverflow.com/questions/520810/}.
}
\author{Florent Delmotte}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  qw("You can type    text here
    with    linebreaks if you
    wish")
  #  [1] "You"        "can"        "type"       "text"      
  #  [5] "here"       "with"       "linebreaks" "if"        
  #  [9] "you"        "wish" 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
