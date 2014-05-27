\name{latable}
\alias{latable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print LaTeX table of regression results}
\description{
  Takes a list of regression models and returns a table of regression output 
  formatted for LaTeX.  There are two columns per regression: one for the 
  coefficient estimates, another for standard errors.
}
\usage{
latable(tables, substrings.to.remove = NULL, rows.to.remove=NULL, npmakebox = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tables}{
    List of regression models.  Supports models of class \code{glm,} \code{ivreg,} \code{lm,} 
    \code{negbin,} \code{polr,} \code{vglm,} and \code{zeroinfl.}       
}
  \item{substrings.to.remove}{
    List of strings or regular expressions.  If it is not a list, 
    it will be coerced to a list with as.list().
        Substrings in the row names that match any element in 
    \code{substrings.to.remove} will be removed before the output is created.    
}
  \item{rows.to.remove}{
    Should be a list of strings or regular expressions.  If it is not a list, 
    it will be coerced to a list with as.list().
        Rows that contain substrings matching any element in \code{rows.to.remove}
    will be removed from the output table before it is returned by the 
    function.  This is useful for creating "incomplete" regression tables that
    do not contain rows for some variables, e.g., control variables. 
  }
  \item{npmakebox}{
    Improves formatting of the ``Number of observations'' row, mainly by
    ensuring that the Ns for each regression aren't decimal-aligned with the 
    coefficient estimates.  Requires the \code{numprint} package to be loaded 
    in LaTeX.
  }     
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a table of regression output formatted for LaTeX.  The table is 
  designed to be copied directly into LaTeX.
}
\references{
%% ~put references to the literature/web site here ~
}

\author{John G. Bullock}

\note{The format of the tables produced by \code{latable} is inspired by 
      "Estimates of relative survival rates, by cancer site," a table in 
      Edward Tufte's essay on "The Cognitive Style of PowerPoint."
      
      The current version works well for \code{lm} and \code{ivreg} models.  
      It may be buggy when applied to models of other classes.
      
      The current version produces buggy output if the name of the 
      intercept row (typically ``(Intercept)'' or ``Intercept'' is modified by
      \code{substrings.to.remove} or \code{rows.to.remove}.          
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  There are other packages that perform similar functions.  See the 
  \code{xtable} and \code{apsrtable} functions for alternatives.
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{LaTeX}
\keyword{table}
\keyword{tables}% __ONLY ONE__ keyword per line
\keyword{Tufte}
