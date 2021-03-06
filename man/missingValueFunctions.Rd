% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_value_functions.R
\name{missingValueFunctions}
\alias{missingValueFunctions}
\alias{allNA}
\alias{lNA}
\alias{lNAv}
\alias{maxNA}
\alias{meanNA}
\alias{medianNA}
\alias{minNA}
\alias{rangeNA}
\alias{sdNA}
\alias{sumNA}
\alias{varNA}
\title{Missing-value helper functions.}
\usage{
allNA(x)

lNA(x, verbose = FALSE)

lNAv(x)

maxNA(x)

meanNA(x)

medianNA(x)

minNA(x)

rangeNA(x)

sdNA(x)

sumNA(x)

varNA(x)
}
\arguments{
\item{x}{An R object.}

\item{verbose}{Logical variable. If \code{TRUE}, \code{lNA} will print the 
lengths of \code{x} before and after \code{NA} values have been removed.}
}
\description{
Functions to make code a little clearer. These are mainly ordinary functions,
like \code{mean()}, with \code{na.rm} set to \code{TRUE}. For example, 
\code{meanNA()} is defined as \code{function(x) mean(x, na.rm = TRUE)}.
}
\details{
\code{lNA(x)} returns its value silently. \code{lNAv} is shorthand for 
\code{lNA(x, verbose = TRUE)}; it returns the same value as \code{lNA(x)}  
but also prints the lengths of the vector before and after NAs are removed.
}
\examples{
lNA(NA)    # 0

x <- c(1:3, NA, 5)
allNA(x <= 5)  # TRUE
lNA(x)         # 4
lNAv(x)        # 4 (length with NA=5)

sum(NA)        # NA
sumNA(NA)      # 0

sum(x)         # NA
sumNA(x)       # 11
meanNA(x)      # 2.75

sdNA(x)
varNA(x)
}
