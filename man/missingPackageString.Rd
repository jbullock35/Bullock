% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missingPackageString.R
\name{missingPackageString}
\alias{missingPackageString}
\title{Checks for existence of required LaTeX packages.}
\usage{
missingPackageString(
  installedPackageList,
  requiredPackageList,
  writePDF,
  writeTex
)
}
\arguments{
\item{installedPackageList, requiredPackageList}{Character vectors.}

\item{writePDF, writeTex}{Logical variables. See the 
\code{\link[=latexTable]{latexTable()}} documentation for further 
information about these arguments.}
}
\description{
The code produced by \code{latexTable()} can be rendered only if certain 
LaTeX packages are installed. (See the note at the end of the 
\code{\link[=latexTablePDF]{latexTablePDF()}} help file for details.) If 
packages are missing, this function generates an informative string that 
can be used in warning or error messages.
}
\details{
This function is not exported. It is called by \code{latexTablePDF()} only 
if packages are missing. It generates either an error (if \code{writePDF} 
is \code{TRUE}) or a warning (if \code{writePDF} is \code{FALSE} but
\code{writeTex} is \code{TRUE}).
}
