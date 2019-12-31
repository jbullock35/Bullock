#' Checks for existence of required LaTeX packages.
#' 
#' The code produced by \code{latexTable()} can be rendered only if certain 
#' LaTeX packages are installed. (See the note at the end of the 
#' \code{\link[=latexTablePDF]{latexTablePDF()}} help file for details.) If 
#' packages are missing, this function generates an informative string that 
#' can be used in warning or error messages. 
#' 
#' This function is not exported. It is called by \code{latexTablePDF()} only 
#' if packages are missing. It generates either an error (if \code{writePDF} 
#' is \code{TRUE}) or a warning (if \code{writePDF} is \code{FALSE} but
#' \code{writeTex} is \code{TRUE}).

#' @importFrom rlang .data

#' @param installedPackageList,requiredPackageList Character vectors.
#' @param writePDF,writeTex Logical variables. See the 
#' \code{\link[=latexTable]{latexTable()}} documentation for further 
#' information about these arguments.
missingPackageString <- function (installedPackageList, requiredPackageList, writePDF, writeTex) {
  missingPackages <- sapply(requiredPackageList, grepl, installedPackageList) %>% 
    apply(.data, 2, any) %>%
    { which(!.data) } %>%
    names
  
  missingPackageString <- switch(
    length(missingPackages),

    paste0(  # 1 missing package
      "It seems that a required LaTeX package, \"", 
      missingPackages, 
      "\", isn't installed."),

    paste0(  # 2 missing packages
      "It seems that two required LaTeX packages, ", 
      paste0("\"", missingPackages, "\"", collapse = " and "),
      ", aren't installed."),

    paste0(  # >2 missing packages
      "It seems that required LaTeX packages -- ", 
      paste0(
        paste0("\"", missingPackages[-length(missingPackages)], "\", ", collapse=""),
        paste0("and \"", missingPackages[length(missingPackages)], "\" -- ")),
      "aren't installed.")
  )

  if (writePDF) {
    stop(stringr::str_wrap(
      string = paste0(missingPackageString, " Before you can create a PDF file, you must install the required LaTeX packages. For details, see the note near the end of the latexTablePDF() help file."),
      width  = 72,
      exdent = 2))
  }
  else if (writeTex) {
    warning(stringr::str_wrap(
      string = paste0(missingPackageString, " You must install the required LaTeX packages before you can use the .tex file that you are creating. For details, see the note near the end of the latexTablePDF() help file."),
      width  = 72,
      exdent = 2))
  }
}

