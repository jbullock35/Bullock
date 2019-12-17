#' Renders a LaTeX table as a PDF file.
#' 
#' \code{latexTablePDF} takes an object produced by \code{latexTable} and 
#' writes a PDF file. It can also write the corresponding .tex file.
#' 
#' Although \code{latexTablePDF} produces PDF files by default, it is also 
#' useful for creating .tex files. For example, you may have a list of tables
#' produced by \code{latexTable()} and a complex LaTeX 
#' document that contains many different sections and tables. When 
#' \code{latexTablePDF} is used with \code{writeTex = TRUE}, it will produce a 
#' single file that contains LaTeX code for all of the tables in your list. 
#' You can insert those tables into your LaTeX document by adding a 
#' single \code{\\input} or \code{\\include} command to your LaTeX document.
#' Doing so will either insert the tables directly (if you ran  
#' \code{latexTable()} with \code{callCommand = FALSE} or make available to  
#' you a set of LaTeX macros that you can use to insert the tables into  
#' arbitrary places in your LaTeX document.  [PUT THIS INTO THE VIGNETTE!] 

#' @note \emph{Required LaTeX tools.} If \code{writePDF} is \code{TRUE}, 
#'   \code{pdflatex} must be installed on your system. (It is part of almost 
#'   every LaTeX installation.) In addition, if \code{writePDF} is \code{TRUE}  
#'   and \code{containerFilename} is \code{NULL} (the default), the following 
#'   LaTeX packages must be installed: \code{array, booktabs, caption, 
#'   fancyhdr, geometry, pdflscape, } and \code{ragged2e}. Most of these, and 
#'   perhaps all of them, are already part of your LaTeX installation.\cr\cr\cr\cr



#' @family functions for making tables


#' @examples
#' \dontrun{
#'   data(iris)
#'   lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
#'   lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#'   lm3 <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#'   lmList <- list(lm1, lm2, lm3)
#'   rT1 <- regTable(lmList)
#'   lT1 <- latexTable(
#'     mat        = rT1,
#'     colNames   = qw("(1) (2) (3)"),
#'     rowNames   = c("Intercept", "Petal length", "Petal width", "Petal length $\\times$ petal width"),
#'     footerRows = list(
#'       c("Number of observations", sapply(lmList, nobs)),
#'       c("R$^2$", sapply(lmList, function (x) round(summary(x)$r.squared, 2)))
#'     ),
#'     spacerColumns      = c(0, 2, 4),
#'     spacerColumnsWidth = '.67em',
#'     spacerRows         = 1,  # separate Intercept row from other rows
#'     caption            = '\\textit{Sepal length as a function of petal length and petal width.} Lorem ipsum dolor.'
#'   )
#'   latexTablePDF(lT1)
#' }




#' @param latexTable Object of class \code{latexTable} (typically produced by 
#'   \code{latexTable()} or a list of such objects.
#' @param container Logical variable. Should the LaTeX code in \code{latexTable}
#'   be inserted into a LaTeX file that contains a complete LaTeX preamble and
#'   the \code{\\begin{document}} and \code{\\end{document}} tags? If you want 
#'   \code{latexTablePDF} to produce a PDF file, \code{container} must be 
#'   \code{TRUE}. But if you just want to write \code{latexTable} to disk as 
#'   a set of .tex files that you will insert into your own larger LaTeX 
#'   document, \code{container} should probably be \code{FALSE}.  
#' @param containerFilename A string. Specifies the path of the "container" 
#'   LaTeX file into which the \code{latexTable} table or tables will be 
#'   inserted to make a complete LaTeX file that can be rendered as PDF.
#' @param outputFilenameStem A string. It is the path and name of the file   
#'   is to be saved to disk, up to the extension. For example, if you want to  
#'   save "myTable.pdf" to disk, set \code{outputFilenameStem = "mytable"}.
#' @param writePDF Logical variable. Should a PDF file be saved to disk?
#' @param writeTex Logical variable. Should a .tex file be saved to disk?
#' @param overwriteExisting Logical variable. Should files to be saved overwrite
#'   existing files that have the same names?
#' @param verbose Logical variable. \code{latexTablePDF} calls  
#'   \code{pdflatex} to render PDF files, and if \code{verbose} is \code{TRUE}, 
#'   all of the output from \code{pdflatex} will be printed to screen.\cr\cr\cr\cr 

#' @param continuedFloat Logical variable. Should be \code{TRUE} if the table 
#'   or tables to be rendered are part of a series and should all share the 
#'   same number. For example, \code{continuedFloat} should be true if 
#'   \code{latexTable} is a list of tables and you all want them to be 
#'   numbered as Table{\NB}3.  
#' @param continuedFloatStar Logical variable. Should be \code{TRUE} if the table 
#'   or tables to be rendered are part of a series, and if all tables should 
#'   share the same number but be distinguished by some secondary character.
#'   For example, \code{continuedFloatStar} should be true if \code{latexTable} 
#'   is a list of tables and you all want them to be numbered as Table~3a, 
#'   Table~3b, etc.\cr
#'     \indent \code{continuedFloat} and \code{continuedFloatStar} cannot both 
#'   be \code{TRUE}.
#' @param firstPageEmpty Logical variable. If \code{TRUE}, the page that 
#'   contains the first table in \code{latexTable} will have an empty header
#'   and footer. (Technically, \code{latexTablePDF} will insert 
#'   \code{\\thispagestyle{empty}} into the code block that contains the first
#'   table in \code{latexTable}.)    
#' @param firstTableNumber Integer. What number should the first table in 
#'   \code{latexTable} have?\cr
#'   \indent By default, the table numbering will be "natural." For example, 
#' if \code{latexTablePDF} is used to produce a PDF document, the first table 
#' in the document will be Table{\NB}1. And if \code{latexTablePDF} is instead
#' used to create a .tex file that is to be included in a larger LaTeX 
#' document, the number of the first \code{latexTable} table will be determined
#' by the number of preceding tables in the document. For example, if 
#' you use \code{latexTablePDF} to create a .tex file that you then insert 
#' into a larger LaTeX document, and if the .tex file is preceded in the 
#' LaTeX document by two tables, the first table in \code{latexTable} will be 
#' Table{\NB}3.\cr\cr\cr\cr
#' 
#' @param openPDFOnExit Logical variable. Open the PDF file after it is created
#'   by \code{latexTablePDF}? This argument has an effect only on Windows, and
#'   only if \code{writePDF} is \code{TRUE}.     



# TODO:
# --Explain how the function can be useful even when not producing PDF. See 
#   IV_tables_firstStage.R for an example.
# --Check for existence of pdflatex in the path.  [2019 12 16]
# --Write a unit test: write a temporary PDF file and then test to see whether
#   it has actually been written.  [2019 12 16]
# --Test this function (a) on systems that don't have fontcommands.sty or 
#   mathcommands.sty installed, and (b) on non-Windows systems.  [2019 12 16]


#' @export
latexTablePDF <- function(
  latexTable,
  container          = TRUE,  # if FALSE, .tex file can't be PDF'd
  containerFilename  = NULL,
  outputFilenameStem = 'latexTable',
  writePDF           = TRUE,
  writeTex           = FALSE,
  overwriteExisting  = FALSE,
  verbose            = FALSE,
  
  continuedFloat     = FALSE,
  continuedFloatStar = FALSE,  
  firstPageEmpty     = TRUE,
  firstTableNumber   = NULL,
  
  openPDFOnExit      = TRUE) {
  

  # PRELIMINARIES AND ERROR-CHECKING
  oldWD <- getwd()
  on.exit(setwd(oldWD))
  if (! any(grepl('list|latexTable', class(latexTable)))) { 
    stop("latexTable must be of the 'list' or 'latexTable' classes.")
  }
  if ('list' %in% class(latexTable) && any(!grepl('latexTable', sapply(latexTable, class)))) {
    stop("Every element in the latexTable list must be of the 'latexTable' class.")
  }
  
  # Transform "latexTable" into a list if need be
  if ('latexTable' %in% class(latexTable)) {
    latexTable <- list(latexTable)
  }
  
  # Further warnings and errors
  if (file.exists(paste0(outputFilenameStem, '.pdf')) && overwriteExisting) {
    warning(paste0(outputFilenameStem, '.pdf will be overwritten.'))
  } else if (file.exists(paste0(outputFilenameStem, '.pdf')) && !overwriteExisting) {
    stop(paste0(outputFilenameStem, '.pdf already exists and overwriteExisting = FALSE.'))
  }
  if (file.exists(paste0(outputFilenameStem, '.tex')) && overwriteExisting && writeTex) {
    warning(paste0(outputFilenameStem, '.tex will be overwritten.'))
  } else if (file.exists(paste0(outputFilenameStem, '.tex')) && !overwriteExisting) {
    stop(paste0(outputFilenameStem, '.tex already exists and overwriteExisting = FALSE.'))
  }
  if (continuedFloat && continuedFloatStar) {
    stop('"continuedFloat" and "continuedFloatStar" cannot both be TRUE.')    
  }    
  if (!container && writePDF) {
    stop('if "container" is FALSE, "writePDF" must also be FALSE.')    
  }    
  
  
  
  
  # REMOVE PAGE NUMBER FROM FIRST PAGE
  # To remove the page number from the first page, 
  # \thispagestyle{empty} needs to go in the code block for the first 
  # LaTeX table.  [2014 03 15]
  if (firstPageEmpty) {
    table1 <- latexTable[[1]]
    positionToInsertNewCommand <- which(grepl('newcommand', table1))
    table1 <- c(
      table1[1:positionToInsertNewCommand], 
      '  \\thispagestyle{empty}',
      table1[(positionToInsertNewCommand+1):length(table1)]
    )  
    latexTable[[1]] <- table1
    rm(table1, positionToInsertNewCommand)
  }
  
  
  # CHANGE TABLE NUMBERING
  # We need to account for the weird LaTeX numbering scheme. For example, if 
  # firstTableNumber = 2, we insert \setcounter{table}{1} to get the first
  # table in the PDF file to be Table 2.  [2019 01 15] 
  if (!is.null(firstTableNumber)) {
    table1 <- latexTable[[1]]
    positionToInsertNewCommand <- which(grepl('newcommand', table1))
    table1 <- c(
      table1[1:positionToInsertNewCommand], 
      paste0('  \\setcounter{table}{', firstTableNumber - 1, '}'),
      table1[(positionToInsertNewCommand+1):length(table1)]
    )  
    latexTable[[1]] <- table1
    rm(table1, positionToInsertNewCommand)
  }
  
  # ADD \CONTINUEDFLOAT COMMANDS
  # E.g., for creating Figure "3a," Figure "3b"
  insertContinuedCommand <- function (x, star = FALSE) {
    lineToInsert <- if (star) '    \\ContinuedFloat*' else '    \\ContinuedFloat'    
    beginLine    <- which(grepl('\\\\begin\\{(table|figure)\\}', x))
    x            <- c(x[1:beginLine], lineToInsert, x[(beginLine+1):length(x)])
    x
  }
  if (continuedFloat || continuedFloatStar) {
    latexTable <- lapply(
      X   = latexTable, 
      FUN = function (x) insertContinuedCommand(x, star = continuedFloatStar))    
  }
  
  
  # CREATE THE ENTIRE LATEX DOCUMENT
  if (container) {
    if (!is.null(containerFilename)) {
      latexContainer <- readLines(containerFilename)
    } else {
      latexContainer <- readLines(
        system.file("tableContainer.tex", package = "Bullock", mustWork = TRUE)
      )
    }
    lineToReplace  <- which(grepl('TABLE GOES HERE!', latexContainer))  
    newTable <- latexContainer[1:(lineToReplace)-1]
    for (i in 1:length(latexTable)) {
      newTable <- c(newTable, paste0('% TABLE ', i), latexTable[[i]], "", "")
    }
    newTable <- c(newTable, latexContainer[(lineToReplace+1):length(latexContainer)])
  } else {
    newTable <- ''
    for (i in 1:length(latexTable)) {
      newTable <- c(newTable, paste0('% TABLE ', i), latexTable[[i]], "", "")
    }    
  }
    
  # Check for \afterpage commands and eliminate them.  Apparently, in 
  # documents for which there is no content other than tables, 
  # \afterpage causes pdflatex to fail to compile to PDF.  [2014 03 15]  
  afterpageLines <- which(grepl('\\\\afterpage', newTable))
  if (any(afterpageLines)) {
    newTable <- newTable[-c(afterpageLines, afterpageLines+6)]
  }
  
  
  # CREATE FILES IN THE NEW TEMPORARY DIRECTORY
  tmpFilename <- tempfile(fileext = '.tex')  # includes path to tempdir()
  writeLines(newTable, tmpFilename)
  setwd(tempdir())
  if (writePDF) {  
    if (container && verbose) {
      system2('pdflatex', shQuote(tmpFilename))
    } 
    else if (container && !verbose) {
      system2('pdflatex', shQuote(tmpFilename), stdout = FALSE, stderr = FALSE)
    }  
  }

  
  
  
  # MOVE PDF OUT OF TEMPORARY DIRECTORY, THEN DELETE TEMP. DIRECTORY
  if (writePDF) {
    file.copy(
      from      = sub("\\.tex", "\\.pdf", tmpFilename), 
      to        = paste0(oldWD, '/', outputFilenameStem, '.pdf'),
      overwrite = overwriteExisting)
  }
  if (writeTex) {
    file.copy(
      from      = tmpFilename, 
      to        = paste0(oldWD, '/', outputFilenameStem, '.tex'),
      overwrite = overwriteExisting)
  }

  
  # OPEN THE NEW PDF FILE (IF RUNNING WINDOWS)
  if (writePDF && openPDFOnExit && Sys.info()['sysname'] == 'Windows') {
    PDFFullPath <- normalizePath(paste0(oldWD, '/', outputFilenameStem, '.pdf'))
    shell.exec(PDFFullPath)
  }
  
}
  