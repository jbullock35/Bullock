#' Renders a LaTeX table as a PDF file.
#' 
#' \code{latexTablePDF()} takes an object produced by \linkInt{latexTable} and 
#' writes a PDF file. It can also write the corresponding .tex file.
#' 
#' Although \code{latexTablePDF} produces PDF files by default, it is also 
#' useful for creating .tex files. For example, you may have a list of tables
#' produced by \code{latexTable()} and a complex LaTeX 
#' document that contains many different sections and tables. When 
#' \code{latexTablePDF} is used with \code{writeTex = TRUE}, it will produce a 
#' single file that contains LaTeX code for all of the tables in your list. 
#' You can then insert those tables into your LaTeX document by adding a 
#' single \code{\\input} or \code{\\include} command to your LaTeX document.
#' For details, see \href{../doc/tables.html}{\code{vignette("tables", package = "Bullock")}}.

#' @note \emph{Required LaTeX tools.} If \code{writePDF} is \code{TRUE}, 
#'   \code{pdflatex} must be installed on your system. (It is part of almost 
#'   every LaTeX installation.) The `array,` `booktabs,` `caption,` and 
#'   `numprint` packages must also be installed. In addition:
#' * If \code{writePDF} is \code{TRUE} and \code{containerFilename} is 
#'   "tableContainer.tex" (the default), the `fancyhdr,` `footmisc`, `geometry,`
#'   and `ragged2e` packages must be installed.
#' * If \code{writePDF} is \code{TRUE} and you are producing a landscaped 
#'   table, the \code{afterpage} and \code{pdflscape} packages must be
#'   installed.
#' 
#' Most of these packages, and perhaps all of them, are already part of your 
#' LaTeX installation.
#' @md


#' @seealso Other functions for making tables: \linkInt{latexTable}, 
#' \linkInt{latexTablePDF}. See also the \href{../doc/tables.html}{Building better tables in less time} 
#' vignette. 

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
#'     colNames   = lt_colNumbers(),
#'     rowNames   = c(
#'       "Intercept", 
#'       "Petal length", 
#'       "Petal width", 
#'       "Petal length $\\times$ petal width"),
#'     footerRows = list(lt_nobsRow(), lt_rSquaredRow()),
#'     spacerRows = 1,  # insert white space between Intercept row and other rows
#'     caption    = paste0(
#'       "\\textit{Sepal length as a function of petal length and petal width.} ",
#'       "Entries are estimates and standard errors from OLS regressions..."
#'     )
#'   )
#'   latexTablePDF(lT1, outputFilenameStem = "irisData")
#' 
#' 
#'   # Create a PDF or .tex file that contains two tables:
#'   lm1v <- update(lm1, subset = (Species == 'versicolor'))
#'   lm2v <- update(lm2, subset = (Species == 'versicolor'))
#'   lm3v <- update(lm3, subset = (Species == 'versicolor'))
#'   rT2  <- regTable(list(lm1v, lm2v, lm3v))
#'   lT2  <- update(lT1, mat = rT2, commandName = "tableVersicolor")
#' 
#'   latexTablePDF(                                # PDF with two pages
#'     lT2, 
#'     outputFilenameStem = "irisData_twoTables")  
#'   latexTablePDF(                                # add .tex file with code for two tables
#'     lT2, 
#'     outputFilenameStem = "irisData_twoTables",
#'     writeTex           = TRUE)
#' }  



#' @param latexTable Object of class \code{latexTable} (typically produced by 
#'   \code{latexTable()}) or a list of such objects.
#' @param container Logical variable. Should the LaTeX code in \code{latexTable}
#'   be inserted into a LaTeX file that contains a complete LaTeX preamble and
#'   the \code{\\begin{document}} and \code{\\end{document}} tags? If you want 
#'   \code{latexTablePDF} to produce a PDF file, \code{container} must be 
#'   \code{TRUE}. But if you just want to write \code{latexTable} to disk as 
#'   a .tex file that you will insert into your own larger LaTeX document,  
#'   \code{container} should probably be \code{FALSE}.  
#' @param containerFilename A string. Specifies the path of the "container" 
#'   LaTeX file into which the \code{latexTable} table or tables will be 
#'   inserted to make a complete LaTeX file that can be rendered as PDF. By
#'   default, it is "tableContainer.tex", which is included in this package.
#' @param outputFilenameStem A string. It is the path and name of the file   
#'   is to be saved to disk, up to the extension. For example, if you want to  
#'   save "myTable.pdf" to disk, set \code{outputFilenameStem = "mytable"}.
#' @param writePDF Logical variable. Should a PDF file be saved to disk?
#' @param writeTex Logical variable. Should a .tex file be saved to disk?
#' @param overwriteExisting Logical variable. Should files to be saved overwrite
#'   existing files that have the same names?
#' @param verbose Logical variable. \code{latexTablePDF} calls  
#'   \code{pdflatex} to render PDF files, and if \code{verbose} is \code{TRUE}, 
#'   all of the output from \code{pdflatex} will be printed to screen. Useful 
#'   for debugging.\cr\cr\cr\cr 

#' @param continuedFloat Logical variable. Should be \code{TRUE} if the table 
#'   or tables to be rendered are part of a series and should all share the 
#'   same number. For example, \code{continuedFloat} should be true if 
#'   \code{latexTable} is a list of tables and you all want them to be 
#'   numbered as Table{\NB}3.  
#' @param continuedFloatStar Logical variable. Should be \code{TRUE} if the table 
#'   or tables to be rendered are part of a series, and if all tables should 
#'   share the same number but be distinguished by some secondary character.
#'   For example, \code{continuedFloatStar} should be true if \code{latexTable} 
#'   is a list of tables and you all want them to be numbered as Table{\NB}3a, 
#'   Table{\NB}3b, etc.\cr
#'     \indent \code{continuedFloat} and \code{continuedFloatStar} cannot both 
#'   be \code{TRUE}.
#' @param firstPageEmpty Logical variable. If \code{TRUE}, the page that 
#'   contains the first table in \code{latexTable} will have an empty header
#'   and footer. (Technically, \code{latexTablePDF} will insert 
#'   \code{\\thispagestyle{empty}} into the code block that contains the first
#'   table in \code{latexTable}.)    
#' @param firstTableNumber Integer. What number should the first table in 
#'   \code{latexTable} have?\cr
#'   \indent By default, the table numbering will be "natural." That is, the 
#' number of the first \code{latexTable} table will be determined
#' by the number of preceding tables in the document. For example, if 
#' you use \code{latexTablePDF()} to create a .tex file that you then insert 
#' into a larger LaTeX document, and if the .tex file is preceded in the 
#' LaTeX document by two tables, the first table created by \code{latexTable()}
#' will be Table{\NB}3.\cr\cr\cr\cr
#' 
#' @param openPDFOnExit Logical variable. Open the PDF file after it is created
#'   by \code{latexTablePDF}? This argument has an effect only on Windows, and
#'   only if \code{writePDF} is \code{TRUE}.     



#' @export
latexTablePDF <- function(
  latexTable,
  container          = TRUE,                  # if FALSE, .tex file can't be PDF'd
  containerFilename  = "tableContainer.tex",  # included in the Bullock package
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
  

  # TRANSFORM latexTable TO A LIST IF NEED BE
  # The first argument to the function is "latexTable". The user can specify 
  # that latexTable is either a single latexTable object or a list of such
  # objects. If the user specifies a single latexTable object, we now convert
  # it to a list.
  if ('latexTable' %in% class(latexTable)) {
    latexTable <- list(latexTable)
  }

  
  # PRELIMINARIES AND ERROR-CHECKING
  lT_listClass               <- sapply(latexTable, function (x) 'latexTable' %in% class(x))  
  lt_headerFooterNotIncluded <- sapply(latexTable, function (x) all(grepl('\\\\tabularnewline', x)))
  lt_duplicatedCommandNames  <- sapply(latexTable, function (x) x[1]) %>% 
    duplicated %>% 
    any
  lt_duplicatedLabels        <- sapply(latexTable, function (x) {
      x <- paste0(x, collapse = '')  # make latexTable object into a single string
      gsub('.*\\\\label\\{(.*?)\\}.*', '\\1', x)  # return the label
    }) %>%
    duplicated %>%
    any
  
  
  
  if ( ! all(lT_listClass) ) {
    stop("Every element in the latexTable list must be of the 'latexTable' class.")
  }
  if ( any(lt_headerFooterNotIncluded) ) {
    stop(
      stringr::str_wrap('It seems that at least one latexTable object was created with "headerFooter = FALSE". latexTablePDF() cannot work with latexTable objects unless they have headers and footers.')
    )
  }
  if (lt_duplicatedCommandNames) {
    stop(
      stringr::str_wrap('At least two of the latexTable objects share the same command name (that is, the same macro name). LaTeX cannot handle duplicated command names. Change them by running latexTable() while using the "commandName" argument to specify a different command name for each table.')
    )    
  }
  if (lt_duplicatedLabels){
    stop(
      stringr::str_wrap('At least two of the latexTable objects share the same label. Problems will arise if you try to cross-reference your tables in your LaTeX document -- for example, with "\\ref" or "\\pageref".')
    )        
  }
  
  
  # CHECK FOR PDFLATEX
  # This is a time-consuming operation. So if we find pdflatex the first time,  
  # we create the ".pdflatex_found" variable in the user's environment. In 
  # future runs of latexTablePDF(), the check aborts if ".pdflatex_found" 
  # exists.  [2020 01 01]
  if (writePDF && !exists(".pdflatex_found")) {
    if (!nzchar(Sys.which("pdflatex")))  {
      stop("pdflatex doesn't seem to be on your path. A PDF file can't be created.")
    }
    else {
      assign(".pdflatex_found", TRUE, envir = parent.frame())
    }
  }
  
  
  
  # CHECK FOR INSTALLED PACKAGES
  # This is a time-consuming operation. To be efficient, we create
  # ".latex_packages_found", a boolean, in the user's environment the first 
  # time in the user's session that the packages are found. In subsequent
  # calls to latexTablePDF(), the check is aborted if ".latex_packages_found"
  # exists.  [2020 01 01]
  #
  # TODO: The .latex_packages_found variable is created if packages are found
  # -- but the packages that are checked depend on the user's latexTable call.
  # For example, if the user isn't working with landscaped tables, 
  # .latex_packages_found may be created even if the "pdflscape" LaTeX package
  # isn't installed. That could lead to problems if the user wants to create a
  # landscaped table later in the session. We could get around this by 
  # making .latex_packages_found a string that tells us exactly which packages
  # have been installed.  [2020 01 01]
  kpsewhichExists <- nzchar(Sys.which("kpsewhich"))  # LaTeX package-checking tool
  if (containerFilename=='tableContainer.tex' && kpsewhichExists && !exists(".latex_packages_found")) {
    latexTable_collapsed <- sapply(latexTable, paste0, collapse = '')
    tablesAreLandscaped <- grepl("\\\\begin\\{landscape\\}", latexTable_collapsed) %>%
      any
    requiredPackageList  <- qw("array booktabs caption numprint")
    if (containerFilename == 'tableContainer.tex') {
      requiredPackageList <- c(requiredPackageList, qw("fancyhdr footmisc geometry ragged2e"))
    }
    if (tablesAreLandscaped) {
      requiredPackageList <- c(requiredPackageList, qw("afterpage pdflscape"))
    }
    installedPackageList <- system2(
      command = "kpsewhich", 
      args    = paste0(requiredPackageList, ".sty", collapse=' '), 
      stdout  = TRUE)
    if (length(installedPackageList) < length(requiredPackageList)) {  # if a package is missing
      # Stop if trying to write PDF, warn if only trying to write .tex.
      missingPackageString(installedPackageList, requiredPackageList, writePDF, writeTex)  
    }
    else {
      assign(".latex_packages_found", TRUE, envir = parent.frame())
    }
  }
    
  # Further warnings and errors
  if (writePDF && file.exists(paste0(outputFilenameStem, '.pdf')) && overwriteExisting) {
    message(paste0(outputFilenameStem, '.pdf will be overwritten.'))
  } else if (writePDF && file.exists(paste0(outputFilenameStem, '.pdf')) && !overwriteExisting) {
    stop(paste0(outputFilenameStem, '.pdf already exists and overwriteExisting = FALSE.'))
  }
  if (writeTex && file.exists(paste0(outputFilenameStem, '.tex')) && overwriteExisting) {
    message(paste0(outputFilenameStem, '.tex will be overwritten.'))
  } else if (writeTex && file.exists(paste0(outputFilenameStem, '.tex')) && !overwriteExisting) {
    stop(paste0(outputFilenameStem, '.tex already exists and overwriteExisting = FALSE.'))
  }
  if (continuedFloat && continuedFloatStar) {
    stop('"continuedFloat" and "continuedFloatStar" cannot both be TRUE.')    
  }    
  if (container && is.null(containerFilename)) {
    stop('if "container" is TRUE, "containerFilename" must not be NULL.')
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
  
  
  # CHECK FOR CALL TO TABLE MACRO, AND INSERT SUCH A CALL IF NECESSARY
  # If writePDF is TRUE but a table in the latexTable list doesn't conclude 
  # with a call to the table-generating macro (because it was produced with a 
  # call to latexTable() in which callCommand was FALSE), pdflatex will 
  # generate an empty file. We need to prevent that. So we check each table in  
  # the list for a call to the macro, and insert it if it isn't there.
  #   macroNameIndex, macroName, fileCallsMacroAtEnd, macroInserted, and 
  # macroCall are all vectors -- not lists. macroNameIndex will typically be a 
  # vector of ones (for example, c(1, 1, 1)).  [2020 02 22]
  macroNameIndex <- sapply(latexTable, function (x) grepl("\\\\newcommand\\\\\\w+\\[\\d\\]\\{%", x), simplify = FALSE) %>% 
    sapply(which) %>% 
    sapply(max)
  macroName <- mapply(
    function (x, index) sub("\\\\newcommand\\\\(\\w+).*", "\\1", x[[index]]),
    latexTable,
    macroNameIndex)
  fileCallsMacroAtEnd <- mapply(
    function (x, macroName) any( grepl(paste0("^\\\\", macroName, "\\{\\w\\}"), x)),
    latexTable,
    macroName)
  macroInserted <- rep(FALSE, length(latexTable))  # not inserted yet, in any case
  macroCall <- rep(NA, length(latexTable))
  if (writePDF) {
    for (i in 1:length(latexTable)) {
      if (!fileCallsMacroAtEnd[[i]]) {
        macroCall[i] <- paste0("\\", macroName[[i]], "{p}")
        latexTable[[i]] <- c(latexTable[[i]], macroCall[i])
        macroInserted[i] <- TRUE
      }
    }
  }

  
  # WRAP THE LATEX DOCUMENT IN THE CONTAINER
  if (container) {
    if (containerFilename == 'tableContainer.tex') {
      latexContainer <- readLines(system.file(containerFilename, package = "Bullock", mustWork = TRUE))
    }    
    else {
      latexContainer <- readLines(containerFilename)
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
  
  
  # CREATE FILES IN A TEMPORARY DIRECTORY
  # We create files in a temporary directory so that we don't have to clean up
  # the files created by pdflatex: .aux, .log, etc. Also so that we don't 
  # clutter the user's directory if the function aborts for any reason.
  # [2020 01 02]
  tmpFilename <- tempfile(fileext = '.tex')  # includes path to tempdir()
  writeLines(newTable, tmpFilename)
  if (writePDF) {  
      system2(
        command = 'pdflatex', 
        args    = c(
          paste("-output-directory", shQuote(tempdir())),
          shQuote(tmpFilename) 
        ),
        stdout = if (verbose) "" else FALSE,
        stderr = if (verbose) "" else FALSE
      )
  }

  
  # REMOVE MACRO CALL FROM THE .TEX FILE IF NECESSARY
  # If writePDF is TRUE but the latexTable didn't include a macro call at the
  # end -- that is, if it was generated by a call to latexTable() for which 
  # callCommand was FALSE -- I inserted a macro call so that the PDF could be
  # created. Now that it has been created, I remove the macro call.
  #   At this stage, we have already written all of the tables in the 
  # latexTable list to a single .tex file. So we load that file, remove the 
  # relevant lines, and then save the new shorter file back to the same 
  # filename.  [2020 02 21]
  if (any(macroInserted)) {
    texFile <- readLines(tmpFilename)
    for (i in 1:length(macroInserted)) {
      if (macroInserted[i]) {
        macroCallIndex <- grepl(macroCall[i], texFile, fixed = TRUE) %>% which  
        texFile <- texFile[-macroCallIndex]
      }
    }
    writeLines(texFile, tmpFilename)
  }
  
  
  
  # MOVE PDF TO FINAL DESTINATION, THEN DELETE THE TEMPORARY DIRECTORY
  if (writePDF) {
    file.copy(
      from      = sub("\\.tex", "\\.pdf", tmpFilename), 
      to        = normalizePath(paste0(outputFilenameStem, '.pdf'), mustWork = FALSE),
      overwrite = overwriteExisting)
  }
  if (writeTex) {
    file.copy(
      from      = tmpFilename, 
      to        = normalizePath(paste0(outputFilenameStem, '.tex'), mustWork = FALSE),
      overwrite = overwriteExisting)
  }

  
  # OPEN THE NEW PDF FILE (IF RUNNING WINDOWS)
  if (writePDF && openPDFOnExit && Sys.info()['sysname'] == 'Windows') {
    PDFFullPath <- normalizePath(paste0(outputFilenameStem, '.pdf'))
    shell.exec(PDFFullPath)
  }
  
}
  
