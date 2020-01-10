#' Create a LaTeX table from a matrix.
#' 
#' \code{latexTable()} takes a single matrix, \code{mat}. By default, it returns 
#' a LaTeX macro that creates a well-formatted LaTeX table. 


#' @return An object of classes \code{latexTable} and \code{character}. The 
#' returned object is a vector of strings of LaTeX code; each string is a line
#' in a LaTeX macro that can create a table. (There is one small exception. If
#' \code{callCommand} is \code{TRUE}, the last line is not part of the macro; 
#' instead, it calls the macro, thereby telling LaTeX to display the table).


#' @note
#' \emph{Required LaTeX packages.} The LaTeX code produced by the  
#'  \code{latexTable} makes use of capabilities provided by the \code{array}, 
#'  \code{booktabs}, \code{caption}, and \code{numprint}{\NB}LaTeX packages---and,
#' for landscaped tables, the \code{afterpage} and \code{pdflscape}{\NB}packages.
#' If you haven't installed those LaTeX packages, you won't be able to render 
#' the tables produced by \code{latexTable}.\cr\cr
 
#' \emph{Changes from pre-release versions:}
#' * The names of some arguments have 
#' changed slightly since the pre-release versions of this function. They have  
#' been changed to enforce consistency: camelCase is used for all arguments,  
#' and every acronym is followed by an underscore (_) character. We thus have  
#' \code{SE_table} instead of \code{SEtable}, \code{tabColSep} instead of 
#' \code{tabcolsep}, and so{\NB}on.
#' * Some default arguments have changed. In particular, the default 
#' \code{spacerColumns} argument is no longer \code{NULL}. Instead, the 
#' default is to insert spacer columns in appropriate places. See documentation
#' of the \code{spacerColumns} argument for details.
#' @md

#' @concept Tufte
#' @concept table
#' @concept tables
#' @seealso Other functions for making tables: \linkInt{regTable}, 
#' \linkInt{latexTablePDF}. See also the \href{../doc/tables.html}{Building better tables in less time} 
#' vignette. 


#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
#' lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' lm3 <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' rT1 <- regTable(list(lm1, lm2, lm3))
#' latexTable(rT1)
#' latexTable(rT1, SE_table = FALSE, colNames = lt_colNumbers())
#' lt2 <- latexTable(
#'   mat      = rT1, 
#'   colNames = list(qw("model model model"), qw("1 2 3")))
#' lt3 <- latexTable(
#'   mat         = rT1, 
#'   colNames    = lt_colNumbers(),
#'   rowNames    = c(
#'     "Intercept", 
#'     "Petal length", 
#'     "Petal width", 
#'     "Petal length $\\times$ petal width"),
#'   footerRows  = list(lt_nobsRow(), lt_rSquaredRow()),
#'   commandName = 'mainEstimates',
#'   caption     = "Each entry is an estimate or a standard error from a separate OLS regression.")
#' lt4 <- update(
#'   lt3,
#'   commandName = 'myEstimates',  # change name of LaTeX macro
#'   spacerRows  = 1)              # add vertical space after intercept row


#' @param mat Matrix of information to be displayed in a LaTeX table.
#' @param SE_table Logical variable that indicates whether \code{mat} contains
#'   pairs of columns, with the first column in each pair containing estimates,
#'   and the second column containing the corresponding standard errors. 
#'   (Matrices returned by \linkInt{regTable} have this form.)
#'   Defaults to \code{TRUE}. If \code{TRUE}, the even-numbered columns of \code{mat}
#'   will be rendered in smaller type than the odd-numbered columns: that is,
#'   the standard errors will be rendered in smaller type than their 
#'   corresponding estimates. This default type sizing can be overridden by 
#'   the \code{SE_fontSizeString} argument. 
#' @param headerFooter Logical variable. If \code{TRUE}, which is the default,
#'   the output will be (or at least include) a LaTeX macro that generates a 
#'   table. For example, you will be able to produce a table simply by calling
#'   \code{\\myTable{p}} or \code{\\myTable{h}} in your master LaTeX document.\cr
#'   \indent If \code{headerFooter} is \code{FALSE}, the only output of the 
#'   function will be LaTeX code for "data rows"---one row for each row of 
#'   \code{mat}. 
#' @param commandName A string. It is the name of the macro that produces the
#'   LaTeX table (if \code{headerFooter} is \code{TRUE}). By default, it is 
#'   "myTable"; you can change it to something more descriptive, e.g., 
#'   "mainEstimates". 
#' @param callCommand Logical variable. Should the last line of the
#'   \code{latexTable} object be a a call to the macro that creates the table?
#'   If \code{callCommand} is \code{TRUE}, which is the default, sourcing a 
#'   file that contains \code{latexTable} output---that is, by using
#'   \code{\\input} or \code{\\include} in your master LaTeX document---will 
#'   produce a table when that master LaTeX document is rendered. If
#'   \code{callCommand} is \code{FALSE}, sourcing the file will make the macro
#'   available in your LaTeX document, but it will not call the macro. (You 
#'   will need to call the macro yourself by adding a line like 
#'   \code{\\myTable{p}} to your LaTeX document.)
#' @param label A string. Specifies the LaTeX label for a table. It is not printed  
#'   anywhere in the table, but references to the figure in 
#'   your LaTeX document (for example, references created by \code{\\ref} or 
#'   \code{\\autoref}) must include the label name. For simplicity, the 
#'   default \code{label} is the same as the \code{commandName} argument.\cr\cr\cr\cr


#' @param landscape Logical variable. Determines whether the table is printed 
#'   in landscape or in portrait mode. Affects the output only if if 
#'   \code{headerFooter == TRUE} and \code{callCommand == TRUE}.
#' @param floatPlacement Character vector of length{\NB}1. Acceptable values 
#'   are \code{p} (the default, which places each table on its own page), 
#'   \code{h, H, t, b,} and \code{!}. Affects the output only if 
#'   \code{landscape} is \code{FALSE}. See the 
#'   \href{https://bit.ly/LaTeX-floats}{LaTeX wikibook} for more on float 
#'   placement in LaTeX. 
#' @param starredFloat Logical variable that indicates whether the LaTeX 
#'   table should be specified with \code{table*} instead of \code{table}. The
#'   default is FALSE, but you may want to set it to TRUE if you want you are 
#'   using a multi-column page layout in LaTeX and want the table to cross 
#'   both columns.
#' @param horizOffset A string that specifies a LaTeX length, e.g., ".25in".
#'   When the LaTeX code produced by \code{latexTable} is rendered, the table
#'   will be moved to the right by this length (or to the left if the length 
#'   is negative, e.g., "-.25in").\cr\cr\cr\cr
#' 
#' 
#' @param rowNames Character vector of labels for the rows in \code{mat}. The
#'   labels will be printed to the left of each row in \code{mat}.
#'   \code{rowNames} can be \code{NULL}.
#' @param footerRows List, or object that can be coerced to a list, of footer
#'   rows. Information about N and \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} 
#'   is typically included in \code{footerRows}. Each element in the list 
#'   corresponds to a row in the footer. The first entry in each 
#'   \code{footerRows} list-element should be the row name for the corresponding 
#'   footer row (e.g., '$F$', '$R^2$').\cr
#'     \indent By default, the only footer row indicates the number of 
#'   observations for each model in \code{mat}.
#' @param colNames List, or object that can be coerced to a list, of column
#'   headings. Typically, each element in the list is a character vector, and 
#'   the elements of the character vector specify the names of the table's
#'   columns.\cr 
#'     \indent If \code{SE_table} is \code{TRUE} (the default), each column 
#'   name will appear over a pair of columns. In this case, each element in 
#'   the \code{colNames} list should contain \code{ncol(mat)/2} entries.\cr
#'     \indent To specify multi-line column labels, use a list with multiple 
#'   elements. The entries in the first list element will then appear in the 
#'   top row of the column label, the entries in the second list element will 
#'   appear in the next row of the column label, and so{\NB}on.\cr
#'     \indent By default, column names will be taken from \code{colnames(mat)}.
#'   If \code{colnames(mat)} is \code{NULL}, columns will be numbered "(1)", 
#'   "(2)", etc. See \linkInt{lt_colNames_default} for more information. 
#' @param colNameExpand Logical variable. By default, an entry of \code{\out{''}} in a
#'   \code{colNames} list element---that is, an empty entry---indicates that a 
#'   column should have no column heading. But if \code{colNameExpand} is 
#'   \code{TRUE} and a text entry in a \code{colNames} list element is 
#'   followed by one or more \code{\out{''}} entries, the column name specified by the text
#'   entry will bridge the columns that have \code{\out{''}} entries.\cr
#'     \indent \code{colNameExpand} and \code{spacerColumns} do not play well 
#'   together. If you run \code{latexTable} with \code{colNameExpand == TRUE}
#'   and a non-NULL \code{spacerColumns} argument, you will get LaTeX output,
#'   but you will probably need to edit the "\\multicolumn" and "\\cmidrule"
#'   commands in the output so that LaTeX can render the output.\cr\cr\cr\cr
#' 
#'
#' @param extraRowHeight A string that specifies a length that LaTeX 
#'   recognizes, e.g., '2pt' or '.25in'. The \code{extrarowheight} length in 
#'   LaTeX will be set to \code{extraRowHeight}. In practice, this means that
#'   the vertical space between every row will be increased by 
#'   \code{extraRowHeight}. This argument has no effect if \code{headerFooter} 
#'   is \code{FALSE}.   
#' @param spacerColumns A vector of integers. Specifies columns in \code{mat} 
#'   after which to insert columns that contain no entries. These "spacer 
#'   columns" are used to insert horizontal space into the typeset table. By 
#'   default, spacerColumns are specified by a helper function, 
#'   `spacerColumns_default()`:
#'  * If \code{SE_table} is \code{FALSE}, there is a spacer column between 
#'    every column in \code{mat}.
#'  * If \code{SE_table} is \code{TRUE}, there is a spacer column after  
#'    every even-numbered column in \code{mat}, except for the last column.  
#'  * If \code{rowNames} is not \code{NULL}, a spacer column is inserted 
#'    between the table's row names and the first column of data.\cr
#'     \indent To add a spacerColumn between the rownames and the first data column, 
#'   make 0 one of the values in spacerColumns.\cr
#'     \indent \code{colNameExpand} and \code{spacerColumns} do not play well 
#'   together. If you run \code{latexTable} with \code{colNameExpand == TRUE}
#'   and a non-NULL \code{spacerColumns} argument, you will get LaTeX output,
#'   but you will probably need to edit the "\\multicolumn" and "\\cmidrule"
#'   commands in the output so that LaTeX can render the output.\cr
#'     \indent See below for a technical note on \code{spacerColumns} and column  
#'   spacing in LaTeX.
#' @md

#' @param spacerColumnsWidth Either a single string of a recognizable LaTeX 
#'   length (e.g., '.5em') or a character vector indicating the width of each 
#'   spacer column. Has no effect unless \code{headerFooter} is \code{TRUE}.
#' @param spacerRows A vector of integers. After each row in \code{mat} whose  
#'   number is in \code{spacerRows}, a vertical space of \code{spacerRowsHeight}
#'   will be printed. For example, if \code{spacerRows == c(2, 4)}, a vertical 
#'   space will be added after rows 2 and 4 of \code{mat}.
#' @param spacerRowsHeight A string that specifies a recognizable LaTeX length,
#'   e.g., ".15in".
#' @param tabColSep Character vector indicating a length that LaTeX recognizes,
#'   e.g., ".25in". The \code{tabcolsep} value in LaTeX will be set to this 
#'   value if \code{headerFooter} is \code{TRUE}. If \code{SE_table} is 
#'   \code{TRUE}, \code{tabColSep} will be the default distance between the  
#'   estimate and the SE column in each column pair, and it will be half of the 
#'   distance between column pairs. If \code{SE_table} is FALSE, 
#'   \code{tabColSep} will simply be half of the default distance between 
#'   columns. These distances between columns can be increased by the
#'   \code{spacerColumns} argument.
#' @param spaceBetweenColNameRows Logical variable. If \code{TRUE}, it adds a 
#'   little space between the rows that specify column names. It has an effect
#'   only when the column names are split across multiple rows, i.e., when 
#'   \code{length(colNames) > 1}.
#' @param columnTierSeparator A string. In the LaTeX code generated by 
#'   \code{latexTable}, all columns are separated from each other by " & ".  
#'   Column tiers -- that is, pairs of columns giving the estimate and the SE 
#'   for a particular coefficient -- are further separated by 
#'   \code{columnTierSeparator}, which defaults to two spaces ('  '). This 
#'   option affects only the LaTeX code produced by \code{latexTable}; it 
#'   exists to make the LaTeX code more readable. It does not affect the 
#'   typeset (e.g., PDF) version of the table.\cr\cr\cr\cr
#'
#'   
#' @param printCaption Logical variable.
#' @param caption A string. It can include LaTeX commands, e.g., 
#'   "\\\\textit{Results from a minimal specification.}" It can also include 
#'   references to other labeled parts of your LaTeX document, e.g., 
#'   "\\\\autoref{SomeFigure}". See the examples.
#' @param captionMargins A vector of two strings that specify the margins of  
#'   the caption. The strings should be LaTeX lengths, e.g., ".25in" or ".67em".
#'   By default, \code{captionMargins} is \code{NULL}.\cr\cr\cr\cr
 

#' @param formatNumbers Logical variable. Pretty-print the entries in mat, 
#'   e.g., by adjusting the number of digits after the decimal place.
#' @param decimalPlaces Integer. If \code{formatNumbers} is \code{TRUE}, table 
#'   entries will be shown to this decimal place. For example, if 
#'   \code{decimalPlaces==2}, both "3.0035" and "3" will become "3.00."\cr
#'     \indent If \code{formatNumbers} is \code{FALSE}, entries will not be adjusted,
#'   but \code{decimalPlaces} will still be used to determine the widths of 
#'   columns and some aspects of column spacing.
#' @param SE_fontSizeString A string. Indicates how standard errors are to be 
#'   formatted when \code{SE_table} is \code{TRUE}. Defaults to 
#'   \code{\\\\fontsize{10.3bp}{10.3bp}\\\\selectfont}, which renders standard
#'   errors in slightly smaller type than the corresponding estimates.  
#' @param NA_text A string. \code{NA} entries in \code{mat} will be replaced 
#'   by the string.\cr\cr\cr\cr
#' 
#' 
#' @param clipboard Logical variable. Copy entire output to clipboard. 
#'   Useful if you want to paste the output directly into a \code{.tex} file.
#'   Works only on Windows.


# TODO: 
# --See if I can use clipr::write_clip to copy to clipboard for non-Windows
#   systems. Try with Ubuntu (in Windows).  [2019 12 08]
# --Add a numprint option to specify the number of digits in each 
#   numprint-specification-column, both before and after the decimal 
#   place.  [2015 02 22]
# --Simplify some of the regex search-and-replace just by using append() 
#   (which should be called insert()).  [2012 07 23]


# Programming notes:
# --str_pad() cannot handle NA values.  They will need to be converted to 
#   something else (e.g., "NA") before being passed to str_pad().
#   [2012 07 22]





#' @export
latexTable <- function(
  mat, 
  SE_table            = TRUE,
  headerFooter        = TRUE,
  commandName         = 'myTable',
  callCommand         = TRUE,
  label               = commandName,
  
  floatPlacement      = 'p',
  landscape           = if (SE_table) ncol(mat) / 2 >= 6 else ncol(mat) >= 6,
  starredFloat        = FALSE,
  horizOffset         = '-0in',
  
  rowNames            = rownames(mat), 
  footerRows          = lt_footer(),
  colNames            = lt_colNames_default(),
  colNameExpand       = FALSE,

  extraRowHeight      = if (SE_table) '2pt' else '4pt',
  spacerColumns       = lt_spacerColumns_default(),
  spacerColumnsWidth  = '.67em',
  spacerRows          = NULL,
  spacerRowsHeight    = '.15in',
  tabColSep           = '2.75pt',
  spaceBetweenColNameRows = TRUE, 
  columnTierSeparator = '  ',

  printCaption        = TRUE,
  caption             = paste0(label, ' caption goes here.'),
  captionMargins      = NULL, 

  formatNumbers       = TRUE,  
  decimalPlaces       = 2,  
  SE_fontSizeString   = '\\fontsize{10.3bp}{10.3bp}\\selectfont',
  NA_text             = '',

  clipboard           = FALSE) {
  
  
    
  # STORE CALL FOR LATER USE
  # In case user wants to update the latexTable object with update.latexTable.
  # [2019 12 19]
  latexTableCall <- match.call()   

  
  # GET PRELIMINARY INFORMATION  
  # These seemingly redundant lines are important.  Without them, changes to 
  # rownames(mat) and colnames(mat) will change rowNames and colNames, 
  # respectively, provided that the user doesn't specify the rowNames or 
  # colNames arguments.  The principle seems to be "default arguments that are
  # functions of other arguments can be modified until they are first called."
  # In other words, there is some very lazy evaluation at work.  [2012 07 22]
  #
  # TODO: can I get around this by using "force(rowNames)" and "force(colNames)"?
  # See http://adv-r.had.co.nz/Functions.html#all-calls.  [2019 12 20]
  # --Replacing the "decimalPlaces" assignment with force(decimalPlaces) 
  #   doesn't work; instead, I get an error from lt_rSquaredRow(). Perhaps   
  #   it would work if force(decimalPlaces) were called from lt_footer().
  #   Date: 2019 12 28
  # rowNames      <- rowNames
  # colNames      <- colNames 
  # decimalPlaces <- decimalPlaces  
  
  if (! is.null(colNames)) {
    colNames <- if (is.list(colNames)) colNames else list(colNames)
  }
  footerRows <- footerRows
  if (! is.null(footerRows)) {
    footerRows <- if (is.list(footerRows)) footerRows else list(footerRows)
  }
  landscape  <- landscape
  nrow       <- nrow(mat)
  ncol       <- ncol(mat)
  if (formatNumbers) {
    mat <- round(mat, decimalPlaces)
  }


  
  ############################################################################
  # CHECK ARGUMENTS 
  ############################################################################
  if (grepl('[^bhHpt!]', floatPlacement)) {
    stop('floatPlacement can contain only these characters: "p", "h", "H", "t", "b", and "!".')
  }
  if (SE_table && !is.null(colNames) && length(colNames[[1]]) != ncol/2) {
    stop("length of colNames[[1]], ", length(colNames), ", is not half of ncol(mat).")
  }
  if (grepl('&', columnTierSeparator)) {
    warning(stringr::str_wrap("columnTierSeparator includes an ampersand. This is likely to screw up the layout of your table.", 72, exdent = 2))
  }
  if (!is.null(captionMargins) && length(captionMargins) != 2) {
    stop("length(captionMargins) must be NULL or equal 2.")
  }
  if (! grepl('^[[:alpha:]]+$', commandName)) {
    stop("commandName has non-letter characters, but LaTeX macro names must contain only letters")
  }
  
  # SPACER COLUMNS
  if ( any(grepl('&', rowNames)) ) {
    if (! is.null(spacerColumns)) {
      stop("spacerColumns is non-NULL and there are ampersands (perhaps escaped ampersands) in your rowNames. This is a recipe for havoc.")
    }
    else {
      warning("the ampersands in your rowNames could screw up the table, even if they are escaped.")
    }
  }  
  if (!is.null(spacerColumns) && colNameExpand) {
    warning("You have specified spacerColumns and colNameExpand is TRUE. The output of the function probably won't be valid; you will probably need to adjust the header to get the column specifications right.")
  }  
  if(!is.null(spacerColumns) && max(spacerColumns) >= ncol) {
    stop("max(spacerColumns) must be less than ncol(mat).")
  }
  if (!is.null(spacerColumns) && (ncol %% 2 != 0) && headerFooter) {
    warning("spacerColumns is non-NULL, ncol(mat) is odd, and headerFooter == TRUE. This combination of options is unlikely to produce a table that will work in LaTeX.")
  }

  
 
  
  # ADJUST DIGIT SETTINGS  [2014]
  oldDigitsOption <- as.integer(options("digits"))
  options(digits = decimalPlaces + 1)
  on.exit(options(digits = oldDigitsOption))
  z <- paste(rep(0, decimalPlaces), collapse = '')  # zeroes to append
  
  
  # WORK ON THE MATRIX ROW BY ROW
  # First, convert each row to a string.  Then operate on the string.  
  # [2012 07 22]
  if (formatNumbers) {
    for (i in 1:nrow(mat)) {
  
      # NEW ATTEMPT TO PAD OUT ENTRIES WITH TRAILING ZEROES.  This "first 
      # strike" attempt uses str_pad().  [2014 06 21]  
      matChar <- as.character(mat[i,])                       # get row "i" of "mat"
      matChar <- gsub('^0$', paste0('0.', z), matChar)       # take care of entries that are simply "0"
      matChar <- gsub('^(-?\\d+)$', '\\1.', matChar)         # add decimal point to entries that contain only digits
      matCharAfterDecimal <- gsub('-?\\d+\\.', '', matChar)  # get characters after the decimal place, e.g., "12" in "-3.12"
      
      decimalPlacesToAdd <- decimalPlaces - nchar(matCharAfterDecimal)  # e.g., add two zeroes to one entry, zero zeroes to another
      for (ind in 1:length(matChar)) {     # for each entry in row "i" of "mat"
        if (is.na(matChar[ind])) { next }         # skip if NA
        else if (decimalPlacesToAdd[ind] >= 0) {
          matChar[ind] <- stringr::str_pad(       # if entry is too short, pad the entries with trailing zeroes
            string = matChar[ind],
            width  = nchar(matChar[ind]) + decimalPlacesToAdd[ind],
            side   = 'right',
            pad    = '0')
        }
        else if (decimalPlacesToAdd[ind] < 0) {
          matChar[ind] <- round(as.numeric(matChar[ind]), decimalPlaces)
        }
      }
      matLine <- paste(matChar, collapse = ' & ')
      
      # ADJUST FORMATTING OF NUMBERS
      # This is a much older attempt to pad out entries with trailing zeroes and
      # to process the entries in other ways.  It replaces '0' with '0.00', '123' 
      # with '123.00', etc.  [2011 02 17]     
      if (SE_table) {
        matLine <- sub('^(-?\\d+)\\s',    paste0('\\1.',  z, ' '), matLine)  # If the first entry in a row is X, this changes it to, e.g., X.00
        matLine <- gsub('\\s(-?\\d+)\\s', paste0(' \\1.', z, ' '), matLine)  # For subsequent entries, replace ' 1 ' with ' 1.00 ', ' -1 ' with ' -1.00 ', etc.
        matLine <- gsub('\\s(\\d+)$',     paste0(' \\1.', z),      matLine)
      }
      
      # Remove leading zeroes.  [2011 12 17]
      matLine <- gsub('^0\\.',   ' .', matLine)
      matLine <- gsub('\\s0\\.', ' .', matLine)
      matLine <- gsub('-0\\.',   '-.', matLine)
      
      # Replace, e.g., '.1' with '.10' [2011 02 17]
      # matLine <- gsub('(\\.\\d)\\s', '\\10 ', matLine)  # ".1 " becomes ".10 "
      # matLine <- gsub('(\\.\\d)$',   '\\10',  matLine)  # ".1"  becomes ".10"
      
      # Replace NA values [2012 07 22]
      if (! is.null(NA_text)) {
        matLine <- gsub(' NA', paste0(' ', NA_text), matLine)
        matLine <- gsub('NA',  paste0(' ', NA_text), matLine)
        matLine <- gsub(' & $', ' &  ', matLine)  
        # w/o this, strsplit will return vector of ncol - 1 (too short) 
      }
      
      # Replace the values in mat[i, ] with the new text-processed values from 
      # matLine.  [2012 07 22]
      mat[i, ] <- strsplit(matLine, ' & ', fixed = TRUE)[[1]]
    }
  }
  
  # PAD ENTRIES SO THAT ALL ENTRIES IN COLUMN HAVE EQUAL WIDTH  [2012 07 22]  
  # Column padding needs to be done after all of the text substitutions have 
  # been made -- taking out leading zeroes, etc.  This is padding in the 
  # columns that appear in the .tex file, not padding in the columns that 
  # will ultimately appear in the PDF file.  [2012 07 22]
  #
  # In the mapply() command, str_pad() works row-wise -- not column-wise -- on
  # the matrix that it receives.  But the column widths need to be applied 
  # column-wise.  This is why I transpose the matrix when passing it to 
  # mapply, then transpose it back again with the matrix() command.  
  # [2012 07 22]
  colWidths <- apply(mat, 2, function (x) max(nchar(x))) 
  mat <- mapply(stringr::str_pad, t(mat), width = colWidths, SIMPLIFY = TRUE)
  mat <- matrix(t(mat), nrow, ncol, byrow = TRUE)
  
  
  # CREATE OUTPUT VARIABLE
  # outputStrings is a vector of strings.  It holds the LaTeX code.
  # [2014 03 14]
  outputStrings <- NULL 
  
  
  # CREATE HEADER
  if (headerFooter) {
    
    # colDest is a vector describing the type of each column that comes after  
    # the column of rownames (if there is a column for the rownames).  It 
    # takes the values "est", "SE", or "spacer".  It's used for column 
    # specification and for\cmidrule commands.  [2012 07 24]
    if (SE_table) {
      colDest <- rep(c("est", "SE"), ncol/2)  #
    } else {
      colDest <- rep(c("est"), ncol)  #
    }
    if (! is.null(spacerColumns)) {
      for (i in rev(sort(spacerColumns))) {
        colDest <- append(colDest, "spacer", after = i)  
      }
    }    
    
    # Remove everything from decimal point on.  Then replace all NA cells 
    # with ''.
    tmpRegex <- paste0('\\s*', NA_text, '\\s*')
    if (nrow(mat) == 1) {
      tmp <- gsub('\\.\\d*', '', mat)                  
      tmp <- gsub(tmpRegex,  '', tmp)
    }
    else {
      tmp <- apply(mat, 2, function (x) sub('\\.\\d*', '', x))
      tmp <- apply(tmp, 2, function (x) sub(tmpRegex, '', x))
    }
    
    # leadingDigits is used for the numprint column specifications.  [2012 07 24]
    leadingDigits <- apply(tmp, 2, function (x) max(nchar(x)))  # get # of digits before decimal point in each data column 
    
    # Process spacerColumnsWidth.  If a different spacer column width was 
    # specified for each column, make spacerColumnsWidth into a stack from 
    # which I can shift values.  [2012 08 05]
    spacerColumnsWidth <- rep(spacerColumnsWidth, length(spacerColumns))      # no effect if spacerColumnsWidth already has correct length.    
    
    # Start to write the header.
    outputStrings <- c(outputStrings, paste0('\\newcommand\\', commandName, '[1]{'))
    if (starredFloat) { 
      outputStrings <- c(outputStrings, '  \\begin{table*}[#1]')
    } else {
      outputStrings <- c(outputStrings, '  \\begin{table}[#1]')      
    }
    outputStrings <- c(outputStrings, paste0('    \\setlength{\\extrarowheight}{', extraRowHeight, '}'))
    outputStrings <- c(outputStrings, '    \\begin{center}')
    if (! is.null (horizOffset)) {
      outputStrings <- c(outputStrings, paste0('      \\hspace*{', horizOffset, '}'))
    }
    outputStrings <- c(outputStrings, paste0('      \\setlength{\\tabcolsep}{', tabColSep, '}'))
    outputStrings <- c(outputStrings, '      \\begin{tabular}{%')
    if (! is.null(rowNames)) { 
      outputStrings <- c(outputStrings, '        r%')
    }
    for (i in colDest) {
      if (i == 'est' && SE_table) {
        outputStrings <- c(outputStrings, paste0('        >{{\\hspace*{0em}}}N{', shift(leadingDigits), '}{', decimalPlaces, '}%'))        
      } 
      else if (i == 'est' && !SE_table) {
        outputStrings <- c(outputStrings, paste0('        >{{\\hspace*{0em}}}N{', shift(leadingDigits), '}{', decimalPlaces, '}%'))        
      }
      else if (i == 'SE') {
        outputStrings <- c(outputStrings, paste0('        >{{', SE_fontSizeString, '}}N{', shift(leadingDigits), '}{', decimalPlaces, '}%'))
      }
      else if (i == 'spacer') {
        outputStrings <- c(outputStrings, paste0('        p{', shift(spacerColumnsWidth), '}%'))
      }
    }
    outputStrings <- c(outputStrings, '      }')  # ends "\begin{tabular}{"    
    if (! is.null(colNames)) {      
      
      
      ########################################################################
      # ADD \MULTICOLUMN COMMANDS FOR colNames  [2012 07 23] 
      ########################################################################
      # Recall that colNames is a list with multiple entries, to allow for 
      # column headings that span multiple lines.  So mcRow is a "row" in the
      # column headings.  [2014 04 27]
  	  #   colNameColsToSpan is a unique value for each colName. It indicates 
  	  # how many columns the colName should span. The numbering is as LaTeX
  	  # would see it, and spacer columns thus count as columns to span. 
  	  # colNameColsToSpan is used to generate the \cmidrule rule below the 
  	  # column name.  [2019 12 10]
      for (i in 1:length(colNames)) {              # for each element in the colNames list...
        mcRow <- colNames[[i]]                     # ...get the list element...
        if (colNameExpand && '' %in% mcRow) {      # ...if the list element - a character vector -- contains ''
          colNameStartPos   <- which(mcRow != '')  # ...colNameStartPos is the indices of all non-empty elements in the list element (mcRow)
          colNameColsToSpan <- c(colNameStartPos[-1], length(mcRow) + 1) - colNameStartPos
          if (is.null(spacerColumns) && SE_table) {
            colNameColsToSpan <- colNameColsToSpan * 2       # e.g., c(2, 1, 1) becomes c(4, 2, 2)            
          }
          else if (!is.null(spacerColumns) &&  SE_table) {
            colNameColsToSpan <- colNameColsToSpan * 3 - 1
          }
          else if (!is.null(spacerColumns) && !SE_table) {
            colNameColsToSpan <- colNameColsToSpan * 2 - 1
          }        
          colNameColsToSpan[colNameColsToSpan == 0] <- 1  
          mcRow <- mcRow[colNameStartPos]                    # eliminate empty entries from mcRow
          mcRow <- paste0(                                   # print a \multicolumn command for each entry in mcRow
            '        \\multicolumn{', 
      			colNameColsToSpan, 
      			'}{c}{', 
      			mcRow, 
      			'}')  
        }
        else {  # if !colNameExpand
          if (SE_table) {
            mcRow <- paste0('        \\multicolumn{2}{c}{', mcRow, '}')
          } else {
            mcRow <- paste0('        \\multicolumn{1}{c}{', mcRow, '}')
          }
        }
        mcRow <- stringr::str_pad(mcRow, max(nchar(mcRow)), side = 'right')
        mcRow <- paste(mcRow, '&')  # note: pastes a trailing ampersand that I'll later need to amputate
        if (colNameExpand && !is.null(spacerColumns)) {
          # If at this point, assume that there is a spacer column between 
          # each pair of adjacent \multicolumn commands.  [2014 04 27]
          mcRow[-length(mcRow)] <- paste0(mcRow[-length(mcRow)], '&')
        }
        # Adjust placement and spacing of ampersands.  This code exists 
        # partly to ensure that the correct number of ampersands appear in the
        # \multicolumn commands, and partly to ensure that the \multicolumn
        # commands are pretty-printed with correct indentation.  [2012 07 24] 
        if (is.null(rowNames) && 0 %in% spacerColumns) {
          mcRow    <- gsub('  \\m', '    \\m', mcRow,    fixed = TRUE)
          mcRow[1] <- sub ('  \\m', '& \\m',   mcRow[1], fixed = TRUE)  
        }
        else if (!is.null(rowNames) && !(0 %in% spacerColumns)) {
          mcRow    <- gsub('  \\m', '    \\m', mcRow,    fixed = TRUE)  
          mcRow[1] <- sub ('  \\m', '& \\m',   mcRow[1], fixed = TRUE)  
        }
        else if (!is.null(rowNames) && 0 %in% spacerColumns) {
          mcRow    <- gsub(' \\m',   '    \\m',  mcRow,    fixed = TRUE)  # adjust spacing for pretty-printing
          mcRow[1] <- sub ('   \\m', '&& \\m',   mcRow[1], fixed = TRUE)  # add spacer column between rowname and first column
        }
        
        # Adjust last \multicolumn command.  [2012 07 23]
        if (!colNameExpand && SE_table) {
          last.mcRowPos <- ncol / 2
        } 
        else if (!colNameExpand && !SE_table) {
          last.mcRowPos <- length(mcRow) 
        } 
        else if (colNameExpand && SE_table) {
          last.mcRowPos <- length(mcRow)
        } 
        else if (colNameExpand && !SE_table) {
          last.mcRowPos <- length(mcRow)  
        } 
        mcRow[last.mcRowPos] <- sub('\\s*&\\s*$', '', mcRow[last.mcRowPos])   # remove " & " from end of last \multicolumn command
        mcRow[last.mcRowPos] <- paste0(mcRow[last.mcRowPos], '\\tabularnewline')
        
        # Account for most spacerColumns.  The commands here don't account for 
        # a 0 value in spacerColumns, which indicates that a spacerColumn 
        # should be placed between the rownames and the first data columns.   
        # That particular kind of spacerColumn is handled above.  [2012 07 24]
        #   If colNameExpand == TRUE, spacer columns are handled above.  The 
        # column positions indicated in spacerColumns are ignored; instead, 
        # the assumption is that spacer columns are to appear between each 
        # column (or, if SE_table == TRUE, between each column pair).  
        # [2014 04 27]
        if (!colNameExpand && !is.null(spacerColumns) && SE_table) {
          scPos <- spacerColumns / 2  # divide by 2 to get pos. for spacerColumns amid \multicolumn commands
          mcRow[scPos] <- paste0(mcRow[scPos], '&')
        }
        else if (!colNameExpand && !is.null(spacerColumns) && !SE_table) {
          scPos <- spacerColumns 
          mcRow[scPos] <- paste0(mcRow[scPos], '&')
        }        
        outputStrings <- c(outputStrings, mcRow)
        if (spaceBetweenColNameRows && i < length(colNames)) {
          outputStrings <- c(outputStrings, '        \\addlinespace[-.025in]')
        }
      }
      
	  
      ########################################################################
      # ADD \CMIDRULE COMMANDS  [2012 07 23]
      ########################################################################
      # If expandColName == TRUE, the column positions indicated in 
      # spacerColumns are ignored.  Instead, the assumption is that spacer 
      # columns are to appear between each column (or, if SE_table == TRUE,   
      # between each column pair).  [2014 04 27]
      if (!colNameExpand && SE_table) {
        start      <- which(colDest == 'est') + !is.null(rowNames)
        end        <- which(colDest == 'SE')  + !is.null(rowNames)
      } 
      else if (!colNameExpand && !SE_table) {
        start      <- which(colDest == 'est') + !is.null(rowNames)
        end        <- which(colDest == 'est') + !is.null(rowNames)        
      }
      else if (colNameExpand) {
        colNameColsToSpanCume <- Reduce('+', colNameColsToSpan, accumulate = TRUE)  # e.g., c(1, 3, 5) becomes c(1, 4, 9)
        if (! is.null(spacerColumns)) {
		      for (i in 1:length(colNameColsToSpanCume)) {
            colNameColsToSpanCume[i] <- colNameColsToSpanCume[i] + i - 1  
          }
	      }
        start <- c(
          colNameStartPos[1],                                                       # index of first non-empty column name
		      colNameStartPos[1] + colNameColsToSpanCume[-length(colNameColsToSpanCume)] # + 1
        )
        end <- c(
          start + colNameColsToSpan - 1
          # start[-1] - 2,                                                   # all end values except the last 
          # start[length(start)] + colNameColsToSpan[length(start) - 1] - 1  # last end value
        )     
        
        # Adjust start and end to account for row names and a spacer column
        # that appears immediately after the row names.  [2014 04 27]
        start <- start + !is.null(rowNames) 
        start <- start + 0 %in% spacerColumns
        end   <- end + !is.null(rowNames) 
        end   <- end + 0 %in% spacerColumns
      }
      lrText       <- if (is.null(spacerColumns) || !SE_table) '(lr)' else '' 
      cmidruleLine <- paste0('\\cmidrule', lrText, '{', start, '-', end, '}', collapse = '')
      outputStrings <- c(outputStrings, paste0('        ', cmidruleLine))      
    }    
  }
  
  
  
  
  ############################################################################
  # PRINT TABLE ROWS (AFTER THE HEADER)
  ############################################################################
  subRegex <- paste0('\\1', columnTierSeparator)
  spacerColumnsTmp <- spacerColumns + !is.null(rowNames)  # adjust for extra ampersand if rowNames have been added
  if (! is.null(rowNames)) {
    rowNames <- stringr::str_pad(rowNames, max(nchar(rowNames)))
    mat <- cbind(rowNames, mat)
  }
  for (i in 1:nrow(mat)) {
    matLine <- paste(mat[i,], collapse = ' & ')
    
    # Add columnTierSeparator after each column tier (estimate-SE column 
    # pair).  [2012 07 23]
    numberOfAmpersands <- length(gregexpr('&', matLine)[[1]])
    if (numberOfAmpersands %% 2 == 0) { 
      rownamePrefix <- sub('(?<=& ).*', '', matLine, perl = TRUE)
      matLine       <- sub('.*?& ', '', matLine)              # remove rownamePrefix
      matLine       <- gsub('(.*?&.*?&)', subRegex, matLine)  # do substitution
      matLine       <- paste0(rownamePrefix, matLine)         # recombine
    }
    else {
      matLine <- gsub('(.*?&.*?&)', subRegex, matLine)
    }
    
    if (!is.null(spacerColumns)) {      
      for (j in rev(sort(spacerColumnsTmp))) {
        myAmpSubRegex <- paste0('^((?:.*?&){', j, '})')
        matLine <- sub(myAmpSubRegex, '\\1&', matLine)
      }
    }      
    
    # When rowNames is NULL but a spacer column appears at position 0, matLine
    # starts with an ampersand right next to a digit.  This line adds a space
    # between the ampersand and the digit.  [2012 07 25]
    matLine <- sub('^&(\\d)', '& \\1', matLine)
    matLine <- paste0('        ', matLine, '\\tabularnewline')
    
    # Add matLine to vector of rows to be printed.
    outputStrings <- c(outputStrings, matLine)
    
    # Add spacer rows.  [2015 02 14]
    if (i %in% spacerRows) {
      outputStrings <- c(outputStrings, paste0('        \\addlinespace[', spacerRowsHeight, ']'))
    }
  }
  
  # CREATE FOOTER
  if (headerFooter) {
    if (! is.null(footerRows)) {
      outputStrings <- c(outputStrings, '        \\addlinespace[.15in]')
      for (i in footerRows) {
        footerRow <- unlist(i)
        
        # Break off the rowname.  [2012 07 25]
        if (! is.null(footerRow)) {
          footerRowName <- shift(footerRow)
          footerRowIsSpacer <- grepl('^\\\\addlinespace', footerRowName)
          outputStrings <- c(outputStrings, paste0('        ', footerRowName))
          if (0 %in% spacerColumns & !footerRowIsSpacer) {
            outputStrings <- c(outputStrings, ' && ')      
          }
          else if (!footerRowIsSpacer) {
            outputStrings <- c(outputStrings, ' & ')            
          }
        }
        
        # Eliminate leading zeroes for R^2.  [2013 03 14]
        if (footerRowName %in% c('$R^2$', 'R$^2$')) {
          footerRow <- gsub('^0(\\.\\d+)$', '\\1', footerRow)
        }
        
        # Add trailing zeroes for R^2 and SER, e.g., change "1.9" to "1.90" or
        # 1.9000 as needed.  [2019 12 30]
        if (footerRowName %in% c('$R^2$', 'R$^2$', 'SER', 'Std. error of regression', 'Standard error of regression')) {
          for (i in 1:length(footerRow)) {
            ncharAfterDecimal  <- gsub('\\d*\\.', '', footerRow[i]) %>% nchar
            decimalPlacesToAdd <- decimalPlaces - ncharAfterDecimal
            footerRow[i] <- stringr::str_pad(
              string = footerRow[i],  
              width  = nchar(footerRow[i]) + decimalPlacesToAdd,
              side   = 'right',
              pad    = '0')
          }
        }
        
        if (!footerRowIsSpacer) { 
          # Construct the \multicolumn statements for footerRow.  [2012 07 25]
          footerRow           <- paste0('          \\multicolumn{2}{c}{', footerRow     , '} &')        
          footerRow[ncol / 2] <- sub('\\s*&\\s*', '', footerRow[ncol / 2])
          footerRow[ncol / 2] <- paste0(footerRow[ncol / 2], '\\tabularnewline')
          
          # Account for most spacerColumns.  The commands here don't account for 
          # a 0 value in spacerColumns, which indicates that a spacerColumn should
          # be placed between the rownames and the first data columns.  That 
          # particular kind of spacerColumn is handled above.  [2012 07 24]  
          if (!is.null(spacerColumns)) {
            scPos <- spacerColumns / 2  # divide by 2 to get pos. for spacerColumns amid \multicolumn commands
            footerRow[scPos] <- paste0(footerRow[scPos], '&')
          }
          outputStrings <- c(outputStrings, footerRow)
        }
      }
    }
    
    outputStrings <- c(outputStrings, '        \\bottomrule')
    outputStrings <- c(outputStrings, '      \\end{tabular}')
    if (printCaption) {
      if (is.null(captionMargins)) {
        outputStrings <- c(outputStrings, '      %\\captionsetup{margin={.75in, .75in}}')
      } else {
        outputStrings <- c(outputStrings, paste0('      \\captionsetup{margin={', captionMargins[1], ', ', captionMargins[2], '}}'))
      }
      outputStrings <- c(outputStrings, '      \\caption{%')
      outputStrings <- c(outputStrings, paste0('        \\label{', label, '}%'))
      outputStrings <- c(outputStrings, paste0('        ', caption))
      outputStrings <- c(outputStrings, '      }')
    }
    outputStrings <- c(outputStrings, '    \\end{center}')
    if (starredFloat) {
      outputStrings <- c(outputStrings, '  \\end{table*}')
    } else {
      outputStrings <- c(outputStrings, '  \\end{table}')
    }
    outputStrings <- c(outputStrings, '}%')
    if (callCommand) {
      if (landscape) {
        outputStrings <- c(outputStrings, '\\afterpage{')
        outputStrings <- c(outputStrings, '  \\begin{landscape}')
        outputStrings <- c(outputStrings, '    \\thispagestyle{empty}')
        outputStrings <- c(outputStrings, paste0('    \\', commandName, '{t}'))
        outputStrings <- c(outputStrings, '  \\end{landscape}')
        outputStrings <- c(outputStrings, '  \\clearpage')
        outputStrings <- c(outputStrings, '}')
      }
      else {
        outputStrings <- c(outputStrings, paste0('\\', commandName, '{', floatPlacement, '}'))
      }
    }
  }
  
  
  # RETURN THE latexTable OBJECT
  class(outputStrings) <- c('latexTable', class(outputStrings))
  attr(outputStrings, "call") <- latexTableCall
  if (clipboard && Sys.info()['sysname'] == 'Windows') {
    utils::writeClipboard(paste0(outputStrings, collapse = "\n"))
  }
  outputStrings
}  




##############################################################################
# HELPER UTILITIES (NOT NEW METHODS FOR GENERICS) 
##############################################################################



#' Compute default column names for latexTable objects.
#' 
#' If \code{colnames(mat)} is not \code{NULL}, this function will use 
#' \code{colnames(mat)} as the \code{colNames} argument in \code{latexTable()}.
#' If \code{colnames(mat)} is \code{NULL}, column names will be determined by
#' \linkInt{lt_colNumbers}.
#' 
#' The function is not exported and is intended to be called only by 
#' \linkInt{latexTable}.

#' @return A vector of strings. Each string is a column{\NB}name.

#' @param mat A matrix, typically a \code{regTable} object.
#' @param SE_table Logical variable. See \linkInt{latexTable}.
lt_colNames_default <- function (  
  # If arguments are not supplied, we look to the calling frame --
  # parent.frame() -- for the arguments. This strategy is appropriate because
  # this function will typically be called from lt_footer(), which will supply
  # the arguments.  [2019 12 29]
  mat      = parent.frame()$mat,
  SE_table = parent.frame()$SE_table) {

  # If a user issues a call like "latexTable(rT1, colNames = lt_colNames_default())",
  # the default arguments won't work; instead, mat and SE_table will be NULL. 
  # The problem in this case is that the calling environment is the user's  
  # environment (typically the global environment), and mat and SE_table  
  # haven't been specified in that environment. We now solve that problem by   
  # using the latexTable() default values for those arguments. [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, colNames = lt_colNames_default())"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.

  if (is.null(SE_table))  SE_table <- rlang::call_args(userCall)[['SE_table']] %>% eval
  if (is.null(SE_table))  SE_table <- formals(latexTable)$SE_table  # get default value

  if (is.null(colnames(mat))) {
    cN <- lt_colNumbers(mat, SE_table)
  }
  else {
    if (SE_table) cN <- colnames(mat)[seq(1, ncol(mat), by = 2)] 
    else          cN <- colnames(mat)    
  }
  
  cN
}



#' Compute default footers for latexTable() objects.
 
#' @description 
#' The default footer row or rows are determined in this way: if 
#' \code{SE_table} is \code{FALSE} or \code{rowNames} is \code{NULL}, 
#' no footer rows are produced. Otherwise, a footer row will be added for each 
#' of the following attributes of \code{mat}: "r.squared", "SER", and "N". If 
#' \code{mat} lacks one of those attributes, there will be no corresponding 
#' footer{\NB}row.   


#' The function is not exported and is intended to be called only by 
#' \linkInt{latexTable}.
#' 
#' @return A list of string vectors. Each list element contains the strings
#' needed to produce a footer{\NB}row.
#' 
#' @param mat A matrix, typically a \code{regTable} object.
#' @param rowNames Character vector. See \linkInt{latexTable}.
#' @param SE_table Logical variable. See \linkInt{latexTable}.
#' @param decimalPlaces Integer. See See \linkInt{latexTable}.
lt_footer <- function (  

  # If arguments are not supplied, we look to the calling frame --
  # parent.frame() -- for the arguments. This strategy is appropriate because
  # this function will typically be called from latexTable(), rather than 
  # directly by the user. And in that case, arguments like "mat" and 
  # "rowNames" will take on their default values in the calling frame.
  # [2019 12 29]
  mat           = parent.frame()$mat,
  rowNames      = parent.frame()$rowNames,
  SE_table      = parent.frame()$SE_table,
  decimalPlaces = parent.frame()$decimalPlaces) { 

  
  # If a user issues a call like "latexTable(rT1, footerRows = lt_footer())",
  # the default arguments won't work, and mat, rowNames, etc. will all be 
  # NULL. The problem in this case is that the calling environment is the 
  # user's environment (typically the global environment), and mat, rowNames,
  # etc. haven't been specified in that environment. We now solve that 
  # problem by using the latexTable() default values when the user hasn't 
  # supplied them in his call.  [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, footerRows = lt_footer()"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.

  if (is.null(rowNames))  rowNames <- rownames(mat)
  
  if (is.null(SE_table))  SE_table <- rlang::call_args(userCall)[['SE_table']] %>% eval
  if (is.null(SE_table))  SE_table <- formals(latexTable)$SE_table  # get default value

  if (is.null(decimalPlaces))  decimalPlaces <- rlang::call_args(userCall)[['decimalPlaces ']] %>% eval
  if (is.null(decimalPlaces))  decimalPlaces <- formals(latexTable)$decimalPlaces  # get default value
  
  
  # Force evaluation -- that is, escape lazy evaluation. Without this command,
  # lt_rSquaredRow() will throw an error. The problem is that decimalPlaces is
  # in the parent frame when lt_footer() is called, but not when 
  # lt_rSquaredRow() is called.  [2019 12 29]
  force(decimalPlaces)  
  
  if (SE_table && !is.null(rowNames)) {
    footerList <- list()

    if (!is.null(attr(mat, "r.squared")))
      footerList <- c(footerList, lt_rSquaredRow(mat, decimalPlaces) %>% list)
    if (!is.null(attr(mat, "SER")))
      footerList <- c(footerList, lt_SER_row(mat, decimalPlaces) %>% list)
    if (!is.null(attr(mat, "N")))
      footerList <- c(footerList, lt_nobsRow(mat) %>% list)
  }
  
  if ( !exists('footerList') || length(footerList)==0 ) footerList <- NULL
  
  footerList
}




#' Automatically determine column names of the form (1), (2), etc.
#' 
#' Given \code{mat} and \code{SE_table}, this function determines appropriate
#' column-number names of the form "(1)", "(2)", etc.
#' 
#' @return A vector of strings. If \code{SE_table} is \code{TRUE}, the vector 
#' elements are "(1)", "(2)", etc., where the last column number is 
#' \code{ncol(mat)/2}. If \code{SE_table} is \code{FALSE}, the vector 
#' elements are "(1)", "(2)", etc., where the last column number is simply 
#' \code{ncol(mat)}.  

#' @param mat A matrix, typically a \code{regTable} object.
#' @param SE_table Logical variable. See \linkInt{latexTable}.

#' @export 
lt_colNumbers <- function (
  

  # If arguments are not supplied, we look to the calling frame --
  # parent.frame() -- for the arguments. This strategy is appropriate because
  # this function will typically be called from lt_footer(), which will supply
  # the arguments.  [2019 12 29]
  mat      = parent.frame()$mat,
  SE_table = parent.frame()$SE_table) {

  # If a user issues a call like "latexTable(rT1, colNames = lt_colNumbers())",
  # the default arguments won't work; instead, mat and SE_table will be NULL. 
  # The problem in this case is that the calling environment is the user's  
  # environment (typically the global environment), and mat and SE_table  
  # haven't been specified in that environment. We now solve that problem by   
  # using the latexTable() default values when the user hasn't supplied his 
  # own values.  [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, colNames = lt_colNumbers())"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.
  
  if (is.null(SE_table))  SE_table <- rlang::call_args(userCall)[['SE_table']] %>% eval
  if (is.null(SE_table))  SE_table <- formals(latexTable)$SE_table  # get default value
 
  if (SE_table) {
    colNames <- paste0("(", 1:(ncol(mat)/2), ")")
  } else {
    colNames <- paste0("(", 1:ncol(mat), ")")
  }
  
  colNames
}



#' Specify a footer row that indicates the number of observations for each regression.
#' 
#' Given a \code{mat} produced by \code{\link{regTable}()}, this function 
#' returns a footer row that indicates the number of observations for each 
#' model in \code{mat}.

#' @return A vector of strings. The first element in the vector is "Number of 
#' observations". The remaining elements are the numbers of observations for 
#' each regression in \code{mat}.

#' @param mat A \code{regTable} object.

#' @export
lt_nobsRow <- function (

  # If mat is not supplied, we look for it in the calling frame, 
  # parent.frame(). This strategy is appropriate because this function will 
  # typically be called from lt_footer(), which will supply mat.  [2019 12 29]
  mat = parent.frame()$mat) {

  # If a user issues a call like "latexTable(rT1, footerRows = lt_nobsRow())",
  # the default argument here won't work; instead, mat will be NULL. The  
  # problem in this case is that the calling environment is the user's
  # environment (typically the global environment), and mat hasn't been 
  # specified in that environment. We solve that problem with the line below.
  # [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, footerRows = lt_nobsRow())"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.

  if (! 'regTable' %in% class(mat)) {
    warning("mat was not produced with regTable(). The number-of-observations row in your footer is unlikely to be correct. You may want to specify your footerRows argument explicitly.")
  }
     
  c('Number of observations', attr(mat, "N"))
}



#' Specify a footer row that indicates \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} for each regression.
#' 
#' Given a \code{mat} produced by \code{\link{regTable}()} in which all
#' regressions are of class \code{lm}, this function returns a footer row that 
#' indicates \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} for each model in \code{mat}.

#' @return A vector of strings. The first element in the vector is "R$^2$". 
#' The remaining elements are strings that indicate 
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} for each model in \code{mat}.
#' The strings are rounded to the number of digits specified by the 
#' \code{decimalPlaces} argument.

#' @param mat A matrix, typically a \code{regTable} object.
#' @param decimalPlaces Integer. See \code{\link{latexTable}}.

#' @export 
lt_rSquaredRow <- function (

  # If arguments are not supplied, we look to the calling frame --
  # parent.frame() -- for the arguments. This strategy is appropriate because
  # this function will typically be called from lt_footer(), which will supply
  # the arguments.  [2019 12 29]
  mat           = parent.frame()$mat,
  decimalPlaces = parent.frame()$decimalPlaces) {

  # If a user issues a call like "latexTable(rT1, footerRows = lt_rSquaredRow())",
  # the default arguments won't work; instead, mat and decimalPlaces will be 
  # NULL. The problem in this case is that the calling environment is the 
  # user's environment (typically the global environment), and mat and 
  # decimalPlaces haven't been specified in that environment. We now solve  
  # that problem by using the latexTable() default values when the user hasn't
  # supplied his own values.  [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, footerRows = lt_rSquaredRow())"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.

  if (is.null(decimalPlaces)) decimalPlaces <- rlang::call_args(userCall)[['decimalPlaces']] %>% eval
  if (is.null(decimalPlaces)) decimalPlaces <- formals(latexTable)$decimalPlaces  # get default value
  
  if (! 'regTable' %in% class(mat)) 
    stop("The lt_rsquaredRow() function requires that mat be produced by regTable().")
  
  if (is.null(attr(mat, "r.squared")))
    stop("mat doesn't have an 'r.squared' attribute, perhaps because some of the regressions in mat were not created by lm().")
     
  c("R$^2$", round(attr(mat, "r.squared"), decimalPlaces))
}



#' Specify a footer row that indicates the standard error of regression for each model
#' 
#' Given a \code{mat} produced by \code{\link{regTable}()} in which all
#' regressions are of class \code{lm}, this function returns a footer row that 
#' indicates the standard error of regression (i.e., \eqn{\sigma}, the 
#' "residual standard error") for each model in \code{mat}.

#' @return A vector of strings. The first element in the vector is "Std. error
#' of regression". The remaining elements are strings that indicate the SER 
#' for each model in \code{mat}.  The strings are rounded to the number of 
#' digits specified by the \code{decimalPlaces} argument.

#' @param mat A matrix, typically a \code{regTable} object.
#' @param decimalPlaces Integer. See \code{\link{latexTable}}.

#' @export 
lt_SER_row <- function (
  
  # If arguments are not supplied, we look to the calling frame --
  # parent.frame() -- for the arguments. This strategy is appropriate because
  # this function will typically be called from lt_footer(), which will supply
  # the arguments.  [2019 12 29]
  mat           = parent.frame()$mat,
  decimalPlaces = parent.frame()$decimalPlaces) {

  # If a user issues a call like "latexTable(rT1, footerRows = lt_SER_row())",
  # the default arguments won't work; instead, mat and decimalPlaces will be 
  # NULL. The problem in this case is that the calling environment is the 
  # user's environment (typically the global environment), and mat and 
  # decimalPlaces haven't been specified in that environment. We now solve  
  # that problem by using the latexTable() default values when the user hasn't
  # supplied his own values.  [2019 12 29]
  userCall <- sys.call(-1)  
  if (is.null(mat))  mat <- rlang::call_args(userCall)[[1]] %>% eval
    # sys.call(-1) is the user's call, e.g., "latexTable(rT1, footerRows = lt_SER_row())"
    # In this line of code, we assume that the first argument is the "mat"
    # argument.

  if (is.null(decimalPlaces)) decimalPlaces <- rlang::call_args(userCall)[['decimalPlaces']] %>% eval
  if (is.null(decimalPlaces)) decimalPlaces <- formals(latexTable)$decimalPlaces  # get default value

  if (! 'regTable' %in% class(mat)) 
    stop("The lt_SER_row() function requires that mat be produced by regTable().")
  
  if (is.null(attr(mat, "r.squared")))
    stop("mat doesn't have an 'SER' attribute, perhaps because some of the regressions in mat were not created by lm().")
     
  c("Std. error of regression", round(attr(mat, "SER"), decimalPlaces))
}






#' Compute default positions of spacer columns in calls to latexTable().
#' 
#' \code{spacerColumns_default} specifies the default \code{spacerColumns}
#' argument in calls to \code{latexTable}. It takes the values of \code{mat}, 
#' \code{SE_table}, and \code{rowNames} that are passed to \code{latexTable}.
#' From these values, it computes the default positions of spacer columns:
#' * If \code{SE_table} is \code{FALSE}, there is a spacer column between every
#' column in \code{mat}.
#' * If \code{SE_table} is \code{TRUE}, there is a spacer column after every 
#' even-numbered column in \code{mat}, except for the last column.  
#' * If \code{rowNames} is not \code{NULL}, a spacer column is inserted between
#' the table's row names and the first column of data.
#' @md

#' @details 
#' The function is not exported and is intended to be called only by 
#' \linkInt{latexTable}.

#' @return A vector of integers.

#' @param mat Matrix.
#' @param SE_table Logical variable.
#' @param rowNames Vector.
lt_spacerColumns_default <- function (
  
  # If arguments are not supplied, this function will look to the parent frame
  # for the arguments. Typically, the parent frame will be a latexTable()
  # call.  [2019 12 21]
  mat      = parent.frame()$mat, 
  SE_table = parent.frame()$SE_table, 
  rowNames = parent.frame()$rowNames) { 

  sC <- if (SE_table && ncol(mat) > 2) { 
    seq(2, ncol(mat)-2, by = 2) 
  } else if (SE_table && ncol(mat) == 2) {
    NULL
  } else {
    1:(ncol(mat)-1)
  }

  if (!is.null(rowNames)) sC <- c(0, sC)
  
  sC
}




##############################################################################
# METHODS FOR THE latexTable CLASS
##############################################################################
print.latexTable <- function (x, ...) { 
  lineNumbers <- paste0("[", 1:length(x), "] "  )
  lineNumbers <- stringr::str_pad(lineNumbers, max(nchar(lineNumbers)))
  for (i in 1:length(x)) {
    cat(lineNumbers[i], "\"", x[i], "\"", "\n", sep = "", ...)
  }
}
  #print.latexTable <- function (x, ...) {
  #  writeLines(x, ...)
  #}



# The subsetting method here is lightly adapted from `[.noquote`. If we didn't 
# specify this method, any subset latexTable (for example, "myLatexTable[1:5]") 
# would print messily. Because this method is specified, head() and tail() are 
# adjusted as well.  [2019 12 19]
`[.latexTable` <- function (x, ...) {
  classX <- class(x)
  x <- unclass(x)[...]
  class(x) <- classX
  x
}



# Adapted from update.default()  [2019 12 20]
#' Update a latexTable object with new arguments.
#' 
#' Each \code{latexTable} object stores, as an attribute, the call that 
#' produced{\NB}it. \code{update.latexTable()} updates the call by replacing 
#' arguments or adding new ones. It then calls \code{\link{latexTable}()} to produce
#' a new \code{latexTable} object. 
#' 
#' \code{update.latexTable()} is adapted from \code{stats::update.default()}.  
#' It is a method for the generic \code{update()}.

#' @return A \code{latexTable} object.

#' @param object A \code{latexTable} object
#' @param ... Arguments to \code{latexTable()}, e.g., \code{colNames}, 
#'   \code{caption}.

#' @examples
#' lT1 <- latexTable(matrix(1:16, nrow = 4))
#' lT2 <- update(lT1, mat = matrix(2:17, nrow = 8), commandName = "intTable")
update.latexTable <- function (object, ...) {
  oldCall <- attr(object, "call")  
  if (is.null(oldCall)) 
    stop("You passed an object without a call component. update.latexTable() won't work on subsetted latexTable objects -- did you pass a subsetted latexTable object?")

  extras <- match.call(expand.dots = FALSE)$...  # returns pairlist of all arguments in ...
  extras <- as.list(extras)                      # convert to a regular list

  existingArgs <- !is.na(match(names(extras), names(oldCall)))  # boolean
  newCall      <- oldCall 
  for (a in names(extras)[existingArgs]) {
    newCall[[a]] <- extras[[a]]                                 # overwrite existing arguments
  }
  if (any(!existingArgs)) {                                  # if there are any new args...
      newCall <- c(as.list(newCall), extras[!existingArgs])  # ..append them to the call
      newCall <- as.call(newCall)
  }  
  
  # do.call(latexTable, extras)  
    # do.call(what, args), where args is a named list of arguments
    # Working code: do.call(latexTable, list(mat = matrix(2:17, nrow=4)))
  
  eval(newCall, parent.frame())
}
