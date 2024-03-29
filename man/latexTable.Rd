% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latexTable.R
\name{latexTable}
\alias{latexTable}
\title{Create a LaTeX table from a matrix.}
\usage{
latexTable(
  mat,
  SE_table = TRUE,
  headerFooter = TRUE,
  commandName = "myTable",
  callCommand = TRUE,
  label = commandName,
  floatPlacement = "p",
  landscape = if (SE_table) ncol(mat)/2 >= 6 else ncol(mat) >= 6,
  starredFloat = FALSE,
  horizOffset = "-0in",
  rowNames = rownames(mat),
  footerRows = lt_footer(),
  colNames = lt_colNames_default(),
  colNameExpand = FALSE,
  extraRowHeight = if (SE_table) "2pt" else "4pt",
  spacerColumns = lt_spacerColumns_default(),
  spacerColumnsWidth = ".67em",
  spacerRows = NULL,
  spacerRowsHeight = ".15in",
  tabColSep = "2.75pt",
  spaceBetweenColNameRows = "-.025in",
  columnTierSeparator = "  ",
  printCaption = TRUE,
  caption = paste0(label, " caption goes here."),
  captionMargins = NULL,
  formatNumbers = TRUE,
  decimalPlaces = 2,
  SE_fontSizeString = "\\\\fontsize{10.3bp}{10.3bp}\\\\selectfont",
  NA_text = "",
  clipboard = FALSE
)
}
\arguments{
\item{mat}{Matrix or tibble of numbers to be displayed in a LaTeX table.}

\item{SE_table}{Logical variable that indicates whether \code{mat} contains
pairs (or "tiers") of columns, with the first column in each pair
containing estimates, and the second column containing standard errors.
Defaults to \code{TRUE}. If \code{TRUE}, the even-numbered columns of
\code{mat} will be rendered in smaller type than the odd-numbered columns.
That is, the standard errors will be rendered in smaller type than their
corresponding estimates. This default type sizing can be overridden by
the \code{SE_fontSizeString} argument.}

\item{headerFooter}{Logical variable. If \code{TRUE}, which is the default,
the output will be (or at least include) a LaTeX macro that generates a
table. For example, you will be able to produce a table simply by calling
\code{\\myTable{p}} or \code{\\myTable{h}} in your master LaTeX document.\cr
\indent If \code{headerFooter} is \code{FALSE}, the only output of the
function will be LaTeX code for "data rows"---one row for each row of
\code{mat}.}

\item{commandName}{A string. It is the name of the macro that produces the
LaTeX table (if \code{headerFooter} is \code{TRUE}). By default, it is
"myTable"; you can change it to something more descriptive, e.g.,
"mainEstimates".}

\item{callCommand}{Logical variable. Should the last line of the
\code{latexTable} object be a call to the macro that creates the table?
If \code{callCommand} is \code{TRUE}, which is the default, sourcing a
file that contains \code{latexTable} output---that is, by using
\code{\\input} or \code{\\include} in your master LaTeX document---will
produce a table when that master LaTeX document is rendered. If
\code{callCommand} is \code{FALSE}, sourcing the file will make the macro
available in your LaTeX document, but it won't call the macro. (You will
need to call the macro yourself by adding a line like \code{\\myTable{p}}
to your LaTeX document.)}

\item{label}{A string. Specifies the LaTeX label for a table. It is not printed
anywhere in the table, but references to the figure in
your LaTeX document (for example, references created by \code{\\ref} or
\code{\\autoref}) must include the label name. For simplicity, the
default \code{label} is the same as the \code{commandName} argument.\cr\cr\cr\cr}

\item{floatPlacement}{Character vector of length{\NB}1. Acceptable values
are \code{p} (the default, which places each table on its own page),
\code{h, H, t, b,} and \code{!}. See the
\href{https://bit.ly/LaTeX-floats}{LaTeX wikibook} for more on float
placement in LaTeX.}

\item{landscape}{Logical variable. Determines whether the table is printed
in landscape or in portrait mode. Affects the output only if if
\code{headerFooter == TRUE} and \code{callCommand == TRUE}.}

\item{starredFloat}{Logical variable that indicates whether the LaTeX
table should be specified with \code{table*} instead of \code{table}. The
default is FALSE, but you may want to set it to TRUE if you want you are
using a multi-column page layout in LaTeX and want the table to cross
both columns.}

\item{horizOffset}{A string that specifies a LaTeX length, e.g., ".25in".
When the LaTeX code produced by \code{latexTable} is rendered, the table
will be moved to the right by this length (or to the left if the length
is negative, e.g., "-.25in").\cr\cr\cr\cr}

\item{rowNames}{Character vector of labels for the rows in \code{mat}. The
labels will be printed to the left of each row in \code{mat}.
\code{rowNames} can be \code{NULL}.}

\item{footerRows}{List, object that can be coerced to a list, or a function
that creates a list. Each element in the list is a character vector that
specifies entries for a row of the footer, or a function that creates
such a character vector. The default is
\linkInt{lt_footer}, which typically provides a "Number of observations"
row. If a model is of class "lm," it will also provide an
\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} and
"Std. error of regression" row.\cr
\indent The first entry in each \code{footerRows} list-element should
be the row name for the corresponding footer row (e.g., '$F$', '$R^2$').\cr
\indent See the examples for various ways to specify the
\code{footerRows} argument.}

\item{colNames}{List, or object that can be coerced to a list, of column
headings. Typically, each element in the list is a character vector, and
the elements of the character vector specify the names of the table's
columns.\cr
\indent If \code{SE_table} is \code{TRUE} (the default), each column
name will appear over a pair of columns. In this case, each element in
the \code{colNames} list should contain \code{ncol(mat)/2} entries.\cr
\indent To specify multi-line column labels, use a list with multiple
elements. The entries in the first list element will then appear in the
top row of the column label, the entries in the second list element will
appear in the next row of the column label, and so{\NB}on.\cr
\indent By default, column names will be taken from \code{colnames(mat)}.
If \code{colnames(mat)} is \code{NULL}, columns will be numbered "(1)",
"(2)", etc. See \linkInt{lt_colNames_default} for more information.}

\item{colNameExpand}{Logical variable. By default, an entry of \out{''} in a
\code{colNames} list element---that is, an empty entry---indicates that a
column should have no column heading. But if \code{colNameExpand} is
\code{TRUE} and a text entry in a \code{colNames} list element is
followed by one or more \out{''} entries, the column name specified by the text
entry will bridge the columns that have \out{''} entries.\cr
\indent \code{colNameExpand} and \code{spacerColumns} do not play well
together. If you run \code{latexTable} with \code{colNameExpand == TRUE}
and a non-NULL \code{spacerColumns} argument, you will get LaTeX output,
but you will probably need to edit the "\\multicolumn" and "\\cmidrule"
commands in the output so that LaTeX can render the output.\cr\cr\cr\cr}

\item{extraRowHeight}{A string that specifies a length that LaTeX
recognizes, e.g., '2pt' or '.25in'. The \code{extrarowheight} length in
LaTeX will be set to \code{extraRowHeight}. In practice, this means that
the vertical space between every row will be increased by
\code{extraRowHeight}. This argument has no effect if \code{headerFooter}
is \code{FALSE}.}

\item{spacerColumns}{A vector of integers. Specifies columns in \code{mat}
after which to insert columns that contain no entries. These "spacer
columns" are used to insert horizontal space into the typeset table. By
default, spacerColumns are specified by a helper function,
\code{spacerColumns_default()}:
\itemize{
\item If \code{SE_table} is \code{FALSE}, there is a spacer column between
every column in \code{mat}.
\item If \code{SE_table} is \code{TRUE}, there is a spacer column after
every even-numbered column in \code{mat}, except for the last column.
\item If \code{rowNames} is not \code{NULL}, a spacer column is inserted
between the table's row names and the first column of data.\cr
\indent To add a spacerColumn between the rownames and the first data column,
make 0 one of the values in spacerColumns.\cr
\indent \code{colNameExpand} and \code{spacerColumns} do not play well
together. If you run \code{latexTable} with \code{colNameExpand == TRUE}
and a non-NULL \code{spacerColumns} argument, you will get LaTeX output,
but you will probably need to edit the "\\multicolumn" and "\\cmidrule"
commands in the output so that LaTeX can render the output.\cr
\indent See the end of the
\href{../doc/tables.html}{Building better tables in less time} vignette
for a technical note on \code{spacerColumns} and column
spacing in LaTeX.
}}

\item{spacerColumnsWidth}{Either a single string of a recognizable LaTeX
length (e.g., '.5em') or a character vector indicating the width of each
spacer column. Has no effect unless \code{headerFooter} is \code{TRUE}.}

\item{spacerRows}{A vector of integers. After each row in \code{mat} whose
number is in \code{spacerRows}, a vertical space of \code{spacerRowsHeight}
will be printed. For example, if \code{spacerRows == c(2, 4)}, a vertical
space will be added after rows 2 and 4 of \code{mat}.}

\item{spacerRowsHeight}{A string that specifies a recognizable LaTeX length,
e.g., ".15in".}

\item{tabColSep}{Character vector indicating a length that LaTeX recognizes,
e.g., ".25in". The \code{tabcolsep} value in LaTeX will be set to this
value if \code{headerFooter} is \code{TRUE}. If \code{SE_table} is
\code{TRUE}, \code{tabColSep} will be the default distance between the
estimate and the SE column in each column pair, and it will be half of the
distance between column pairs. If \code{SE_table} is FALSE,
\code{tabColSep} will simply be half of the default distance between
columns. These distances between columns can be increased by the
\code{spacerColumns} argument.}

\item{spaceBetweenColNameRows}{String specifying a LaTeX length, e.g.,
"-.025in" (the default). When column names are to be split across
multiple rows, a vertical space of this length will be inserted between
the rows.}

\item{columnTierSeparator}{A string. In the LaTeX code generated by
\code{latexTable}, all columns are separated from each other by " & ".
Column tiers -- that is, pairs of columns giving the estimate and the SE
for a particular coefficient -- are further separated by
\code{columnTierSeparator}, which defaults to two spaces ('  '). This
option affects only the LaTeX code produced by \code{latexTable}; it
exists to make the LaTeX code more readable. It does not affect the
typeset (e.g., PDF) version of the table.\cr\cr\cr\cr}

\item{printCaption}{Logical variable.}

\item{caption}{A string. It can include LaTeX commands, e.g.,
"\\\\textit{Results from a minimal specification.}" It can also include
references to other labeled parts of your LaTeX document, e.g.,
"\\\\autoref{SomeFigure}". See the examples.}

\item{captionMargins}{A vector of two strings that specify the margins of
the caption. The strings should be LaTeX lengths, e.g., ".25in" or ".67em".
By default, \code{captionMargins} is \code{NULL}.\cr\cr\cr\cr}

\item{formatNumbers}{Logical variable, \code{TRUE} by default. Pretty-print the
entries in \code{mat}{\NB}by:
\itemize{
\item Rounding all entries to the \code{decimalPlaces} digit.
\item Padding out entries with trailing zeroes. For example, \code{3} will become
\code{3.00} if \code{decimalPlaces == 2}.
\item Removing leading zeroes. For example, \code{0.12} will become \code{.12}.
}

If \code{formatNumbers} is \code{FALSE}, formatting of entries in \code{mat} will be
handled by \code{\link[base:print.default]{print.default()}}.}

\item{decimalPlaces}{Integer. If \code{formatNumbers} is \code{TRUE}, table
entries will be shown to this decimal place. For example, if
\code{decimalPlaces==2}, both "3.0035" and "3" will become "3.00."\cr
\indent If \code{formatNumbers} is \code{FALSE}, entries will not be
adjusted, but \code{decimalPlaces} will still be used to determine the
widths of columns and some aspects of column spacing. For example, even
if \code{formatNumbers} is \code{FALSE}, data columns will be wider when
\code{decimalPlaces} is \code{10} than when it is{\NB}\code{2}.}

\item{SE_fontSizeString}{A string. Indicates how standard errors are to be
formatted when \code{SE_table} is \code{TRUE}. Defaults to
\code{\\\\fontsize{10.3bp}{10.3bp}\\\\selectfont}, which renders standard
errors in slightly smaller type than the corresponding estimates.}

\item{NA_text}{A string. \code{NA} and \code{NaN} entries in \code{mat}
will be replaced by the string.\cr\cr\cr\cr}

\item{clipboard}{Logical variable. Copy entire output to clipboard.
Useful if you want to paste the output directly into a \code{.tex} file.
Works only on Windows.}
}
\value{
An object of classes \code{latexTable} and \code{character}. The
returned object is a vector of strings of LaTeX code; each string is a line
in a LaTeX macro that can create a table.\cr
\indent There is one small exception. If \code{callCommand} is
\code{TRUE}, the last line is not part of the macro; instead, it calls the
macro, thereby telling LaTeX to display the table. For example, if
\code{commandName} and \code{label} are \code{myTable}, and if
\code{callCommand} is \code{TRUE}, the last line of the returned object is
\code{\\mytable{p}}.
}
\description{
\code{latexTable()} takes a single matrix or tibble, \code{mat}. By
default, it returns a LaTeX macro that creates a well-formatted LaTeX
table.
}
\note{
\emph{Required LaTeX packages.} The LaTeX code produced by the
\code{latexTable} makes use of capabilities provided by the \code{booktabs},
\code{caption}, \code{float}, \code{numprint}, and \code{ragged2e} LaTeX
packages---and, for landscaped tables, the \code{pdflscape}{\NB}package. If
you haven't installed those LaTeX packages, you won't be able to render the
tables produced by \code{latexTable}.\cr
\indent The LaTeX code produced by \code{latexTable} also makes use of
capabilities provided by the \code{array} and \code{afterpage} LaTeX
packages---but these packages are \href{https://www.ctan.org/pkg/latex-tools}{included}
in every LaTeX distribution.\cr\cr
\emph{Changes from pre-release versions:}
\itemize{
\item The names of some arguments have
changed slightly since the pre-release versions of this function. They have
been changed to enforce consistency: camelCase is used for all arguments,
and every acronym is followed by an underscore (_) character. We thus have
\code{SE_table} instead of \code{SEtable}, \code{tabColSep} instead of
\code{tabcolsep}, and so{\NB}on.
\item The \code{hspace} argument has been renamed to \code{horizOffset}.
\item Some default arguments have changed. In particular, the default
\code{spacerColumns} argument is no longer \code{NULL}. Instead, the
default is to insert spacer columns in appropriate places. See documentation
of the \code{spacerColumns} argument for details.
}
}
\examples{
data(iris)
lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
lm3 <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
rT1 <- regTable(list(lm1, lm2, lm3))
latexTable(rT1)
latexTable(rT1, SE_table = FALSE, colNames = lt_colNumbers())
lt2 <- latexTable(
  mat      = rT1, 
  colNames = list(qw("model model model"), qw("1 2 3")))
\dontrun{
lt3 <- latexTable(
  mat         = rT1, 
  colNames    = lt_colNumbers(),
  rowNames    = c(
    "Intercept", 
    "Petal length", 
    "Petal width", 
    "Petal length $\\\\times$ petal width"),
  footerRows  = list(lt_nobsRow(), lt_rSquaredRow()),
  commandName = 'mainEstimates',
  caption     = "Each entry is an estimate or a standard error from a separate OLS regression.")
lt4 <- update(
  lt3,
  commandName = 'myEstimates',  # change name of LaTeX macro
  spacerRows  = 1,              # add vertical space after intercept row
  footerRows  = list(
    c("My first footer row",  "a",     "b",     "c"),
    c("My second footer row", "Lorem", "ipsum", "dolor")
  ))
}

# You can pass a previously created list to the "footerRows" argument. To
# store functions in that list, use alist():
footerList <- alist(lt_rSquaredRow, lt_SER_row)
latexTable(rT1, footerRows = footerList)
}
\seealso{
Other functions for making tables: \linkInt{regTable},
\linkInt{latexTablePDF}. See also the \href{../doc/tables.html}{Building better tables in less time}
and \href{../doc/tables_RMarkdown.html}{Using \code{latexTable()} with R Markdown and Rnw documents}
vignettes.
}
\concept{Tufte}
\concept{table}
\concept{tables}
