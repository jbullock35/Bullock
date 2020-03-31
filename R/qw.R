#' Perl-like function for quoting a list of words
#' 
#' \code{qw} stands for \emph{quote words.} The function takes a string of 
#' words separated by whitepsace characters.  It returns a vector in which 
#' each element is a word.  The point of the function is to speed the 
#' creation of vectors of words and to make for more readable code.
#' 
#' This function is an R implementation of 
#' \href{https://perldoc.perl.org/5.30.0/perlop.html#Quote-Like-Operators}{Perl's \code{qw()} operator}. 
#' Sadly, the operator has no equivalent in Python.
#'
#' @param x A string. It may contain newline characters; \code{x} will be 
#' split by these characters just as it will be split by ordinary spaces.
#' See the examples.
#' 
#' @author Florent Delmotte (``flodel''). See 
#' \url{http://stackoverflow.com/questions/520810/}.
#' 
#' @seealso \url{https://perldoc.perl.org/5.30.0/perlop.html#Quote-Like-Operators}.
#' 
#' @examples
#'  qw("You can type    text 
#'      with    line breaks if you
#'      wish")
#'  #  [1] "You"        "can"        "type"       "text"      
#'  #  [5] "here"       "with"       "linebreaks" "if"        
#'  #  [9] "you"        "wish"
#' 
#' @export
qw <- function(x) {
  x <- sub('^\\s+', '', x)  # eliminate spaces at the beginning of x
  unlist(strsplit(x, "[[:space:]]+"))
}
