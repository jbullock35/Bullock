#' Move or copy "freestanding" variables into a data frame.
#' 
#' Variables are specified by \code{pattern}, which is a regular expression.
#' The modal length of all variables in the calling environment that match
#' \code{pattern} is determined. Matching variables are then moved to a data 
#' frame (or copied to a data frame if \code{move} is \code{FALSE}). 
#' \code{pattern} may be \code{NULL}, in which case all variables in the 
#' calling environment will be examined.
#' 
#' If there are multiple modal lengths of the objects in the calling 
#' environment, all modes will be used. For example, if the calling 
#' environment has 20 objects of length{\NB}1, 20 objects of length{\NB}2, and
#' one object of length{\NB}3, the returned data frame may have as many as 40
#' columns.
#' 
#' Variables that have the "dim" attribute -- for example, arrays and 
#' matrices -- will not be moved into the new data frame. Functions will 
#' never be moved into the new data frame, either.
#' 
#' @param pattern String that specifies a regular expression. It is  
#' \code{NULL} by default, in which case all variables in the environment are
#' examined for inclusion in the data frame.
#' @param move Logical variable.
#' 
#' @return Data frame containing all one-dimensional variables that have names 
#' matching \code{pattern} and that have the modal length of those variables. 
#' 
#' @export
moveToDF <- function(pattern = NULL, move = TRUE) {

  # GET ALL VARIABLE NAMES
  if (!missing(pattern)) {
    stopifnot(is.character(pattern))
    varNames <- ls(pattern = pattern, envir = parent.frame())
  }
  else { 
    varNames <- ls(envir = parent.frame())
  }

  
  # GET LIST OF MATCHING VARIABLES
  varList <- sapply(varNames, get, simplify = FALSE)  

  
  # REMOVE OBJECTS OF THE WRONG LENGTH
  # Remove, from varList, objects that don't have the modal length of all 
  # objects in the list.
  modalLength <- modalValue(lengths(varList))
  if (length(modalLength) > 1)
    message(paste0(
      "There are multiple modal lengths for the objects in the calling environment: ",
      paste(modalLength, collapse = ', '),
      '.'))
  badClasses  <- 'function' 
  hasDim      <- sapply(varList, function (x) !is.null(dim(x)))  # matrices, arrays, etc.
  hasBadClass <- sapply(varList, function (x) any(class(x) %in% badClasses))
  badVars     <- which(!lengths(varList)%in%modalLength | hasDim | hasBadClass)
  if (length(badVars > 0)) 
    varList   <- varList[-badVars]                               # remove objects from list
  
  
  # REMOVE OBJECTS FROM CALLING ENVIRONMENT IF move == TRUE
  if (move) {
    objNamesToRemove <- names(varList)
    rm(list = objNamesToRemove, envir = parent.frame())
  }
  
  # RETURN DATA FRAME
  data.frame(varList)
}
