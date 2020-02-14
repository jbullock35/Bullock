#' Create a matrix of regression output from a list of regression models.
#' 
#' \code{regTable()} takes a list of regression objects, such as those created 
#' by \code{lm()}. It returns a matrix in which the columns are estimates and 
#' standard errors -- two columns for each model. Together, the two columns
#' that represent a regression are a \emph{column-pair} or a
#' \emph{column-tier.}

#' @importFrom magrittr %>%

#' @param objList List of regression objects. This is the only required 
#' argument. \code{regTable()} has been tested with objects of classes 
#' \code{ivreg, glm, lm,} and \code{plm,} and with the objects produced by the 
#' \code{estimatr} package. It should work with other regression objects, too 
#' -- that is, with any regression objects for which the \code{coef()} and
#' \code{vcov()} functions can be used.

#  NOTE: We don't want to make it possible to pass a single regression object 
#  here. We do that sort of thing for latexTable, but here, it seems 
#  dangerous. Things could go wrong if we users mistakenly say 
#  "regTable(lm1, lm2)". See issue #13 for details, and note that I've looked 
#  into this point more recently.  [2020 02 06] 


#' @param colNames A vector of strings as long as \code{length(objList)}. 
#' 
#' @param rowsToRemove A vector of strings, which may specify regular 
#' expressions. Variables in the regressions whose names match the strings will 
#' be omitted from the \code{regTable} output. This argument overrides 
#' \code{rowsToKeep.}
#' 
#' @param rowsToKeep A vector of strings, which may specify regular 
#' expressions. Variables in the regressions whose names match the strings will 
#' be kept in the \code{regTable} output. All other variables will be omitted.
#' If \code{rowsToRemove} is specified, this argument has no effect.
#' 
#' @param clusterVar Either \code{NULL}, a list of length{\NB}1, or a list of 
#' length \code{length(objList)}. If \code{NULL} (the default), 
#' \code{regTable()} will report the standard errors indicated by your 
#' regression objects. Otherwise, the argument to \code{clusterVar} should 
#' indicate the clusters for the corresponding regression objects in
#' \code{objList}. Clustered SEs for "lm" objects will be produced by 
#' \code{multiwayvcov::cluster.vcov}, and clustered SEs for "ivreg" objects 
#' will be produced by \code{ivpack::cluster.robust.se}. \code{regTable()} 
#' does not support clustering for other kinds of regression objects.

#' @return A matrix in which the columns are estimates and 
#' standard errors -- two columns for each model. The matrix has an "N"
#' attribute that indicates the number of observations for each regression. If
#' all regressions were of class \code{lm} (but not also class \code{glm}), it 
#' also has the "r.squared" and "SER" attributes. (The "SER" attribute 
#' indicates the standard error of regression -- AKA \eqn{\sigma} or the 
#' "residual standard error" --- for each model.)

#' @section Subsetting regTable objects with the `[` operator:
#' You can take subsets of regTable objects with the `[` operator. It subsets
#' intelligently. That is, it knows whether the subsetted object 
#' contains only intact column tiers, and it modifies the class and attributes
#' of the subsetted object accordingly. To wit: 
#' * If you remove only certain column-pairs from your original regTable 
#' object, such that the remaining columns all form intact column-pairs, all
#' attributes for the remaining column-tiers are preserved. For example, the 
#' \code{N} and \code{r.squared} attributes are preserved. In other words, 
#' the `[` operator knows which regressions you have removed from the table, 
#' and it removes attribute information for only those regressions.
#' * If you remove only rows from your original regTable object, all 
#' attributes are preserved. This result has the potential to be misleading:
#' for example, you may remove a row that reports information for a given 
#' predictor, but the `N`, `r.squared`, and `SER` attributes were all 
#' computed from regressions that included that predictor. Consequently, a 
#' message will be printed to remind you that the attributes have been 
#' preserved.
#' * If you remove columns from your original regTable object such that the 
#' remaining columns do *not* all form intact column-pairs, the `N`, 
#' `r.squared`, and `SER` attributes are stripped, and the returned object is 
#' a matrix without the "regTable" class.\cr
#' 
#' See the examples for an illustration.
#' @md

#' @section Change from the pre-publication version:
#' Before \code{regTable()} was incorporated into this package,
#' it used the \code{rowsToKeep} argument differently: variables were kept 
#' only if the \emph{beginnings} of their names matched the strings in 
#' \code{rowsToKeep}.


#' @seealso Other functions for making tables: \linkInt{latexTable}, 
#' \linkInt{latexTablePDF}. See also the \href{../doc/tables.html}{Building better tables in less time} 
#' vignette. 


#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
#' lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' regTable(list(lm1, lm2))
#' regTable(list(lm1, lm2), colNames = c("Sepal length", "Sepal width"))
#' regTable(list(lm1, lm2), rowsToKeep = 'Length')
#' regTable(list(lm1, lm2), rowsToKeep = c('Intercept', 'Length'))
#' regTable(list(lm1, lm2), clusterVar = list(iris$Species))
#' 
#' # illustrate subsetting
#' rT <- regTable(list(lm1, lm2))
#' ncol(rT)           # 4
#' attributes(rT)$N   # 150 150
#' rT2 <- rT[1:2, ] 
#' attributes(rT2)$N  # 150 150
#' rT3 <- rT[, 3:4]
#' attributes(rT3)$N  # 150
#' rT4 <- rT[, 2:3]
#' attributes(rT4)$N  # NULL
#' class(rT4)         # "matrix"



#' @export
regTable <- function (
  objList, 
  colNames     = NULL, 
  rowsToRemove = NULL, 
  rowsToKeep   = NULL,
  clusterVar   = NULL) {

  if (! 'list' %in% class(objList) ) {
    stop("objList must be of class 'list'.")
  }
  if (! is.null(colNames) && length(colNames) != length(objList)) {
    stop("colNames must be NULL or the length of objList.")
  }
  
  
  
  ##############################################################################
  # CHECK CLASSES OF OBJECTS IN objList 
  ##############################################################################  
  # Test classes of objects in objList. We use inherits() instead of class() 
  # for this purpose. inherits(x, c("ivreg", "lm")) returns TRUE if x has 
  # either of those classes, FALSE otherwise. Note that ivreg and plm objects
  # don't inherit the "lm" class. But glm objects -do- inherit the "lm" class.
  # [2020 01 20]
  #
  # classVec_ok <- sapply(objList, inherits, qw("ivreg lm plm"))  # boolean
  # if (! all(classVec_ok)) {
  #   warning("regTable() has only been designed to work with models of class 'lm', 'plm', and 'ivreg'.")
  # }  
  classVec_ivreg <- sapply(objList, inherits, qw("ivreg"))  # boolean vector
  classVec_lm    <- sapply(objList, inherits, qw("lm"))     # boolean vector
  classVec_glm   <- sapply(objList, inherits, qw("glm"))    # boolean vector
  if (!is.null(clusterVar) & any(classVec_lm) & !requireNamespace('multiwayvcov', quietly = TRUE)) {
    stop("To calculate clustered standard errors for regressions of class \"lm\", the \"multiwayvcov\" package must be installed.")
  }
  if (!is.null(clusterVar) & any(classVec_ivreg) & !requireNamespace('ivpack', quietly = TRUE)) {
    stop("To calculate clustered standard errors for regressions of class \"ivreg\", the \"ivpack\" package must be installed.")
  }
  
  classVec_okForCluster <- all(classVec_ivreg | (classVec_lm & !classVec_glm))
  if (!is.null(clusterVar) && any(classVec_glm)) {
    stop(stringr::str_wrap("clusterVar isn't NULL, but regTable() cannot cluster SEs for glm objects."))
  }
  else if (!is.null(clusterVar) && !classVec_okForCluster) {
    stop(stringr::str_wrap("clusterVar isn't NULL, but regTable() can cluster SEs only for \"ivreg\" objects and for non-glm \"lm\" objects."))
  }
  
  
  
  ############################################################################
  # CHECK CLUSTER VARIABLES
  ############################################################################
  # Check for misspecified cluster name, e.g., "iris$species" instead of 
  # "iris$Species". The former will become NULL, and in turn, users will think
  # that they're getting clustered SEs when they're really getting unclustered
  # SEs -- unless we check for this problem.  [2020 01 02]
  if (!missing(clusterVar)) {  # clusterVar was specified by user
    clusterVarName <- deparse(match.call()$clusterVar)
    if (clusterVarName != 'NULL' && is.null(clusterVar)) {
      stop("your clusterVar variable doesn't exist.")      
    }
  }

  if (!is.null(clusterVar) && class(clusterVar) != 'list') {
    stop("clusterVar must be an object of class 'list'")
  }
  
  
  
  ############################################################################
  # GET COLUMN AND ROW NAMES, AND SET UP OUTPUT MATRIX
  ############################################################################
  # Get column names
  if (is.null(colNames)) {
    tmpNames <- NULL
    
    for (obj in objList) {
      if (any(qw("iv_robust lm_lin lm_robust") %in% class(obj))) {  # Objects from the "estimatr" package.
        tmpNames <- c(tmpNames, obj$outcome)
      }
      else if ('ivreg' %in% class(obj)) {
        tmpNames <- c(tmpNames, as.character(obj$formula[2]))
      }
      else {                                                        # Default if all else fails. Works with "lm" objects. 
        tmpNames <- c(tmpNames, as.character(obj$terms[[2]])[1])
      }
    }
    
    # If we were able to extract column names from objects...
    if (is.character(tmpNames) && length(tmpNames)==length(objList)) {
      colNames <- tmpNames
    }
  }
  
  if (! is.null(colNames)) {
    colNames <- paste0(rep(colNames, each = 2), c('', '.se'))
  }

  
  
  # Get names of predictors, including intercept. Eliminate names of 
  # unwanted rows.
  tmp      <- sapply(
    objList, 
    function (x) { names(x$coefficients) }, 
    simplify = FALSE)
  rowNames <- unique(unlist(tmp)) 
  if (! is.null(rowsToRemove)) { 
    for (pat in rowsToRemove) {
      rowNames <- rowNames[!grepl(pat, rowNames)]
    }
  }
  else if (! is.null(rowsToKeep)) {
    rowsToKeep <- paste0(rowsToKeep, collapse = '|')
    keepRegex  <- paste0('(', rowsToKeep, ')')
    rowNames <- rowNames[grepl(keepRegex, rowNames)]      
  }
  

  
  # Set up output matrix
  out      <- matrix(
    nrow     = length(rowNames), 
    ncol     = length(objList)*2, 
    dimnames = list(rowNames, colNames))
  

  
  ############################################################################
  # GET LIST OF ESTIMATES AND CLUSTERED STANDARD ERRORS 
  ############################################################################  
  # Get list of regression output when using clustered SEs. Each element of 
  # the list is a mastrix with two columns: one for estimates, the other for 
  # clustered SEs.  We use coeftest(x) here because it is more robust than 
  # summary(x)$coefficients, which sometimes gives problems when I apply it to 
  # "ivreg" objects.  [2012 08 01]
  if (!is.null(clusterVar)) {
    coefsAndSEs <- vector("list", length(objList))    # initialize empty list
    if (length(clusterVar)==1 & length(objList)>1) {
      clusterVar <- rep(clusterVar, length(objList))  # make clusterVar same length as objList
    }
    classMatrix <- sapply(objList, class)             # regression objs. may have >1 class
    if (class(classMatrix) == 'character') {
      classMatrix <- t(matrix(classMatrix))  
    }
    for (i in 1:ncol(classMatrix)) {
      if (any(classMatrix[, i] == 'lm')) {
        vcov.clustered   <- multiwayvcov::cluster.vcov(objList[[i]], clusterVar[[i]])
        coef_SE_mat      <- lmtest::coeftest(objList[[i]], vcov. = vcov.clustered)
      }
      else if (any(classMatrix[, i] == 'ivreg')) {
        # Use sink() to prevent cluster.robust.se() from printing "Cluster  
        # Robust Standard Errors" to the screen once for every object. 
        # [2020 01 20]
        tmp <- tempfile()
        sink(tmp)
          coef_SE_mat <- ivpack::cluster.robust.se(objList[[i]], clusterVar[[i]])
        sink()
      }
      coefsAndSEs[[i]] <- coef_SE_mat[, c('Estimate', 'Std. Error')]
    }
  }

  #  if (!is.null(clusterVar) & all(classVec_lm)) {
  #    vcovs.clustered <- mapply(  # returns list of covariance matrices
  #      FUN      = multiwayvcov::cluster.vcov,
  #      model    = objList,
  #      cluster  = clusterVar,
  #      SIMPLIFY = FALSE)  # required when objList is length 1
  #    
  #    coefsAndSEs <- mapply(      # returns list of reg. output
  #      FUN      = lmtest::coeftest,
  #      x        = objList,
  #      vcov.    = vcovs.clustered,
  #      SIMPLIFY = FALSE)  # required when objList is length 1
  #    
  #    coefsAndSEs <- sapply(      # returns list of reg. output with only desired columns
  #      X        = coefsAndSEs, 
  #      FUN      = function (x) x[, c('Estimate', 'Std. Error')],
  #      simplify = FALSE)  # required when objList is length 1
  #  }
  #  else if (!is.null(clusterVar) & all(classVec_ivreg)) {
  #    coefsAndSEs <- mapply(
  #      FUN = ivpack::cluster.robust.se,
  #      objList,
  #      clusterVar, 
  #      SIMPLIFY = FALSE)
  #    coefsAndSEs <- lapply(coefsAndSEs, function (x) x[, c('Estimate', 'Std. Error')])    
  #  }



  ############################################################################
  # GET LIST OF ESTIMATES AND STANDARD ERRORS WHEN THERE IS NO CLUSTERING
  ############################################################################  
  # If lmtest::coeftest doesn't work for some objects, I can try getting the 
  # estimates and SEs from broom::tidy(regObj).  [2020 01 23]
  else {  # if no clustering
    coefsAndSEs <- lapply(objList, function (x) {
      tmp <- lmtest::coeftest(x)[, c('Estimate', 'Std. Error')]

      # If we are dealing with a single predictor, the line above will drop 
      # the row name, causing problems. This block of code restores the row
      # name.  [2018 11 15]
      if (class(tmp) == 'numeric') {  # if we're dealing with a single row of output
        tmp <- matrix(
          data = tmp,
          dimnames = list(c('Estimate', 'Std. Error'), rownames(lmtest::coeftest(x)))) %>%
          t
      }
      tmp
    })
  }
  
  
  ############################################################################
  # POPULATE THE out MATRIX WITH ESTIMATES AND STANDARD ERRORS 
  ############################################################################  
  for (i in 1:length(coefsAndSEs)) {
    rowNamesToUse <- rowNames[rowNames %in% rownames(coefsAndSEs[[i]])]
    out[rowNamesToUse, (i*2 -1):(i*2)] <- coefsAndSEs[[i]][rowNamesToUse, ]
  } 

  
  
  ############################################################################
  # ADD CLASS AND ATTRIBUTES, THEN RETURN THE OBJECT 
  ############################################################################  
  class(out)     <- c('regTable', class(out))  
  attr(out, "N") <- sapply(objList, nobs)
  if ( all(classVec_lm) && !any(classVec_glm) ) {  # if every model is an lm() model 
    objListSum <- sapply(objList, function (x) summary(x)[c('r.squared', 'sigma')] )
    attr(out, "r.squared") <- as.numeric( objListSum['r.squared', ] )
    attr(out, "SER")       <- as.numeric( objListSum['sigma', ] )
  }
  out
}


##############################################################################
# METHODS FOR regTable CLASS
##############################################################################
#' @export 
`[.regTable` <- function (x, ...) {
  myCall    <- match.call()
  attrX     <- attributes(x)[c('N', 'r.squared', 'SER')]
  dimnamesX <- dimnames(x)  # a two-element list: rows, then columns
  

  
  # GET INDICES OF SELECTED ROWS AND COLUMNS  
  callArgs <- rlang::call_args(myCall)  # a list
  
  # We need to check the length of callArgs because str(myRegTable) won't work
  # otherwise. The problem is that it tries to subset regTable with a single
  # number. For example, it (really str.default()) will call "x[6]". This will
  # work when x is a matrix. But it won't work when "x" is a regTable unless
  # we check the length of callArgs below.  [2020 01 29]
  if (length(callArgs) >= 3) {
    rowDim <- rlang::call_args(myCall)[[2]]    
    colDim <- rlang::call_args(myCall)[[3]]
  } 
  else {
    rowDim <- NULL
    colDim <- NULL
  }
 
  
  # rowDim and colDim should be numbers. But if they have been passed from 
  # user-created function, they are not numbers; they instead have the "name"
  # class. (See myFunc() in regTable_test.R for an example.) We now solve 
  # this problem.  [2020 02 14]
  if (!rlang::is_missing(rowDim) && is.name(rowDim)) rowDim <- dynGet(deparse(rowDim)) 
  if (!rlang::is_missing(colDim) && is.name(colDim)) colDim <- dynGet(deparse(colDim))   


  # rowDim or colDim may be missing. This will happen, for example, if a user
  # passes rT[1:2, ] or rT[, 1:2]. So we now use rlang::is_missing(). It 
  # returns FALSE for objects that are NULL. But it returns TRUE when 
  # dimensions were specified in the normal way and one dimension wasn't 
  # specified, as in the examples above.  [2020 02 13]
  rowDimMissingWhenPassed <- rlang::is_missing(rowDim)  # used at end of file
  if (rlang::is_missing(rowDim)) rowDim <- 1:nrow(x)
  if (rlang::is_missing(colDim)) colDim <- 1:ncol(x)
  
  
  # rowDim or colDim may still be a call rather than a set of numbers. For 
  # example, if the user passes rT[1:2, 3:4], rowDim and colDim are calls, not
  # numbers. We now convert them to numbers.
  if (is.call(rowDim)) rowDim <- eval(rowDim)
  if (is.call(colDim)) colDim <- eval(colDim)
  
  
  #  colDimMissing <- rlang::is_missing(colDim)
  #  if (rowDimMissing && !colDimMissing) {  # matches rT [, 3:5]
  #    rowDim <- 1:nrow(x)
  #  }
  #  else if (colDimMissing && !rowDimMissing) {
  #    colDim <- 1:ncol(x)
  #  }
  #  else if (!rowDimMissing) {              # matches rT[1:4, 3:5] and rT[1:4, ]
  #    rowDim <- tryCatch(
  #      expr  = eval(rowDim),
  #      error = function (e) { 
  #        dynGet(deparse(rowDim)) 
  #      }
  #    )
  #  }

  # The function returns the number of generations to search back in the 
  # call stack.  [2020 01 30]
  # find_rowDim <- function (rowDim) {
  #   rowDimString    <- deparse(rowDim)  # for example, "i" 
  #   sysCallList     <- sys.calls()
  #   sysCallLength   <- length(sysCallList)
  #   rowDimPositions <- grep(paste0("\\[\\s*", rowDimString, "\\s*,"), sysCallList)
  #   ifelse(length(rowDimPositions)>0, sysCallLength - rowDimPositions, NULL)
  # }
  #
  # tryCatch(
  #   expr  = eval(rowDim),
  #   error = function (e) { browser(); dynGet("i") }
  #   error = function (e) {
  #     goBack <- find_rowDim()
  #     if (!is.null(goBack)) eval(rowDim, envir = parent.frame(goBack - 1))
  #     else print(paste0("Could not evaluate \"i\"\n", e))
  #   }
  # )
  
  #  if (colDimMissing && !rowDimMissing) {  # matches rT[1:4, ]
  #    colDim <- 1:ncol(x)
  #  } 
  #  else if (!colDimMissing) {              # matches rT[1:4, 3:6] and rT[, 3:6]
  #    colDim <- tryCatch(
  #      expr  = eval(colDim),
  #      error = function (e) { 
  #        dynGet(deparse(colDim))
  #      })
  #  }
    
  
  # SUBSET THE regTABLE OBJECT
  x <- unclass(x)[...]
  
  
  
  # RESTORE "MATRIX" CLASS TO X
  # If only one column has been selected, we return a one-column matrix. If 
  # only one row has been selected, we return a one-row matrix. (Later, we 
  # will add the "regTable" class to the one-row matrix.)  [2020 01 30]
  if (length(attributes(x)$dim) == 2) class(x) <- "matrix"
  else if (length(colDim) == 1 && length(rowDim)>1) x <- as.matrix(x)
  else if (length(rowDim) == 1 && length(colDim)>1) x <- as.matrix(x) %>% t  
  
  
  
  # ASSIGN ROW NAMES AND COLUMN NAMES IF NEEDED
  # If only a single row or column was selected, names have been stripped.
  # We re-assign them here.  [2020 01 30]
  if (class(x)=='matrix' & !is.null(rowDim) && is.null(rownames(x))) {
    rownames(x) <- dimnamesX[[1]][rowDim]
  }
  if (class(x)=='matrix' & !is.null(colDim) && is.null(colnames(x))) {
    colnames(x) <- dimnamesX[[2]][colDim]
  }
  
  
  
  # CHECK: ARE REMAINING COLUMNS COLUMN TIERS?
  # Or is there at least one tier from which the user removed one column but 
  # not both?
  is_odd  <- function (x) x %% 2 == 1
  is_even <- function (x) x %% 2 == 0
  if (any( is_odd(colDim) & !(colDim+1)%in%colDim)) {        # if est. column isn't followed by its SE column
    onlyIntactColTiers <- FALSE
  } 
  else if (any( is_even(colDim) & !(colDim-1)%in%colDim)) {  # if SE column isn't preceded by its est. column
    onlyIntactColTiers <- FALSE
  } 
  else {
    onlyIntactColTiers <- TRUE
    colDimOdd <- colDim[is_odd(colDim)]
    colTiers  <- if (is.null(colDim)) 1:(ncol(x)/2) else ceiling(colDimOdd/2)
  } 
    

  # RESTORE ATTRIBUTES
  # We restore attributes only if the remaining columns form intact column 
  # tiers. For example, if a user saved an estimate column from one tier but 
  # not the corresponding SE column, we don't restore attributes.
  #   When we -do- restore attributes, we restore only the attributes for the 
  # remaining column tier. For example, if the user removed the second column
  # tier (columns 3-4), we remove the corresponding entries in the "N", 
  # "r.squared", and "SER" attributes.  [2020 01 29]  
  if (onlyIntactColTiers) {
    class(x) <- c("regTable", class(x))
    if (!is.null(attrX$N))         attributes(x) <- c(attributes(x), list(N         = attrX$N[colTiers]))
    if (!is.null(attrX$r.squared)) attributes(x) <- c(attributes(x), list(r.squared = attrX$r.squared[colTiers]))
    if (!is.null(attrX$SER))       attributes(x) <- c(attributes(x), list(SER       = attrX$SER[colTiers]))
  }
  

  
  # MESSAGE  
  # If users are removing rows, and attributes N, r.squared, and SER are
  # unchanged, we remind them that the attributed are unchanged.  [2020 01 30]
  presentAttributes <- ifelse(
    is.null(attrX$r.squared),
    'attribute "N" is unchanged.',
    'attributes "N", "r.squared", and "SER" are unchanged.')
  
  if (sys.nframe() > 2) {
    prevCall <- match.call(call = sys.call(-2))
    latexTableCall <- as.character(prevCall)[1] == 'latexTable'
  }
  else {
    latexTableCall <- FALSE
  }
  
  # We add the "length(callArgs) < 3" condition to accommodate str() and 
  # str.default(). See the comment above on str() for details.  [2020 01 29]
  #   We exclude calls from latexTable(). It does a lot of subsetting of 
  # regTable objects, and we don't want to throw warnings in those cases.  
  # [2020 01 29]
  if (onlyIntactColTiers && !rowDimMissingWhenPassed && length(callArgs) >= 3 && !latexTableCall) {
    message(paste0("Rows removed, but ", presentAttributes))
  }  

  
  # RETURN OBJECT
  x 
}




#' Print regTable objects.
#' 
#' \code{print.regTable()} is the default \code{print} method for objects 
#' produced by \linkInt{regTable}. The output that it produces differs from 
#' \code{print.table()} output because \code{print.regTable()}
#' * right-aligns row names;
#' * inserts one space between each estimate and SE column, but two spaces 
#'   between estimate-SE column tiers, thereby making it easier to distinguish
#'   between results from different regressions;
#' * uses two rows of column headings: one to show the outcome for each 
#'   regression, and another to distinguish columns of coefficient estimates 
#'   from columns of SE estimates;
#' * does not print attributes (e.g.,  "N", "r.squared").
#' @md

#' @param x regTable object.
#' @param decimalPlaces Integer. Table entries will be rounded to this number
#' of digits after the decimal point. By default, it is the value of 
#' \code{getOption("Bullock.print.regTable.dp")}, which is set to{\NB}2 when 
#' the Bullock package is loaded. You can change the default by running 
#' \code{options("Bullock.print.regTable.dp") <- N} at any time, where
#' \code{N} is your preferred number of digits after the decimal place.
#' @param ... Arguments passed to \code{print.table()}.
#' @importFrom rlang .data
#' @export 
print.regTable <- function (
  x, 
  decimalPlaces = getOption("Bullock.print.regTable.dp"), 
  ...) {

  # We are right-aligning the data columns. Each column-tier has a 
  # "column-tier name," and we are right-aligning this name, too. In some 
  # cases, we need to "push" the tier to the right so that it lines up with 
  # its name. In other cases -- when the width of the name is less than the 
  # width of the column -- we push the name right to line up with the 
  # right edge of the column.  [2020 01 20]
  
  
  # RIGHT-ALIGN THE ROW NAMES
  if (! is.null(rownames(x))) {
    rowNames <- rownames(x)
    maxNchar <- max(nchar(rowNames))
    rowNames <- stringr::str_pad(rowNames, width = maxNchar, "left")
    rownames(x) <- rowNames
  } 
  
    
  # ROUND MATRIX AND CONVERT TO CHARACTER
  x <- round(x, decimalPlaces)
  x <- matrix(
    as.character(x), 
    nrow = nrow(x), 
    dimnames = list(rownames(x), colnames(x)))
  
  
  # ADD TRAILING ZEROES TO ENTRIES THAT ARE TOO SHORT
  # For example, if decimalPlaces == 3 and an entry is .320, the entry will be
  # shortened to "0.32", which will wreak havoc with alignment. We need it to 
  # be "0.320".  [2020 01 20]
  if (decimalPlaces > 0) {
    for (i in 1:ncol(x)) x[, i] <- gsub('^0$', '0.', x[, i])  # replace "0" with "0."
    xAfterDecimal <- apply(                                      # strip everything before the decimal point
        X      = x, 
        MARGIN = 2, 
        FUN    = function (x) gsub('-?\\d+\\.', '', x))   
    xAfterDecimal <- matrix(xAfterDecimal, nrow = nrow(x))       # necessary if x has only one row
    xAfterDecimalDigits <- nchar(xAfterDecimal)                  # matrix
    zeroesToAdd         <- decimalPlaces - xAfterDecimalDigits   # matrix
    for (i in 1:ncol(x)) {
      x[, i] <- stringr::str_pad(
        string = x[, i], 
        width  = nchar(x[, i]) + zeroesToAdd[, i],
        side   = 'right',
        pad    = '0')
    }
  }

  # CONVERT NA TO ''
  # Protects cells from being written as "NA"
  x[is.na(x)] <- ''  

  
  # 1) INSERT ONE SPACE AT LEFT OF EACH ESTIMATE STRING (AFTER THE FIRST COLUMN)
  # We want the column tiers to be separated by two spaces, not one. It turns 
  # out that this is the easiest way.  [2020 01 20]
  if (ncol(x) > 2) {
    for (i in seq(1, ncol(x), by = 2)) {  # for all "Est" columns
      x[, i] <- paste0(' ', x[, i])
    }
  }  
  
  
  # 2) GET COLUMN-TIER NAMES AND PAD THEM
  # It turns out to be easiest to insert an extra space to the left of each
  # column-tier name (save the first) here rather than later.  [2020 01 20]
  if (is.null(colnames(x))) {
    if (ncol(x) == 2) colTierNames <- NULL
    else              colTierNames <- paste0("(", 1:(ncol(x)/2), ")")
  }
  else {
    colTierNames <- colnames(x)[seq(1, ncol(x), by = 2)]  # Sepal.Length, etc.
  }
  if (length(colTierNames) > 1) {
    colTierNames[2:length(colTierNames)] <- paste0(' ', colTierNames[2:length(colTierNames)])
  }
  
  
  # SET COLUMN NAMES
  # The column names are all "Est" or "SE". They will all be right-aligned by 
  # the "right = TRUE" argument that we pass to print.table() at the end of 
  # the function.  [2020 01 20]
  colnames(x) <- rep(qw("Est SE"), ncol(x)/2)

  
  # 3) GET WIDTHS FOR COLUMNS, COLUMN TIERS, AND GUTTERS BETWEEN TIERS
  # * columnTierWidth is the width, in characters, of the "Est" and "SE" 
  #   columns in a tier -- plus the space that separates them. Recall that 
  #   most "Est" columns were widened by one space in step 1.  [2020 01 20]
  columnNcharMax  <- apply(x, 2, function (x) max(nchar(x, keepNA = FALSE)))
  columnTierWidth <- rep(NA, ncol(x)/2) 
  for (i in seq_along(columnTierWidth)) {
    columnTierWidth[i] <- columnNcharMax[i*2-1] + columnNcharMax[i*2] + 1
  }
  
  
  # 4) RIGHT-ALIGN COLUMN TIERS AND COLUMN-TIER NAMES
  # If decimalPlaces == 0, columnTierWidth will be 4 for each tier: one space 
  # at the start of the tier, one digit, one space, and then one concluding 
  # digit. But the width that counts is 5, not 4: because the second column is 
  # "SE", it takes up two spaces, even though it contains only one digit. Tbis
  # is why we use max(columnTierWidth[i], 5) when calculating tier widths.
  # [2020 01 21]  
  for (i in seq_along(columnTierWidth)) {
    tierPad <- max(columnTierWidth[i], 5) - nchar(colTierNames[i])
    if (tierPad < 0) {                 # if column name is wider than column tier...
      x[, i*2-1] <- stringr::str_pad(  # ...add space to "Est" column in each tier
        string = x[, i*2-1],           
        width  = columnNcharMax[i*2-1] + abs(tierPad))
    }
    
    else if (tierPad > 0 && !is.null(colTierNames)) {  # if column name is narrower than column tier...
      colTierNames[i] <- stringr::str_pad(
        string = colTierNames[i],
        width  = nchar(colTierNames[i]) + tierPad)
    }
    
  }
  
  

  # PRINT OUTPUT
  rownameMarginNchar <- max(nchar(rownames(x))) + 1
  rownameMarginSpace <- paste0(rep(' ', rownameMarginNchar), collapse = '')
  colTierNameString  <- paste0(colTierNames, collapse = ' ')
  
  # Print the colTierNameString that names the outcome for each regression. 
  # But if colTierNameString is empty (because colTierNames is NULL, because
  # the outcome names couldn't be recovered from the regression objects),
  # don't print anything.  [2020 01 23]
  if (colTierNameString != '') {   
    if (requireNamespace("crayon", quietly = TRUE))
      cat( crayon::bold(paste0(rownameMarginSpace, colTierNameString, "\n")) )
    else 
      cat( paste0(rownameMarginSpace, colTierNameString, "\n") )
  }
  print.table(x, right = TRUE, ...)
}



#' @export
str.regTable <- function (object, ...) {
  class(object) <- "matrix"
  utils::str(object)
}	