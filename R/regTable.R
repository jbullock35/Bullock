#' Create a matrix of regression output from a list of regression models.
#' 
#' \code{regTable()} takes a list of regression objects, such as those created 
#' by \code{lm()}. It returns a matrix in which the columns are estimates and 
#' standard errors -- two columns for each model. 

#' @importFrom magrittr %>%

#' @param objList list of regression objects. They may be of class \code{lm},
#' \code{plm}, or \code{ivreg}. \code{glm} objects are OK because they are
#' also of class \code{lm}. This is the only required argument.
#' 
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
#' @param clusterVar A list of length{\NB}1 or \code{length(objList)}. Each element in 
#' the list indicates the clusters for the corresponding regression object in
#' \code{objList}. If the regressions in \code{objList} are of class \code{lm},
#' \code{clusterVar} is passed to \code{multiwayvcov::cluster.vcov}. If the 
#'  regressions in \code{objList} are instead of class \code{ivreg},
#' \code{clustervar} is passed to \code{ivpack::cluster.robust.se}. Can be 
#' \code{NULL} (the default), in which case standard errors won't be clustered.

#' @return A matrix in which the columns are estimates and 
#' standard errors -- two columns for each model. The matrix has an "N"
#' attribute that indicates the number of observations for each regression. If
#' all regressions were of class \code{lm} (but not also class \code{glm}), it 
#' also has the "r.squared" and "SER" attributes. (The "SER" attribute 
#' indicates the standard error of regression -- AKA \eqn{\sigma} or the 
#' "residual standard error" --- for each model.)


#' @note Before \code{regTable} was incorporated into this package,
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
  classVec_ok <- sapply(objList, inherits, qw("ivreg lm plm"))  # boolean
  if (! all(classVec_ok)) {
    warning("regTable() has only been designed to work with models of class 'lm', 'plm', and 'ivreg'.")
  }  
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
      if (any(c('iv_robust', 'lm_robust') %in% class(obj))) {  # objects from the "estimatr" package
        tmpNames <- c(tmpNames, obj$outcome)
      }
      if ('ivreg' %in% class(obj)) {
        tmpNames <- c(tmpNames, obj$formula[2])
      }
      else if ('lm' %in% class(obj)) {
        tmpNames <- c(tmpNames, as.character(obj$terms[[2]])[1])
      }
    }
    colNames <- tmpNames
  }
  if (! is.null(colNames)) {
    colNames <- paste0(rep(colNames, each = 2), c('', '.se'))
  }

  
  
  # Get names of predictors, including intercept.  Eliminate names of 
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





# Set up print method for regTable, so that print(regTable) doesn't list 
# attributes at the bottom.  [2012 08 01]
print.regTable <- function (x) {
  if (! is.null(rownames(x))) {
    rowNames <- rownames(x)
    maxNchar <- max(nchar(rowNames))
    rowNames <- stringr::str_pad(rowNames, width = maxNchar, "left")
    rownames(x) <- rowNames
  } 
  print.table(x)
}
