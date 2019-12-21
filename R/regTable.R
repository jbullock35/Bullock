#' Create a matrix of regression output from a list of regression models.
#' 
#' \code{regTable} takes a list of regression objects, such as those created 
#' by \code{lm}. It returns a matrix in which the columns are estimates and 
#' standard errors -- two columns for each model. \improveCSS

#' @param objList list of regression objects. They may be of class \code{lm},
#' \code{plm}, or \code{ivreg}. This is the only required argument.
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
#' @param clusterSEs A logical scalar. If \code{TRUE}, the reported standard 
#' errors will be clustered at the level specified by \code{clusterVar}.
#' 
#' @param clusterVar A list of length \code{length(objList)}. Each element in 
#' the list indicates the clusters for the corresponding regression object in
#' \code{objList}. If the regressions in \code{objList} are of class \code{lm},
#' \code{clusterVar} is passed to \code{multiwayvcov::cluster.vcov}. If the 
#'  regressions in \code{objList} are instead of class \code{ivreg},
#' \code{clustervar} is passed to \code{ivpack::cluster.robust.se}.


#' @note Before \code{regTable} was incorporated into this package,
#' it used the \code{rowsToKeep} argument differently: variables were kept 
#' only if the \emph{beginnings} of their names matched the strings in 
#' \code{rowsToKeep}.


#' @family functions for making tables 


#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
#' lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' regTable(list(lm1, lm2))
#' regTable(list(lm1, lm2), colNames = c("Sepal length", "Sepal width"))
#' regTable(list(lm1, lm2), rowsToKeep = 'Length')
#' regTable(list(lm1, lm2), rowsToKeep = c('Intercept', 'Length'))


# TODO: add an example that involves clusterVar.  [2019 12 17]


#' @export
regTable <- function (
  objList, 
  colNames     = NULL, 
  rowsToRemove = NULL, 
  rowsToKeep   = NULL,
  clusterSEs   = FALSE,
  clusterVar   = NULL) {

  if (! class(objList) %in% 'list') {
    stop("objList must be of class 'list'.")
  }
  if (! is.null(colNames) && length(colNames) != length(objList)) {
    stop("colNames must be NULL or the length of objList.")
  }
  classVec <- sapply(objList, class)  
  if (is.matrix(classVec)) {
    classVec <- classVec[1,]  # get first class of all models
  }
  if (! all(classVec %in% c('lm', 'plm', 'ivreg'))) {
    warning("regTable() has only been designed to work with models of class 'lm', 'plm', and 'ivreg'.")
  }
  if (clusterSEs & all(classVec == 'lm') & !requireNamespace('multiwayvcov', quietly = TRUE)) {
    stop("To calculate clustered standard errors for regressions of class \"lm\", the \"multiwayvcov\" package must be installed.")
  }
  if (clusterSEs & all(classVec == 'ivreg') & !requireNamespace('ivpack', quietly = TRUE)) {
    stop("To calculate clustered standard errors for regressions of class \"ivreg\", the \"ivpack\" package must be installed.")
  }
  if (clusterSEs & !all(classVec == 'lm') & !all(classVec == 'ivreg')) {
    stop("clusterSEs is TRUE, but regTable() can only cluster SEs only when all objects in objList are 'lm' objects or when all objects in objList are 'ivreg' objects.")
  }
  if (!is.null(clusterVar) && class(clusterVar) != 'list') {
    stop("clusterVar must be an object of class 'list'")
  }
  
  
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
  tmp      <- sapply(objList, function (x) { names(x$coefficients) } )
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
  
  
  # Get clustered vcov matrices for OLS estimates.  Then fill the output 
  # matrix that this function will return.  Use coeftest(x) here because it is 
  # more robust than summary(x)$coefficients, which sometimes gives problems 
  # when I apply it to ivreg objects.  [2012 08 01]
  if (clusterSEs == TRUE & all(classVec == 'lm')) {
    vcovs.clustered <- mapply(
      FUN     = multiwayvcov::cluster.vcov,
      model   = objList,
      cluster = clusterVar)
    coefsAndSEs <- mapply(
      FUN   = lmtest::coeftest,
      x     = objList,
      vcov. = vcovs.clustered) 
    coefsAndSEs <- sapply(coefsAndSEs, function (x) x[, c('Estimate', 'Std. Error')])
  }
  else if (clusterSEs == TRUE & all(classVec == 'ivreg')) {
    coefsAndSEs <- mapply(
      FUN = ivpack::cluster.robust.se,
      objList,
      clusterVar)
    coefsAndSEs <- lapply(coefsAndSEs, function (x) x[, c('Estimate', 'Std. Error')])    
  }
  else {  
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
  
  for (i in 1:length(coefsAndSEs)) {
    rowNamesToUse <- rowNames[rowNames %in% rownames(coefsAndSEs[[i]])]
    out[rowNamesToUse, (i*2 -1):(i*2)] <- coefsAndSEs[[i]][rowNamesToUse, ]
  } 

  
  # ADD CLASS AND ATTRIBUTES TO THE TO-BE-RETURNED OBJECT
  class(out) <- c('regTable', class(out))  
  attr(out, "N")    <- sapply(objList, nobs)
  if ( all('lm' %in% sapply(objList, class)) ) {  # if every model is an lm() model 
    attr(out, "r.squared") <- sapply(objList, function (x) summary(x)$r.squared)
  }
  out
}


# Set up print method for regTable, so that print(regTable) doesn't list 
# attributes at the bottom.  [2012 08 01]
# print.regTable <- prmatrix
print.regTable <- print.table
