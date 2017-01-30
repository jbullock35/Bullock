lNA <- function(x) {   # returns length of object without NAs
  y <- length(x[!is.na(x)])
  print(noquote(paste(y," (length with NA=",length(x),")",sep="")))
  invisible(y)
}

meanNA <- function(x) { return(mean(x,na.rm=TRUE)) }

sdNA <- function(x,na.rm=TRUE) { # borrowed from the R code -- only the default na.rm setting has changed.
  if (is.matrix(x)) 
      apply(x, 2, sd, na.rm = na.rm)
  else if (is.vector(x)) 
      sqrt(var(x, na.rm = na.rm))
  else if (is.data.frame(x)) 
      sapply(x, sd, na.rm = na.rm)
  else sqrt(var(as.vector(x), na.rm = na.rm))
}
    
sumNA <- function(x) { return(sum(x, na.rm=TRUE)) }

varNA <- function(x) {var(x, na.rm=TRUE)}
# varNA <- function (x, y = NULL, na.rm=T, use) { # borrowed from the R code -- only the default na.rm setting has changed.
#    if (missing(use)) 
#        use <- if (na.rm) 
#            "complete.obs"
#        else "all.obs"
#    na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
#    if (is.data.frame(x)) 
#        x <- as.matrix(x)
#    if (is.data.frame(y)) 
#        y <- as.matrix(y)
#    .Internal(cov(x, y, na.method))
#    }
