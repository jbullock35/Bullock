#' Missing-value helper functions.
#' 
#' Functions to make code a little clearer. These are mainly ordinary functions,
#' like \code{mean()}, with \code{na.rm} set to \code{TRUE}. For example, 
#' \code{meanNA()} is defined as \code{function(x) mean(x, na.rm = TRUE)}.
#' 
#' \code{lNA(x)} returns its value silently. \code{lNAv} is shorthand for 
#' \code{lNA(x, verbose = TRUE)}; it returns the same value as \code{lNA(x)}  
#' but also prints the lengths of the vector before and after NAs are removed.


#' @examples 
#' lNA(NA)    # 0
#' 
#' x <- c(1:3, NA, 5)
#' allNA(x <= 5)  # TRUE
#' lNA(x)         # 4
#' lNAv(x)        # 4 (length with NA=5)
#' 
#' sum(NA)        # NA
#' sumNA(NA)      # 0
#' 
#' sum(x)         # NA
#' sumNA(x)       # 11
#' meanNA(x)      # 2.75
#' 
#' sdNA(x)
#' varNA(x)


#' @param x An R object. 
#' @param verbose Logical variable. If \code{TRUE}, \code{lNA} will print the 
#'   lengths of \code{x} before and after \code{NA} values have been removed.
#' @name missingValueFunctions

#' @rdname missingValueFunctions
#' @export
allNA <- function(x) {
  stopifnot(!is.null(x))
  all(x, na.rm = TRUE)
}


#' @rdname missingValueFunctions
#' @export
lNA <- function(x, verbose = FALSE) {   
  stopifnot(!is.null(x))
  y <- length(x[!is.na(x)])
  if (verbose) {
    print(
      noquote(
        paste0(y, " (length with NA=", length(x), ")")
      )
    )
    invisible(y)
  }
  else {
    y
  }
}


#' @rdname missingValueFunctions
#' @export
lNAv <- function (x) lNA(x, verbose = TRUE)


#' @rdname missingValueFunctions
#' @export
maxNA <- function(x) {
  stopifnot(!is.null(x))
  max(x, na.rm = TRUE)
}


#' @rdname missingValueFunctions
#' @export
meanNA <- function(x) {
  stopifnot(!is.null(x))
  mean(x, na.rm = TRUE)
}

#' @rdname missingValueFunctions
#' @importFrom stats median 
#' @export
medianNA <- function(x) {
  stopifnot(!is.null(x))
  median(x, na.rm = TRUE)
}


#' @rdname missingValueFunctions
#' @export
minNA <- function(x) {
  stopifnot(!is.null(x))
  min(x, na.rm = TRUE)
}


#' @rdname missingValueFunctions
#' @export
rangeNA <- function(x) {
  stopifnot(!is.null(x))
  range(x, na.rm = TRUE)
}

#' @rdname missingValueFunctions
#' @importFrom stats sd
#' @export
sdNA <- function(x) {
  stopifnot(!is.null(x))
  sd(x, na.rm = TRUE)
}

#' @rdname missingValueFunctions
#' @export
sumNA <- function(x) {
  stopifnot(!is.null(x))
  sum(x, na.rm = TRUE)
}

#' @rdname missingValueFunctions
#' @importFrom stats var
#' @export
varNA <- function(x) {
  stopifnot(!is.null(x))
  var(x, na.rm = TRUE)
}
