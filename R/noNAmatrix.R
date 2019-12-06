#' Perform listwise deletion on a matrix.
#' 
#' \code{noNAmatrix} performs "listwise deletion" on a matrix, removing all 
#' rows that contain any missing (NA) values.
#' 
#' This function is deprecated. Use \code{na.omit} instead.

#' @param x a matrix

#' @examples noNAmatrix(matrix(c(1:8, NA), nrow=3))

noNAmatrix <- function(x) { 
  .Deprecated("na.omit")
  x <- as.matrix(x)
  if (any(is.na(x))) {    # skip the whole function if there are already no NAs
    mat <- matrix(NA,nrow(x),ncol(x))
    count <- 1  # counter for the NA-free matrix we call "mat"
    for (i in seq(nrow(x))) {
      if (all(!is.na(x[i,]))) {    # if no NAs in the row, add it to the new "pure" matrix
          mat[count,] <- x[i,]
          count <- count+1
          }
      }
    for (i in seq(nrow(mat))) {
      if (any(is.na(mat[i,]))) {mat <- mat[1:i-1,]; break} # eliminate the "leftover" rows of NA
      }
      cat("There were",nrow(x),"total cases.\n")
      cat("There were",nrow(x)-i+1,"listwise deletions, yielding a new matrix of",nrow(mat),"cases.\n")
      mat 
  }
  else { return(x) }
} 
