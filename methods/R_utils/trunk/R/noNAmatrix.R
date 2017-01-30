noNAmatrix <- function(x) { #matrix with listwise deletion.  x is a matrix; it might take  the form of cbind(vector1,vector2,...)
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
            mat # as the last line, this is the value assigned to any new object
    }
    else { return(x) }
} 

# for some reason I haven't discerned, this destroys vectors with no NAs.  It shouldn't act on them at all... 
