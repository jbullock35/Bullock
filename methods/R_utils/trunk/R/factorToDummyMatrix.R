# factorToDummyMatrix.R
# created 2016 August 26

# Create a matrix of dummy variables from a factor.

factorToDummyMatrix <- function(fac) {
  stopifnot('factor' %in% class(fac))
  currentNA_action <- options('na.action')
  options(na.action = 'na.pass')

  mat <- model.matrix(~ fac - 1)

  options(na.action = as.character(currentNA_action))

  colnames(mat) <- levels(fac)
  mat    
}


