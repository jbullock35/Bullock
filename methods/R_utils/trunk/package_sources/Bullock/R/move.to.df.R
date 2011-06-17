#rm(list=ls())
#load("c:/school/methods/utilities/utilities.RData")
#save.image("c:/school/methods/utilities/utilities.RData")

# version 2006 05 31: added the objects.to.remove part
# -- without it, the result was a list of dim 0 0
# when all objects in the original .RData file were
# of the same length

# version 2006 01 06: changed because R 2.2.1 doesn't
# accept ls(pattern='*').  (R 2.0.0, which I had been
# using, did.)
move.to.df <- function(pattern=NULL, move=TRUE) {
  # Copy variables matching the pattern into a data frame,
  # and perhaps delete the free-standing original variables.
  
  if (!missing(pattern)) {
    stopifnot(is.character(pattern))
    pattern <<- pattern                                   # creates pattern in parent frame
    tmp <- eval.parent(expression(ls(pat=pattern)))       # returns character vector -- a list of names
  }
  else { 
    tmp <- eval.parent(expression(ls()))
  }
  tmp <<- tmp                                           # creates tmp in parent frame
  tmplist <- eval.parent(expression(lapply(tmp,get)))   # returns list of actual objects -- not just their names  
  names(tmplist) <- tmp
  n <- mode(sapply(tmplist,length))                     # length of most objects.  Should be the number of respondents in the dataset.
  objects.to.remove <- which(sapply(tmplist,length)!=n) # positions in tmplist of objects to remove
  if (length(objects.to.remove)>0) {
    tmplist  <- tmplist[-(objects.to.remove)]           # remove from tmplist objects that don't belong, like "pattern" and "last.warning"
    tmp      <- tmp[-(objects.to.remove)]               # remove from tmp the names of objects that don't belong
    tmp     <<- tmp                                     # re-create tmp in the parent frame
  }
  my.df <- do.call("data.frame",tmplist)                # create df from the list that contains the actual objects  
  
  # clean up
  if (move==TRUE) {
    if (!missing(pattern)) { 
      pattern <<- paste(pattern,'|tmp|pattern',sep="")
      eval.parent(expression(rm(list=ls(pat=pattern))))
    }
    else {
      eval.parent(expression(rm(list=tmp)))
      eval.parent(expression(rm(tmp)))
    }
  }
  else { eval.parent(expression(rm(tmp,pattern))) }
  return(my.df)  
}



# Archived 2010 05 18
#move.to.df <- function(pattern=NULL, move=T) {
#  # Copy variables matching the pattern into a data frame,
#  # and perhaps delete the free-standing original variables.
#  
#  if (!missing(pattern)) {
#    stopifnot(is.character(pattern))
#    pattern <<- pattern                                   # creates pattern in parent frame
#    tmp <- eval.parent(expression(ls(pat=pattern)))       # returns character vector -- a list of names
#  }
#  else { 
#    tmp <- eval.parent(expression(ls()))
#  }
#  tmp <<- tmp                                           # creates tmp in parent frame
#  tmplist <- eval.parent(expression(lapply(tmp,get)))   # returns list of actual objects -- not just their names  
#  names(tmplist) <- tmp
#  n <- mode(sapply(tmplist,length))                     # length of most objects.  Should be the number of respondents in the dataset.
#  objects.to.remove <- which(sapply(tmplist,length)!=n) # positions in tmplist of objects to remove
#  if (length(objects.to.remove)>0) {
#    tmplist <- tmplist[-(objects.to.remove)]              # remove from tmplist objects that don't belong, like "pattern" and "last.warning"
#  }
#  my.df <- do.call("data.frame",tmplist)                # create df from the list that contains the actual objects  
#  
#  # clean up
#  if (move==T) {
#    if (!missing(pattern)) { 
#      pattern <<- paste(pattern,'|tmp|pattern',sep="")
#      eval.parent(expression(rm(list=ls(pat=pattern))))
#    }
#    else {
#      eval.parent(expression(rm(list=ls())))
#    }
#  }
#  else { eval.parent(expression(rm(tmp,pattern))) }
#  return(my.df)  
#}
