# version 2006 05 31: added the objects.to.remove part
# -- without it, the result was a list of dim 0 0
# when all objects in the original .RData file were
# of the same length

# version 2006 01 06: changed because R 2.2.1 doesn't
# accept ls(pattern='*').  (R 2.0.0, which I had been
# using, did.)
move.to.df <- function(pattern = NULL, move = TRUE) {
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

  # Remove, from the list of objects (tmplist), objects that don't have the 
  # "right length."  The right length for an object is the modal length of 
  # all of the objects.
  #     There is one tricky aspect to this part of the code.  If the object is 
  # not a scalar (e.g., of class "numeric" or "character") but is instead a 
  # matrix or a data frame, each of its columns may be of the right length, 
  # but length(myMatrix) != modal_value(lengthsOfObjects), and so by default,
  # matrices and data frames won't be removed from tmplist.  I add some extra
  # code to counter this problem.
  n <- modal_value(sapply(tmplist,length))            # length of most objects.  Should be the number of respondents in the dataset.
  multiColumnObjectsPos <- which(
    x = sapply(
      X   = tmplist,
      FUN = function (x) any(grepl('matrix|data.frame', class(x)))))
  multiColumnObjects        <- tmplist[multiColumnObjectsPos]
  names(multiColumnObjects) <- tmp[multiColumnObjectsPos]
  
  # Remove objects from tmplist.  Multi-column objects (e.g., matrices, 
  # data frames) will almost certainly be removed.  [2014 11 30]
  objectsToRemove <- which( sapply(tmplist, length) != n)  
  if (length(objectsToRemove) > 0) {
    tmplist  <- tmplist[-(objectsToRemove)]        # remove from tmplist objects that don't belong, like "pattern" and "last.warning"
    tmp      <- tmp[-(objectsToRemove)]            # remove from tmp the names of objects that don't belong
    tmp      <- c(tmp, names(multiColumnObjects))  # add back in the names of multi-column objects, which were removed in the previous line
    tmp     <<- tmp                                # re-create tmp in the parent frame
  }
  my.df <- do.call("data.frame", tmplist)    # create df from the list that contains the actual objects  
  
  # Add the multi-column objects.  [2014 11 30]
  for (i in names(multiColumnObjects)) {
    my.df[, i] <- multiColumnObjects[i]
  }
  
  
  # CLEAN UP
  if (move == TRUE) {
    # multiColumnObjNames <- paste0(names(multiColumnObjects), collapse = "|")
    if (!missing(pattern)) { 
      pattern <<- paste0(pattern, '|tmp|pattern')
      eval.parent(expression(rm(list = ls(pat=pattern))))
    }
    else {
      eval.parent(expression(rm(list = tmp)))
      eval.parent(expression(rm(tmp)))
    }
  }
  else { 
    eval.parent(expression(rm(tmp, pattern))) 
  }
  return(my.df)  
}
