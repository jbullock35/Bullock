# reorderAllFactorsWithSameLevels.R
# Created on 2022-10-14
# Created by John Bullock

# Find all factors in a namespace that share a common set of factor levels.
# Then reorder them.
#
# There are much more elegant ways to accomplish this task. But this code
# works and requires no extra packages.  [2022 10 14]


LEVELS_TO_CHECK <- c("a", "b")

fac1 <- factor(c("a", "b"))
fac2 <- factor(c("a", "b"))
fac3 <- ordered(c("a","b"))
fac4 <- factor(c("x", "y", "z"))

factorsToReorder <- NULL  # Will be a vector of factor names
for (myObjName in ls()) {
  myObj <- get(myObjName)
  if (inherits(myObj, "factor") && all(LEVELS_TO_CHECK %in% levels(myObj))) {
    factorsToReorder <- c(factorsToReorder, myObjName)
  }
}

for (myFacName in factorsToReorder) {
  myFac <- get(myFacName)
  assign(myFacName, factor(myFac, levels = c("b", "a")))
}

fac1
fac2
fac3
fac4



# Return a list of recoded factors.  
#
# Map(
#   f = function (x) { levels(x) <- c("b", "a") },
#   factorsToReorder
#   ) 