# modal_value.R
#
# Revised 2017 April 26 from http://stackoverflow.com/a/8189441/697473.
#
# When there are multiple modes, this function returns only the first one.
# To get all modes, replace the last line with 
#   tab <- tabulate(match(x, ux))
#   ux[tab == max(tab)]  

modal_value <- function(x, na.rm = FALSE) {
  if (na.rm) {
    ux <- unique(x[!is.na(x)])
  } else {
    ux <- unique(x)
  }
  ux[which.max(tabulate(match(x, ux)))]  
}


# copied from http://rwiki.sciviews.org/doku.php?id=tips:stats-basic:modalvalue
#
#modal_value <- function(x, na.rm = FALSE)
#{
#    x = unlist(x);
#    if(na.rm) x = x[!is.na(x)]
#    u = unique(x);
#    n = length(u);
#    frequencies = rep(0, n);
#    for(i in 1:n)
#    {
#        if(is.na(u[i]))
#        {
#            frequencies[i] = sum(is.na(x))
#        } else
#        {
#            frequencies[i] = sum(x==u[i], na.rm=TRUE)
#        }
#    }
#    u[which.max(frequencies)]
#}
#
#
