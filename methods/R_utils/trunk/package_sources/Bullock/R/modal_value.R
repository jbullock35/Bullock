# modal_value.R
# copied from http://rwiki.sciviews.org/doku.php?id=tips:stats-basic:modalvalue

modal_value <- function(x, na.rm=FALSE)
{
    x = unlist(x);
    if(na.rm) x = x[!is.na(x)]
    u = unique(x);
    n = length(u);
    frequencies = rep(0, n);
    for(i in 1:n)
    {
        if(is.na(u[i]))
        {
            frequencies[i] = sum(is.na(x))
        } else
        {
            frequencies[i] = sum(x==u[i], na.rm=TRUE)
        }
    }
    u[which.max(frequencies)]
}


