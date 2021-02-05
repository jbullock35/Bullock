#' List objects and their characteristics
#' 
#' \code{lsos()} and \code{ls.objects()} are like \code{ls()} in that they list  
#' the objects in a given environment. But they provide more detail: they show 
#' the class of each object, the amount of memory devoted to each objects, and
#' the number of rows or columns of each object (if applicable).
#' 
#' \code{lsos()} is a user-friendly wrapper for \code{.ls.objects()}. It is 
#' shorthand for \code{.ls.objects(order.by="Size", decreasing=TRUE, head=TRUE, n=8)}.
#' 
#' Both functions were created by Dirk Edelbuettel and modified by JD Long and
#' John Bullock. See \url{http://stackoverflow.com/questions/1358003/} for 
#' details.

#' @author 
#' Both functions were created by Dirk Edelbuettel and modified by JD Long 
#' and John Bullock. See \url{http://stackoverflow.com/questions/1358003/} 
#' for details.
#' 
#' @return 
#' Data frame with columns "Class," "Size," "Row," and "Columns." The row 
#' names of the data frame are the names of objects in the environment.
#' 
#' 
#' @param ... Additional arguments to \code{.ls.objects()}.
#' @param MB Logical. If \code{TRUE} (the default), object size is reported 
#' in megabytes. If \code{FALSE}, it is reported in kilobytes.
#' @param pos Numeric. Specifies the position, in the search list, of the
#' environment to search. Can be specified instead of \code{envir}.
#' @param envir Environment to search. Can be specified instead of \code{pos}.
#' @param pattern String. An optional regular expression; if it is specified,
#' only objects whose names match the regular expression will be returned.
#' @param order.by String, with value "Class", "Size", "Rows", or "Columns."
#' The returned data frame will be sorted by the specified column. 
#' @param decreasing Logical variable. Should results be listed in decreasing
#' order of the \code{order.by} column? In \code{.ls.objects()} the default is
#' \code{FALSE}; in \code{lsos()}, it is \code{TRUE}.
#' @param n Numeric. Number of objects to list. In \code{lsos()}, the default 
#' is{\NB}8.  

#' @examples
#' lsos()
#' lsos(pattern = '\\.df$')  # list only objects ending in ".df"

    


#' @rdname lsos
.ls.objects <- function (pos = 1, envir = as.environment(pos), pattern, order.by,
                        decreasing=FALSE, MB = TRUE, n=NULL) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(envir = envir, pattern = pattern)
    if (length(names) == 0) {
      cat("No objects matching the pattern could be found.")
      return(invisible(NULL))
    }
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(
        names, 
        function (x) format(utils::object.size(x), units = if (MB) "MB" else "Kb", digits = 3)) %>%
      { gsub("^([\\d\\.]*).*", "\\1", .data, perl = TRUE) } %>%  # extract numbers from "0.005 Mb", etc.
      as.numeric()
    
    obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Class", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
        out <- out[c("Class", "PrettySize", "Rows", "Columns")]
        names(out) <- c("Class", "Size", "Rows", "Columns")
    if (MB) names(out)[2] <- "Size (MB)" else "Size (Kb)"
    if (!is.null(n))
        out <- utils::head(out, n)
    out
}


#' @rdname lsos
#' @export
lsos <- function(..., MB = TRUE, n = 8) {
    .ls.objects(..., MB = TRUE, order.by="Size", decreasing=TRUE, n=n)
}

