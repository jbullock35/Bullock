#' Transform a vector of year values into an ordered factor of year groups.
#'
#' This function is a wrapper around \linkInt{cut}. Given a vector of 
#' strings or integers that represent years, and a vector of breakpoints, it
#' returns a factor in which each level represents a group of years. Unlike 
#' \code{cut()}, it returns pretty labels for the levels: "1975-79" instead 
#' of "[1975,1980)", and so on. Also unlike \code{cut()}, it ensures that 
#' all of the data are accounted for in the levels of the factor that it 
#' creates: data will never be dropped from a factor that \code{cutYears()}
#' returns.
#' 
#' By default, \code{cutYears()} differs from \code{cut()} in the following 
#' ways:
#' 
#' * Accepts only `x` vectors in which every value has four characters or 
#'   four digits.
#' * Returns a factor that has better labels for groups of years: for 
#'   example, "1975-80" rather than "\[1975,1980)".
#' * Returns factor levels that encompass all values of `x`. Consequently, 
#'   `cutYears()` will never convert year values to `NA`, as `cut()` will 
#'   often do. 
#' * Returns an ordered factor by default.
#' * By default, `cutYears()` drops levels that are outside the bounds
#'   of `x`. For example, if `x` ranges from 1975 to 1985, the factor 
#'   returned by `cut()` may have an infinite number of levels, including,
#'   say, "(1900-1905]". (The exact levels returns by `cut()` depend on the 
#'   arguments passed to it, especially the `breaks` argument.) But in a 
#'   case like this, the lowest factor level returned by `cutYears()` will
#'   include 1975, and the highest factor level returned by `cutYears()` 
#'   will contain 1985.
#' @md 


#' @param x Vector of four-digit integers, or of four-character strings 
#' that can be converted to integers, e.g., "1900".
#' 
#' @param breaks Numeric vector of cutpoints
#' 
#' @param levelsBoundedByData Ensures that the lowest and highest levels of
#' the returned factor will contain some data. Also ensures that the label
#' for the highest factor level reports the maximum year in \code{x}, rather
#' than a higher year.
#' 
#' @examples
#' years <- rep(1975:1993, each = 3)
#' fac1a <- cut(     years, breaks = seq(1975, 1993, by = 3))
#' fac1b <- cutYears(years, breaks = seq(1975, 1993, by = 3))
#' table(fac1a)
#' table(fac1b)
#' 
#' fac2a <- cut(     years, breaks = seq(1975, 1990, by = 3))
#' fac2b <- cutYears(years, breaks = seq(1975, 1990, by = 3))
#' table(fac2a)
#' table(fac2b)
#' 
#' fac3a <- cut(     years, breaks = seq(1955, 1990, by = 3))
#' fac3b <- cutYears(years, breaks = seq(1955, 1990, by = 3))
#' table(fac3a)
#' table(fac3b)

 

 
#' @seealso 
#' \linkInt{cut}, \code{\link[Hmisc:cut2]{Hmisc::cut2()}}


#' @export 
cutYears <- function (x, breaks, levelsBoundedByData = TRUE) {
  
  if (! inherits(x, qw("character integer numeric"))) {
    stop('"x" must be of class "character", "integer", or "numeric".')
  }
  if (! inherits(x, qw("character integer numeric"))) {
    stop('"x" must be of class "character", "integer", or "numeric".')
  }
  
  x <- as.integer(x)
  
  if (minNA(x) < 1000 | maxNA(x) > 9999) {
    stop("'x' must range from 1000 to 9999")
  }
  
    
  # If min(breaks) is greater than the earliest year, add a new breakpoint 
  # that is earlier than any others.  [2021 01 16]
  if (min(x) < min(breaks)) {
    message("You have set min(x) < min(breaks). Revising factor levels accordingly.")
    breaks <- c(min(x), breaks)
  }
  
  # If "breaks" doesn't include the maximum year, add a new breakpoint.
  if (max(breaks) < max(x)) {
    breaks <- c(breaks, max(x))
  }
  
  # If levelsBoundedByData is TRUE and the maximum breakpoint exceeds the 
  # maximum year, reduce the maximum breakpoint to the maximum year.
  if (levelsBoundedByData & max(breaks) > max(x)) {

    # If multiple breakpoints are greater than the maximum year, keep only 
    # the smallest of them.
    while (length(which(breaks > max(x))) > 1) {
      breaks <- breaks[-length(breaks)]
    } 
    
    # If the second-highest breakpoint is the same as the maximum year, 
    # eliminate the highest breakpoint. Otherwise, reduce the highest 
    # breakpoint to the maximum year.
    if (breaks[length(breaks) - 1] == max(x)) {
    } 
    else {
      breaks[which.max(breaks)] <- max(x)
    }
  }
 
  
  
  # CONSTRUCT LABELS FOR EACH LEVEL OF THE FACTOR
  yearLabels <- NULL
  for (i in 1:(length(breaks) - 1)) {
    # We are constructing labels of the form {breaks[1]-(breaks[2]-1)}, 
    # except that the last label is simply of the form {breaks[1]-(breaks[2])}.
    # For example, if the breaks are c(1975, 1979, 1983), the labels are
    # "1975-78" and "1979-83".
    secondYearOffset <- 1 * (i+1 != length(breaks))  # always 0 or 1
    firstYearLabel  <- breaks[i]
    secondYearLabel <- breaks[i+1] - secondYearOffset
    if (substr(firstYearLabel, 1, 2) == substr(secondYearLabel, 1, 2)) {
      secondYearLabel <- substr(secondYearLabel, 3, 4)
    }
    newLabel   <- paste(firstYearLabel, secondYearLabel, sep = "-")
    yearLabels <- c(yearLabels, newLabel)    
  }
  
  
  # CREATE ORDERED FACTOR
  yearFac <- cut(
    x              = x, 
    breaks         = breaks, 
    labels         = yearLabels, 
    include.lowest = TRUE, 
    right          = FALSE,
    ordered_result = TRUE)
  
  
  # DROP EMPTY LEVELS THAT ARE OUTSIDE THE RANGE OF THE DATA
  if (levelsBoundedByData) {
    
    levelIndices_missing    <- which(table(yearFac) == 0)
    levelIndices_nonMissing <- which(table(yearFac) > 0)
    
    # drop levels that have no data and that are before the first level 
    if (any(levelIndices_missing < min(levelIndices_nonMissing))) {
      levels(yearFac)[levelIndices_missing < min(levelIndices_nonMissing)] <- NA
    }
    
    # drop levels that have no data and that are after the last level
    if (max(levelIndices_nonMissing) < length(levels(yearFac))) {
      levels(yearFac)[(max(levelIndices_nonMissing)+1):length(levels(yearFac))] <- NA
    }
  }
  
  yearFac
}
