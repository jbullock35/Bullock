#' Transform a string that has spaces to camelCase.
#'
#' Given a string like "mean attitude", the function returns "meanAttitude".
#' But if the character just before the space is uppercase, the space will 
#' be replaced by an underscore (_): "TV news" will become "TV_news". 

#' @param x A string.
#' 
#' @examples 
#' spaceToCamel("colleges and universities")  # [1] "collegesAndUniversities"
#' spaceToCamel("TV news")                    # [1] "TV_news" 
#' spaceToCamel("TV News")                    # [1] "TV_News"
#' 
#' @seealso 
#' \linkInt{snakeToCamel}
#' 
#' @export 
spaceToCamel <- function(x) {
  x <- gsub("([[:lower:]])\\s(\\w)", "\\1\\U\\2", x, perl = TRUE)
  x <- gsub("([[:upper:]])\\s(\\w)", "\\1_\\2",   x, perl = TRUE)
  x
}
