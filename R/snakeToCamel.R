#' Transform a string from snake_case to camelCase.
#'
#' Given a string like "mean_attitude", the function returns "meanAttitude".
#' But for any given string segment involving an underscore character, the 
#' the segment will be transformed only if the character before the 
#' underscore is a lower-case letter. For example, "MEAN_attitude" won't be 
#' transformed. Nor will "PID3_early."

#' @param x A string.
#' 
#' @examples 
#' snakeToCamel("party_ID")      # [1] "partyID"
#' snakeToCamel("hours_9_to_5")  # [1] "hours9_to_5" 
#' 
#' @export 
snakeToCamel <- function(x) {
  gsub("([[:lower:]])_(\\w)", "\\1\\U\\2", x, perl = TRUE)
}
