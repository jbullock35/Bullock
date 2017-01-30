`%IN%` <- function (x, table) {
  result <- x %in% table
  result[is.na(x)] <- NA
  result
}
