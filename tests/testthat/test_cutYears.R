# test_lNA.R
# Created on 2019-01-18
# Created by John Bullock

context("Check cutYears() output")

years1   <- rep(1975:1993, each = 3)
years2   <- rep(1975:2010, each = 3)
yearFac1 <- cutYears(years1, breaks = seq(1975, 1983, by = 3))
yearFac2 <- cutYears(years1, breaks = seq(1975, 1990, by = 3))
yearFac3 <- cutYears(years2, breaks = seq(1975, 2020, by = 3))  # THE "1999-01" LABEL IS A PROBLEM!
yearFac4 <- cutYears(years2, breaks = seq(1975, 2000, by = 3))  # THE "1999-10" LABEL IS A PROBLEM!



test_that("cutYears() drops no data", {
  expect_identical(length(years1), length(yearFac1))
  expect_identical(length(years1), length(yearFac2))
  expect_identical(length(years2), length(yearFac3))
  expect_identical(length(years2), length(yearFac4))  
})


test_that("cutYears() accurately sorts years into factor levels when years are in [1900, 1999]", {
  for (myFac in list(yearFac1, yearFac2)) {
    myFacLevels   <- levels(myFac)
    for (myLevLabel in myFacLevels) {
      myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
      amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
      myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
      numYears      <- sum(years1 >= myLevRange[1]  &  years1 <= myLevRange[2])      
      expect_identical(numYears, sum(myFac == myLevLabel))
    }
  }
})


# test_that("cutYears() accurately sorts years into factor levels when years are in different centuries", {
#   for (myFac in list(yearFac3, yearFac4)) {
#     myFacLevels   <- levels(myFac)
#     for (myLevLabel in myFacLevels) {
#       myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
#       amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
#       myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
#       numYears      <- sum(years2 >= myLevRange[1]  &  years2 <= myLevRange[2])      
#       expect_identical(numYears, sum(myFac == myLevLabel))
#     }
#   }
# })




# test_that("cutYears() accurately sorts years into factor levels when years are in [1900, 2099]", {
# 
# })