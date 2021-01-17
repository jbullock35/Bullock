# test_lNA.R
# Created on 2019-01-18
# Created by John Bullock

context("Check cutYears() output")

years1    <- rep(1975:1993, each = 3)
years1_NA <- rep(1975:1993, each = 3)
years1_NA[c(2, 20:30)] <- NA
yearFac1  <- cutYears(years1, breaks = seq(1975, 1983, by = 3))
yearFac2  <- cutYears(years1, breaks = seq(1975, 1990, by = 3))
          
years2    <- rep(1975:2010, each = 3)
yearFac3  <- cutYears(years2, breaks = seq(1975, 2020, by = 3))  
yearFac4  <- cutYears(years2, breaks = seq(1975, 2000, by = 3))  
yearFac5  <- cutYears(years2, breaks = seq(1977, 2020, by = 3))  
          
years3    <- rep(1975:2010, times = 1:36)
yearFac6  <- cutYears(years3, breaks = seq(1975, 2020, by = 3))  
yearFac7  <- cutYears(years3, breaks = seq(1975, 2000, by = 3))  
yearFac8  <- cutYears(years3, breaks = seq(1977, 2020, by = 3))  

yearFac09 <- cutYears(years1, breaks = seq(1965, 1983, by = 3)) 




test_that("cutYears() drops no data when levelsBoundedByData is TRUE", {
  expect_identical(length(years1), lNA(yearFac1))
  expect_identical(length(years1), lNA(yearFac2))
  expect_identical(length(years2), lNA(yearFac3))
  expect_identical(length(years2), lNA(yearFac4))  
  expect_identical(length(years2), lNA(yearFac5))  
  expect_identical(length(years3), lNA(yearFac6))
  expect_identical(length(years3), lNA(yearFac7))  
  expect_identical(length(years3), lNA(yearFac8))    
  expect_identical(length(years1), lNA(yearFac09))  
})


test_that("cutYears() drops no data when levelsBoundedByData is FALSE", {
  expect_identical(length(years1), lNA(cutYears(years1, breaks = seq(1975, 1983, by = 3), levelsBoundedByData = FALSE)))
  expect_identical(length(years1), lNA(cutYears(years1, breaks = seq(1975, 1990, by = 3), levelsBoundedByData = FALSE)))

  expect_identical(length(years2), lNA(cutYears(years2, breaks = seq(1975, 2020, by = 3), levelsBoundedByData = FALSE)))
  expect_identical(length(years2), lNA(cutYears(years2, breaks = seq(1975, 2000, by = 3), levelsBoundedByData = FALSE)))
  expect_identical(length(years2), lNA(cutYears(years2, breaks = seq(1977, 2000, by = 3), levelsBoundedByData = FALSE)))  

  expect_identical(length(years3), lNA(cutYears(years3, breaks = seq(1975, 2020, by = 3), levelsBoundedByData = FALSE)))  
  expect_identical(length(years3), lNA(cutYears(years3, breaks = seq(1975, 2000, by = 3), levelsBoundedByData = FALSE)))
  expect_identical(length(years3), lNA(cutYears(years3, breaks = seq(1977, 2020, by = 3), levelsBoundedByData = FALSE)))
})


test_that("cutYears() drops no data when some 'x' values are NA", {
  expect_identical(length(years1_NA), length(cutYears(years1_NA, breaks = seq(1977, 2020, by = 3))))
  expect_identical(lNA(years1_NA),    lNA(cutYears(years1_NA, breaks = seq(1977, 2020, by = 3))))
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


test_that("cutYears() accurately sorts years into factor levels when years are in different centuries", {
  for (myFac in list(yearFac3, yearFac4, yearFac5)) {
    myFacLevels   <- levels(myFac)
    for (myLevLabel in myFacLevels) {
      myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
      amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
      myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
      numYears      <- sum(years2 >= myLevRange[1]  &  years2 <= myLevRange[2])      
      expect_identical(numYears, sum(myFac == myLevLabel))
    }
  }
})



test_that("cutYears() accurately sorts years into factor levels when years are in different centuries and number of values for each year isn't a constant", {
  for (myFac in list(yearFac6, yearFac7, yearFac8)) {
    myFacLevels   <- levels(myFac)
    for (myLevLabel in myFacLevels) {
      myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
      amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
      myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
      numYears      <- sum(years3 >= myLevRange[1]  &  years3 <= myLevRange[2])      
      expect_identical(numYears, sum(myFac == myLevLabel))
    }
  }
})


test_that("cutYears() accurately sorts years into factor levels when min(breaks) < min(x)", {
  myFacLevels   <- levels(yearFac09)
  for (myLevLabel in myFacLevels) {
    myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
    amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
    myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
    numYears      <- sum(years1 >= myLevRange[1]  &  years1 <= myLevRange[2])      
    expect_identical(numYears, sum(yearFac09 == myLevLabel))
  }
})


test_that("cutYears() accurately sorts years into factor levels when some 'x' values are NA", {
  myFac <- cutYears(years1_NA, breaks = seq(1977, 2020, by = 3))
  myFacLevels   <- levels(myFac)
  for (myLevLabel in myFacLevels) {
    myLevRange    <- as.integer(unlist(strsplit(myLevLabel, "-")))
    amountToAdd   <- as.integer(substr(myLevRange[1], 1, 2)) * 100                           # e.g., 1900 if myLevRange[1] is 1956  
    myLevRange[2] <- ifelse(myLevRange[2] > 99, myLevRange[2], myLevRange[2] + amountToAdd)  # e.g., in "1905-34", change 34 to 1934
    numYears      <- sum(years1_NA >= myLevRange[1]  &  years1_NA <= myLevRange[2])      
    expect_identical(numYears, sum(years1_NA == myLevLabel))
  }
})


