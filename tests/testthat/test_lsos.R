# test_lsos.R
# Created on 2021-03-08
# Created by John Bullock

context("Check lsos() output")

testEnv <- new.env()
testEnv$x <- 1
testEnv$mat <- matrix(1:4, 2, 2)

test_that("lsos() returns a data frame", {
  expect_true(inherits(lsos(envir = testEnv),             "data.frame"))  
  expect_true(inherits(lsos(envir = testEnv, MB = FALSE), "data.frame"))
  expect_true(inherits(lsos(envir = testEnv),             "data.frame"))  
  expect_true(inherits(lsos(envir = testEnv, MB = FALSE), "data.frame"))  
})

test_that("lsos() returns expected output", {
  expect_equal(colnames(lsos(envir = testEnv))[2],             "Size (MB)")  
  expect_equal(colnames(lsos(envir = testEnv, MB = FALSE))[2], "Size (Kb)")    
  expect_lt(
    as.numeric(lsos(envir = testEnv)["mat", 2]), 
    as.numeric(lsos(envir = testEnv, MB = FALSE)["mat", 2]))
})

