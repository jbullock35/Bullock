# test_lNA.R
# Created on 2019-01-18
# Created by John Bullock

context("Check lNA() output")

x <- c(3, NA, NA, 5)
# lNA(x)            # [1] 2
# lNA1 <- lNA(x)    # no message
#
# lNAv(x)           # 1-line message         
# lNAv1 <- lNAv(x)  # 1-line message


test_that("lNA() assigns values and prints messages as it should", {
  expect_identical(lNA(x), 2L)
  expect_output(lNA(x), NA)          # Should be no printed output
  expect_output(lNA1 <- lNA(x), NA)  # Should be no printed output
})


test_that("lNAv() assigns values and prints messages as it should", {
  expect_identical(lNAv(x), 2L)
  expect_output(lNAv(x), '2 \\(length with NA=4\\)$')        
  expect_output(lNAv1 <- lNAv(x), '2 \\(length with NA=4\\)$') 
})






