# test_makePercentage.R
# Created on 2021-03-06
# Created by John Bullock

context("Check makePercentage() output")

test_that("makePercentage() returns expected output from scalar input", {
  expect_equal(makePercentage(.5),           "50%")
  expect_equal(makePercentage(.555),         "56%")
  expect_equal(makePercentage(.555, dp = 4), "55.5000%")
  expect_equal(makePercentage(5),            "500%")
  expect_equal(makePercentage(5, dp = 2),    "500.00%")  
})

test_that("makePercentage() returns expected output from vector input", {
  expect_equal(makePercentage(1:3), c("100%", "200%", "300%"))
})
