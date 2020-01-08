# test_stackUtils.R
# Created on 2019-01-07
# Created by John Bullock

context("Check output from the stack utilities: push(), pop(), shift(), and unshift()")

test_that("pop() works as expected", {
  x <- 1:5
  y <- pop(x)
  z <- pop(x)
  expect_identical(x, 1:3)
  expect_equal(y, 5)
  expect_equal(z, 4)
})

test_that("push() works as expected", {
  x <- 1:5
  push(x, "hello")
  expect_identical(x[5], "5")
  expect_identical(x[6], "hello")
  x <- -4:-2
  push(x, 10)
  expect_identical(x[4], 10)
})


test_that("shift() works as expected", {
  x <- 1:5
  y <- shift(x)
  z <- shift(x)
  expect_identical(x, 3:5)
  expect_equal(y, 1)
  expect_equal(z, 2)
})


test_that("unshift() works as expected", {
  x <- 1:5
  unshift(x, "hello")
  expect_identical(x[5], "4")
  expect_identical(x[1], "hello")
  x <- -4:-2
  unshift(x, 10)
  expect_identical(x[4], -2)
  expect_identical(x[1], 10)
})


test_that("copies of a stack object don't inherit subsequent changes to that object", {
  x <- 1:5
  xCopy <- x
  push(x, 6)
  expect_equal(x, 1:6)
  expect_equal(xCopy, 1:5)
})



