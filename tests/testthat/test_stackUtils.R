# test_stackUtils.R
# Created on 2019-01-07
# Created by John Bullock

context("Check output from the stack utilities: push(), pop(), shift(), and unshift()")

test_that("push() and pop() work as expected", {
  x <- newStack()
  push(x, 1:5)
  y <- pop(x)
  z <- pop(x)
  expect_identical(x$data, 1:3)
  expect_equal(y, 5)
  expect_equal(z, 4)
})

test_that("shift() and unshift() work as expected", {
  x <- newStack()
  shift(x, 1:5)
  y <- unshift(x)
  z <- unshift(x)
  expect_identical(x$data, 3:5)
  expect_equal(y, 1)
  expect_equal(z, 2)
})

test_that("copies of a stack object don't inherit subsequent changes to that object", {
  x <- newStack(1:5)
  xCopy <- x
  push(x, 6)
  expect_equal(x$data, 1:6)
  expect_equal(xCopy$data, 1:5)
})



