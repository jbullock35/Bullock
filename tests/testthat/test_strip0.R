# test_strip0.R
# Created on 2020-06-26
# Created by John Bullock

context("Check strip0() output")

test_that("strip0() returns expected output for scalars", {
  expect_equal(strip0("0.346"),  ".35")
  expect_equal(strip0(0.346),    ".35")
  expect_equal(strip0(.346),     ".35")
  expect_equal(strip0(5.346),   "5.35")
})

test_that("strip0() returns expected output for vectors", {
  expect_equal(strip0(c("0.789", ".2346", 53.3, 53.346)),  c(".79", ".23", "53.3", "53.35"))
})

