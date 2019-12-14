# latexTable_tests.R
# Created on 2019-12-14
# Created by John Bullock

context("Check LaTeX output from latexTable()")

mat1 <- matrix(c(-3:12, 0.0, 0.0000, -1.0000008, NA, 1.555, 9:11), nrow = 4)
lt1     <- latexTable(mat1)
lt1_dp3 <- latexTable(mat1, decimalPlaces = 3)

test_that("numbers are formatted correctly when decimalPlaces == 2 (the default)", {
  expect_match(
    lt1,
    '-3\\.00\\s*&\\s*1\\.00\\s*&\\s*5\\.00\\s*&\\s*9\\.00\\s*&\\s*\\.00\\s*&\\s*1\\.5[56]',
    all = FALSE)
  expect_match(
    lt1,
    '-2\\.00\\s*&\\s*2\\.00\\s*&\\s*6\\.00\\s*&\\s*10\\.00\\s*&\\s*\\.00\\s*&\\s*9\\.00',
    all = FALSE)
  expect_match(
    lt1,
    '-1\\.00\\s*&\\s*3\\.00\\s*&\\s*7\\.00\\s*&\\s*11\\.00\\s*&\\s*-1\\.00\\s*&\\s*10\\.00',
    all = FALSE)
  expect_match(
    lt1,
    '\\.00\\s*&\\s*4\\.00\\s*&\\s*8\\.00\\s*&\\s*12\\.00\\s*&\\s+&\\s*11\\.00',
    all = FALSE)
})


test_that("numbers are formatted correctly when decimalPlaces == 3", {
  expect_match(
    lt1_dp3,
    '-3\\.000\\s*&\\s*1\\.000\\s*&\\s*5\\.000\\s*&\\s*9\\.000\\s*&\\s*\\.000\\s*&\\s*1\\.55[56]',
    all = FALSE)
  expect_match(
    lt1_dp3,
    '-2\\.000\\s*&\\s*2\\.000\\s*&\\s*6\\.000\\s*&\\s*10\\.000\\s*&\\s*\\.000\\s*&\\s*9\\.000',
    all = FALSE)
  expect_match(
    lt1_dp3,
    '-1\\.000\\s*&\\s*3\\.000\\s*&\\s*7\\.000\\s*&\\s*11\\.000\\s*&\\s*-1\\.000\\s*&\\s*10\\.000',
    all = FALSE)
  expect_match(
    lt1_dp3,
    '\\.000\\s*&\\s*4\\.000\\s*&\\s*8\\.000\\s*&\\s*12\\.000\\s*&\\s+&\\s*11\\.000',
    all = FALSE)
})

test_that("\\multicolumn specifications are correct", {
  # TODO: Pick up here, both with SE_table == TRUE and SE_table == FALSE. Work
  #  with the examples below.  [2019 12 14]
})



latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '', 4))
latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '3', '4'),
  colNameExpand = TRUE)


