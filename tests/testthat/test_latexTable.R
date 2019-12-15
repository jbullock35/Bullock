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



lt2 <- latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '', 4))
lt2_colNameExpand <- latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '', 4),
  colNameExpand = TRUE)
lt2_spacerColumns <- latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '', 4),
  spacerColumns = c(0,1,2))


test_that("\\multicolumn specifications are correct", {
  expect_match(                                       # first column
    lt2,
    '^\\s+\\\\multicolumn\\{2\\}\\{c\\}\\{1\\}\\s+&\\s*',
    all = FALSE)
  expect_match(                                       # second and third columns
    lt2,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{\\}\\s+&\\s*',
    all = FALSE)
  expect_match(                                       # fourth column
    lt2,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{4\\}\\\\tabularnewline',
    all = FALSE)

  expect_match(                                       # first column heading
    lt2_colNameExpand,
    '^\\s+\\\\multicolumn\\{6\\}\\{c\\}\\{1\\}\\s+&\\s*',
    all = FALSE)
  expect_match(                                       # second column heading
    lt2_colNameExpand,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{4\\}\\\\tabularnewline',
    all = FALSE)
  
  expect_match(                                       # first column
    lt2_spacerColumns,
    '^\\s+&\\s*\\\\multicolumn\\{2\\}\\{c\\}\\{1\\}\\s+&\\s*&\\s*',
    all = FALSE)
  expect_match(                                       # second and third columns
    lt2_spacerColumns,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{\\}\\s+&\\s*',
    all = FALSE)
  expect_match(                                       # fourth column
    lt2_spacerColumns,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{4\\}\\\\tabularnewline',
    all = FALSE)
})



test_that("\\cmidrule specifications are correct", {
  expect_match(
    lt2,
    '\\\\cmidrule\\(lr\\)\\{1-2\\}\\s*\\\\cmidrule\\(lr\\)\\{3-4\\}\\s*\\\\cmidrule\\(lr\\)\\{5-6\\}\\s*\\\\cmidrule\\(lr\\)\\{7-8\\}\\s*',
    all = FALSE)

  expect_match(
    lt2_colNameExpand,
    '\\\\cmidrule\\(lr\\)\\{1-6\\}\\s*\\\\cmidrule\\(lr\\)\\{7-8\\}\\s*',
    all = FALSE)
  
  expect_match(
    lt2_spacerColumns,
    '\\\\cmidrule\\{2-4\\}\\s*\\\\cmidrule\\{6-7\\}\\s*\\\\cmidrule\\{8-9\\}\\s*\\\\cmidrule\\{10-11\\}\\s*',
    all = FALSE)
})


