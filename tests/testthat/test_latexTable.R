# test_latexTable.R
# Created on 2019-12-14
# Created by John Bullock

context("Check LaTeX output from latexTable()")

regex_multicol2c <- '\\\\multicolumn\\{2\\}\\{c\\}'
regex_N   <- '(N|Number of observations)' 
regex_R2  <- 'R?\\$\\^2\\$'
regex_SER <- '(SER|Standard error of regression|Std. error of regression)'

mat1 <- matrix(c(-3:12, 0.0, 0.0000, -1.0000008, NA, 1.555, 9:11), nrow = 4)
lt1_default  <- latexTable(mat1) 
lt1_dp1      <- latexTable(mat1, decimalPlaces = 1) 
lt1_noSC     <- latexTable(mat1, spacerColumns = NULL)
lt1_noSC_dp3 <- update(lt1_noSC, decimalPlaces = 3)
lt1_mat2col  <- update(lt1_default, mat = matrix(1:16, nrow = 8)) 

lt2 <- latexTable(
  mat = matrix(1:16, nrow=2), 
  colNames = c('1', '', '', 4),
  spacerColumns = NULL)
lt2_colNameExpand <- update(lt2, colNameExpand = TRUE)
lt2_spacerColumns <- update(lt2, spacerColumns = c(0,1,2))

data(iris)
lm1 <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
lm3 <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
rT1 <- regTable(list(lm1, lm2, lm3))
lT3 <- latexTable(rT1)
lT3_collapsed <- paste0(lT3, collapse = '')
lT3_dp3 <- update(lT3, decimalPlaces = 3)
lT3_dp3_collapsed <- paste0(lT3_dp3, collapse = '')
lT3_placement_t        <- update(lT3, floatPlacement = 't')
lT3_placement_H        <- update(lT3, floatPlacement = 'H')
lT3_placement_not_thbH <- update(lT3, floatPlacement = '!thbH')


test_that("numbers are formatted correctly when no arguments except 'mat' are given", {
  expect_match(
    lt1_default,
    '-3\\.00\\s*&\\s*1\\.00\\s*&&\\s*5\\.00\\s*&\\s*9\\.00\\s*&&\\s*\\.00\\s*&\\s*1\\.5[56]',
    all = FALSE)
  expect_match(
    lt1_default,
    '-2\\.00\\s*&\\s*2\\.00\\s*&&\\s*6\\.00\\s*&\\s*10\\.00\\s*&&\\s*\\.00\\s*&\\s*9\\.00',
    all = FALSE)
  expect_match(
    lt1_default,
    '-1\\.00\\s*&\\s*3\\.00\\s*&&\\s*7\\.00\\s*&\\s*11\\.00\\s*&&\\s*-1\\.00\\s*&\\s*10\\.00',
    all = FALSE)
  expect_match(
    lt1_default,
    '\\.00\\s*&\\s*4\\.00\\s*&&\\s*8\\.00\\s*&\\s*12\\.00\\s*&&\\s+&\\s*11\\.00',
    all = FALSE)
})



test_that("numbers are formatted correctly when decimalPlaces == 1", {
  expect_match(
    lt1_dp1,
    '-3\\.0\\s*&\\s*1\\.0\\s*&&\\s*5\\.0\\s*&\\s*9\\.0\\s*&&\\s*\\.0\\s*&\\s*1\\.6',
    all = FALSE)
  expect_match(
    lt1_dp1,
    '-2\\.0\\s*&\\s*2\\.0\\s*&&\\s*6\\.0\\s*&\\s*10\\.0\\s*&&\\s*\\.0\\s*&\\s*9\\.0',
    all = FALSE)
  expect_match(
    lt1_dp1,
    '-1\\.0\\s*&\\s*3\\.0\\s*&&\\s*7\\.0\\s*&\\s*11\\.0\\s*&&\\s*-1\\.0\\s*&\\s*10\\.0',
    all = FALSE)
  expect_match(
    lt1_dp1,
    '\\.0\\s*&\\s*4\\.0\\s*&&\\s*8\\.0\\s*&\\s*12\\.0\\s*&&\\s+&\\s*11\\.0',
    all = FALSE)
})



test_that("numbers are formatted correctly when spacerColumns is NULL", {
  expect_match(
    lt1_noSC,
    '-3\\.00\\s*&\\s*1\\.00\\s*&\\s*5\\.00\\s*&\\s*9\\.00\\s*&\\s*\\.00\\s*&\\s*1\\.5[56]',
    all = FALSE)
  expect_match(
    lt1_noSC,
    '-2\\.00\\s*&\\s*2\\.00\\s*&\\s*6\\.00\\s*&\\s*10\\.00\\s*&\\s*\\.00\\s*&\\s*9\\.00',
    all = FALSE)
  expect_match(
    lt1_noSC,
    '-1\\.00\\s*&\\s*3\\.00\\s*&\\s*7\\.00\\s*&\\s*11\\.00\\s*&\\s*-1\\.00\\s*&\\s*10\\.00',
    all = FALSE)
  expect_match(
    lt1_noSC,
    '\\.00\\s*&\\s*4\\.00\\s*&\\s*8\\.00\\s*&\\s*12\\.00\\s*&\\s+&\\s*11\\.00',
    all = FALSE)
})


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
  
  expect_match(                                       # mat has only 2 columns
    lt1_mat2col,
    '\\\\multicolumn\\{2\\}\\{c\\}\\{\\(1\\)\\}\\\\tabularnewline',
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
  
  expect_match(
    lt1_mat2col,  # mat has only two columns
    '\\\\cmidrule\\(lr\\)\\{1-2\\}\\s*$',
    all = FALSE)
})



##############################################################################
# FOOTER ROWS
##############################################################################
test_that("footer rows are correct with default arguments", {
  expect_match(
    lT3_collapsed,
    paste0(
      regex_R2, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{.76\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{.77\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{.81\\}\\s?\\\\tabularnewline'))
  
  expect_match(
    lT3_collapsed,
    paste0(
      regex_SER, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{0?.41\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{0?.40\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{0?.37\\}\\s?\\\\tabularnewline'))

  expect_match(
    lT3_collapsed,
    paste0(
      regex_N, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{150\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{150\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{150\\}\\s?\\\\tabularnewline'))
})


test_that("footer rows are correct when decimalPlaces == 3", {
  expect_match(
    lT3_dp3_collapsed,
    paste0(
      regex_R2, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{.760\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{.766\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{.808\\}\\s?\\\\tabularnewline'))
  
  expect_match(
    lT3_dp3_collapsed,
    paste0(
      regex_SER, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{0?.407\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{0?.403\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{0?.367\\}\\s?\\\\tabularnewline'))

  expect_match(
    lT3_dp3_collapsed,
    paste0(
      regex_N, 
      '\\s+&&\\s+', 
      regex_multicol2c,
      '\\{150\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{150\\}\\s+&&\\s+',
      regex_multicol2c,
      '\\{150\\}\\s?\\\\tabularnewline'))
})


test_that("floatPlacement argument is used correctly", {
  expect_equal(as.character(lT3[length(lT3)]), "\\myTable{p}")
  expect_equal(as.character(lT3_placement_t[length(lT3_placement_t)]), "\\myTable{t}")
  expect_equal(as.character(lT3_placement_H[length(lT3_placement_H)]), "\\myTable{H}")    
  expect_equal(as.character(lT3_placement_not_thbH[length(lT3_placement_not_thbH)]), "\\myTable{!thbH}")        
})