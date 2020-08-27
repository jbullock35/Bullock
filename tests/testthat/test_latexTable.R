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
mat_char <- matrix(as.character(mat1), nrow(mat1), ncol(mat1))
mat_char[is.na(mat_char)]

# Create objects in parent environment that have the same names as objects 
# in the function environment. Do this to ensure that the function isn't 
# drawing on the parent environment. (It has happened before.)  [2020 08 27]
footerRow  <- data.frame(x=3)
footerRows <- rep(1:5, each = 10)
footerList <- data.frame(x=3)
landscape  <- data.frame(x=3)


# Create latexTables
lt1_noFormat      <- latexTable(mat1, formatNumbers = FALSE)
lt1_noFormat_dp10 <- latexTable(mat1, formatNumbers = FALSE, decimalPlaces = 10)
lt1_dp0      <- latexTable(mat1, decimalPlaces = 0)
lt1_dp1      <- latexTable(mat1, decimalPlaces = 1) 
lt1_noSC     <- latexTable(mat1, spacerColumns = NULL)
lt1_noSC_dp3 <- update(lt1_noSC, decimalPlaces = 3)
lt1_mat2col  <- update(lt1_default, mat = matrix(1:16, nrow = 8))
lt1_withFooter <- update(
  lt1_default, 
  footerRows = list(c("footer row 1", "a", "b", "c")))
lt1_withFooter_noSE <- update(
  lt1_withFooter, 
  SE_table = FALSE)


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
lT3_collapsed <- paste0(lT3, collapse = '')  # could probably use capture.output() instead of collapsing
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


test_that("data rows are correct when formatNumbers is FALSE", {
  expect_match(
    lt1_noFormat,
    '-3\\s*&\\s*1\\s*&&\\s*5\\s*&\\s*9\\s*&&\\s*0\\s*&\\s*1.555',
    all = FALSE)
  expect_match(
    lt1_noFormat,
    '-2\\s*&\\s*2\\s*&&\\s*6\\s*&\\s*10\\s*&&\\s*0\\s&\\s*9',
    all = FALSE)
  expect_match(
    lt1_noFormat,
    '-1\\s*&\\s*3\\s*&&\\s*7\\s*&\\s*11\\s*&&\\s*-1.0000008\\s*&\\s*10',
    all = FALSE)
  expect_match(
    lt1_noFormat,
    '0\\s*&\\s*4\\s*&&\\s*8\\s*&\\s*12\\s*&&\\s+&\\s*11',
    all = FALSE)
})


test_that("numprint column widths are correct when formatNumbers is FALSE and decimalPlaces == 10", {
  expect_equal(
    sum( grepl('N\\{[12]\\}\\{10\\}', lt1_noFormat_dp10) ),
    6)
})



# Note that "0" entries are kept as "0", rather than eliminated, when 
# decimalPlaces == 0.  [2020 01 13]
test_that("numbers are formatted correctly when decimalPlaces == 0", {
  expect_match(
    lt1_dp0,
    '-3\\s*&\\s*1\\s*&&\\s*5\\s*&\\s*9\\s*&&\\s*0\\s*&\\s*2',
    all = FALSE)
  expect_match(
    lt1_dp0,
    '-2\\s*&\\s*2\\s*&&\\s*6\\s*&\\s*10\\s*&&\\s*0\\s&\\s*9',
    all = FALSE)
  expect_match(
    lt1_dp0,
    '-1\\s*&\\s*3\\s*&&\\s*7\\s*&\\s*11\\s*&&\\s*-1\\s*&\\s*10',
    all = FALSE)
  expect_match(
    lt1_dp0,
    '\\s*&\\s*4\\s*&&\\s*8\\s*&\\s*12\\s*&&\\s+&\\s*11',
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


test_that("footer rows are handled correctly when SE_table is FALSE", {
  expect_match(lt1_withFooter_noSE[32], "        footer row 1 & ")
  expect_match(lt1_withFooter_noSE[33], "          \\\\multicolumn\\{1\\}\\{c\\}\\{a\\} &&")
  expect_match(lt1_withFooter_noSE[34], "          \\\\multicolumn\\{1\\}\\{c\\}\\{b\\} &&")
  expect_match(lt1_withFooter_noSE[35], "          \\\\multicolumn\\{1\\}\\{c\\}\\{c\\}\\\\tabularnewline")
})


test_that("footer rows are handled correctly when a function is passed to the footerRows argument", {
  expect_error(latexTable(mat1, footerRows = lt_rSquaredRow))
  expect_error(latexTable(mat1, footerRows = lt_rSquaredRow()))      

  lT1_footerCheck <- latexTable(rT1, footerRows = lt_rSquaredRow())
  expect_match(lT1_footerCheck[29], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.76\\} &&")  
  expect_match(lT1_footerCheck[30], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.77\\} &&")
  expect_match(lT1_footerCheck[31], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.81\\}\\\\tabularnewline")
  
  lT1_footerCheck2 <- latexTable(rT1, footerRows = list(lt_rSquaredRow()))
  expect_match(lT1_footerCheck2[29], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.76\\} &&")  
  expect_match(lT1_footerCheck2[30], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.77\\} &&")
  expect_match(lT1_footerCheck2[31], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.81\\}\\\\tabularnewline")

  lT1_footerCheck3 <- latexTable(rT1, footerRows = list(lt_rSquaredRow(), c("a", "b", "c", "d")))
  expect_match(lT1_footerCheck3[29], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.76\\} &&")  
  expect_match(lT1_footerCheck3[30], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.77\\} &&")
  expect_match(lT1_footerCheck3[31], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.81\\}\\\\tabularnewline")
  expect_match(lT1_footerCheck3[32], "        a && ")
  expect_match(lT1_footerCheck3[33], "          \\\\multicolumn\\{2\\}\\{c\\}\\{b\\} &&")
  expect_match(lT1_footerCheck3[34], "          \\\\multicolumn\\{2\\}\\{c\\}\\{c\\} &&")
  expect_match(lT1_footerCheck3[35], "          \\\\multicolumn\\{2\\}\\{c\\}\\{d\\}\\\\tabularnewline")
  
  footerList <- alist(lt_rSquaredRow, c("a", "b", "c", "d"))
  lT1_footerCheck4 <- latexTable(rT1, footerRows = footerList)
  expect_match(lT1_footerCheck4[29], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.76\\} &&")  
  expect_match(lT1_footerCheck4[30], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.77\\} &&")
  expect_match(lT1_footerCheck4[31], "          \\\\multicolumn\\{2\\}\\{c\\}\\{.81\\}\\\\tabularnewline")
  expect_match(lT1_footerCheck4[32], "        a && ")
  expect_match(lT1_footerCheck4[33], "          \\\\multicolumn\\{2\\}\\{c\\}\\{b\\} &&")
  expect_match(lT1_footerCheck4[34], "          \\\\multicolumn\\{2\\}\\{c\\}\\{c\\} &&")
  expect_match(lT1_footerCheck4[35], "          \\\\multicolumn\\{2\\}\\{c\\}\\{d\\}\\\\tabularnewline")
})



# **************************************************************************
# LANDSCAPE SETTING ####
# **************************************************************************

# NARROW TABLE
expect_length(grep('landscape', latexTable(mat1)), 0) 
expect_length(grep('landscape', latexTable(mat1, landscape = FALSE)), 0)
expect_length(grep('landscape', latexTable(mat1, landscape = TRUE)),  2) 
landscape <- FALSE
expect_length(grep('landscape', latexTable(mat1, landscape = TRUE)),  2) 
landscape <- TRUE
expect_length(grep('landscape', latexTable(mat1, landscape = FALSE)), 0)

# WIDE TABLE
mat1Wide <- cbind(mat1, mat1, mat1)
expect_length(grep('landscape', latexTable(mat1Wide)), 2) 
expect_length(grep('landscape', latexTable(mat1Wide, landscape = FALSE)), 0)
expect_length(grep('landscape', latexTable(mat1Wide, landscape = TRUE)),  2) 
landscape <- FALSE
expect_length(grep('landscape', latexTable(mat1Wide, landscape = TRUE)),  2) 
landscape <- TRUE
expect_length(grep('landscape', latexTable(mat1Wide, landscape = FALSE)), 0)



############################################################################
# FLOAT PLACEMENT
############################################################################
test_that("floatPlacement argument is used correctly", {
  expect_equal(as.character(lT3[length(lT3)]), "\\myTable{p}")
  expect_equal(as.character(lT3_placement_t[length(lT3_placement_t)]), "\\myTable{t}")
  expect_equal(as.character(lT3_placement_H[length(lT3_placement_H)]), "\\myTable{H}")    
  expect_equal(as.character(lT3_placement_not_thbH[length(lT3_placement_not_thbH)]), "\\myTable{!thbH}")        
})


