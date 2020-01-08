## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(Bullock)

## ----quickstart-regtable, R.options = list(width = 200)-----------------------------------------------------------------------------------------------------------------------------------------------
data(iris)
lm1    <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2    <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
lm3    <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
lmList <- list(lm1, lm2, lm3)
rT1    <- regTable(lmList)
rT1

## ----quickstart-latexTable----------------------------------------------------
lT1 <- latexTable(
  mat      = rT1,
  rowNames = c("Intercept", "Petal length", "Petal width", "Petal length $\\times$ petal width"),
  colNames   = lt_colNumbers(),
  caption    = '\\textit{Sepal length as a function of petal length and petal width.}'
)

head(lT1)

tail(lT1)

## ----quickstart-latexTablePDF, eval = FALSE-----------------------------------
#  latexTablePDF(lT1, outputFilenameStem = 'irisTable')

## ----quickstart-latexTablePDF-tex, eval = FALSE-------------------------------
#  latexTablePDF(lT1, outputFilenameStem = 'irisTable', writeTex = TRUE)

## ----update-------------------------------------------------------------------
lm1v <- update(lm1, subset = (Species == 'versicolor'))
lm2v <- update(lm2, subset = (Species == 'versicolor'))
lm3v <- update(lm3, subset = (Species == 'versicolor'))
rT2  <- regTable(list(lm1v, lm2v, lm3v))
lT2  <- update(lT1, mat = rT2)
lT2[23:27]  # just showing the data rows

## ----tweak-by-hand------------------------------------------------------------
lT2[24:26]  

# Capitalize first letter of each word that is preceded by a space
lT2[24:26] <- gsub("([[:space:]])([[:alpha:]])", "\\1\\U\\2", lT2[24:26], perl=TRUE)
lT2[24:26]

## ----cutPaste-----------------------------------------------------------------
latexTable(
  headerFooter  = FALSE,
  mat           = rT1,
  rowNames      = c("Intercept", "Petal length", "Petal width", "Petal length $\\times$ petal width"),
  spacerColumns = c(0, 2, 4)
)

