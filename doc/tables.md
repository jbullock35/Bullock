---
title: "Building better tables in less time"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{tables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

<style>
  a, a[href^="https:"] {
    text-decoration: none;
  }
  div.sourceCode {
    margin-top: -1ex;
    margin-bottom: 6ex;
    margin-left: 0;
    margin-right: 0
  }
  h2 { 
    border-bottom: unset; 
    margin-block-end: unset;
    margin-bottom: 0.5ex;
    padding-top: 2.75ex;
  }
  p {
    margin: unset;
    margin-bottom: 2ex;
  }
  
  /* For images immediately preceded by code chunks */
  .sourceCode + p > img {  
    margin-top: -4ex;
    margin-bottom: 4ex;
  }
  
  .marginBottom1p5ex { margin-bottom:  1.5ex; }
  .marginBottom2ex   { margin-bottom:  2.0ex; }
  .marginBottom4ex   { margin-bottom:  4.0ex; }
  .marginTopN4p5ex   { margin-top:    -4.5ex; }
  
</style>





The `Bullock` package provides three functions to help you make tables:
`regTable()`, `latexTable()`, and `latexTablePDF()`. The functions can be
used separately but are designed to be used together. `regTable()` generates a 
matrix of regression results, `latexTable()` turns it into LaTeX code, and 
`latexTablePDF()` renders the LaTeX code as&nbsp;PDF.

These functions prioritize the careful use of white space between different
table elements, which makes tables easier to read. They also produce "Tufte
tables," in which standard errors are printed to the right of their estimates
and in smaller type. Relative to conventional formatting---which places
standard errors below the estimates and in parentheses---the formatting used
here makes it easier for readers to instantly identify the information that
they need. To my knowledge, the table design used here was first used in
Edward Tufte's essay on [The Cognitive Style of
PowerPoint](https://www.edwardtufte.com/tufte/powerpoint).

Another feature of this package is that, relative to other packages, it offers
*fewer* options for table formatting. For example, the functions in this
package offer you no opportunity to change colors or fonts, to add rules and
borders in inappropriate places, or to denote statistical significance with
asterisks or other symbols. The result is a relatively simple syntax for 
constructing tables, and a set of functions that make it difficult to 
produce bad tables.








## Quick start
Begin by generating a few regression objects with `lm()` and then putting the
results into a matrix:

```r
data(iris)
lm1    <- lm(Sepal.Length ~ Petal.Length,               data = iris)
lm2    <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
lm3    <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
lmList <- list(lm1, lm2, lm3)
rT1    <- regTable(lmList)
rT1
#>                          Sepal.Length Sepal.Length.se Sepal.Length Sepal.Length.se Sepal.Length Sepal.Length.se
#> (Intercept)                 4.3066034      0.07838896    4.1905824      0.09704587    4.5771709      0.11195206
#> Petal.Length                0.4089223      0.01889134    0.5417772      0.06928179    0.4416762      0.06550938
#> Petal.Width                        NA              NA   -0.3195506      0.16045262   -1.2393154      0.21937022
#> Petal.Length:Petal.Width           NA              NA           NA              NA    0.1885887      0.03357199
#> attr(,"class")
#> [1] "regTable" "matrix"  
#> attr(,"N")
#> [1] 150 150 150
#> attr(,"r.squared")
#> [1] 0.7599546 0.7662613 0.8078020
#> attr(,"SER")
#> [1] 0.4070745 0.4030555 0.3667382
```


Then use `latexTable()` convert the matrix into LaTeX code:












