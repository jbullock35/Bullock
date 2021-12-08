# Version 2.4.0.3.9000 (2021-12-07)
* `latexTable()` now handles tibbles (#60)
* `latexTable()` now automatically escapes `%` signs in `colNames`; 
  unescaped `%` signs prevent LaTeX from compiling the .tex file produced 
  by `latexTablePDF()`.
* `latexTable()` recognizes `NaN` as missing data when replacing missing 
  data with `NA_text`.
* `latexTable()` gives an informative error message when `colNames` and 
  `colNameExpand` are used inappropriately.
* `latexTable()` now throws an error when any element in `colNames` contains
  a newline character.  
* `rescale()` now checks to ensure that `x` contains some non-missing data
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.4.0.3 (2021-11-20)
* Significant improvements to output of `reliability()`.
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.4.0.2 (2021-07-14)
* Add `suppress_warnings()`.
* Fix small problem with `makePercentage()` documentation.
&nbsp;  
&nbsp;  
&nbsp;  


# Version 2.4.0.1 (2021-03-08)
* Fix `lsos()` and add tests for it (#61).
* Add `makePercentage()`.
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.4.0 (2021-02-21)

## New functions
* `allNA` (#48), `minNA()` and `maxNA()`.
* `cutYears()` (#50), 
* `printAll()` to print all rows of a tibble.
* `sourcing()` (#49)
* `spaceToCamel()`.

## Other changes
* Missing-value functions (`lNA()` and kin) now throw an error when `x` is
  `NULL`. For example, if a user runs `lNA(foo$x)` but `x` is not an element 
  of `foo`, the function will throw an error message (#45). It previously 
  returned 0.
* Change default of `firstPageEmpty` in `latexTablePDF()` from `TRUE` to 
  `container`; that is, `firstPageEmpty` will by default be `TRUE` if 
  `container` is `TRUE`, and `FALSE` if it is `FALSE`.   
* Improve ability of `latexTable()` to handle lists that contain functions,
  e.g., `lt_rSquaredRow`.
* In `latexTable()`, check that `mat` has class `matrix` to avoid an 
  obscure error message when it doesn't (#34).
* `PDF_crop()` now throws an error when new cropped PDF isn't created (#42).  
* Change default `lsos()` output so that object size is reported in MB (#47).
* Add more tests of `latexTable()`.
* Fix test of `PDF_crop()` to handle cases in which 
  Sys.getenv("R_SESSION_TMPDIR") is an empty string.
* Deprecated `mergeFac()` in favor of `dplyr::coalesce()` (#46).
&nbsp;  
&nbsp;  

* Added, to the "R Markdown" vignette, instructions for using `latexTable()` 
  output in a Sweave / Rnw document.
* Small improvements to `latexTable()` documentation.
* Roxygenized documentation for `reliability()` and `NAMESPACE`. All 
  documentation is now roxygenized (#7).  
&nbsp;  
&nbsp;  
&nbsp;  


# Version 2.3.0 (2020-07-16) 
* Added `strip0()` (#41).
* Added `snakeToCamel()`, `medianNA()`, `rangeNA()` (#32).
* `PDF_crop()` can now take paths that have spaces.
* Reduced test_regTable.R reliance on multiwayvcov package (#36).
* Fix bug with `latexTable()` when `mat` is a tibble.
* Minor update to `qw()` documentation (#37).
&nbsp;  
&nbsp;  
&nbsp;  


# Version 2.2.2 (2020-03-22)
Updated `latexTable()` and `regTable()` for compatibility with R 4.0.0.
&nbsp;  
&nbsp;  
&nbsp;  


# Version 2.2.1 (2020-03-20)
* Added vignette on using `latexTable()` with R Markdown documents
  (#20).
* Updated `latexTable()` and `latexTable()` PDF documentation on 
  required LaTeX packages. Added check for the `float` package to 
  `latexTablePDF()`.
* Moved knitr and rmarkdown from "Suggests" to "Imports" to prevent 
  package installation errors when `build_vignettes = TRUE`
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.2.0 (2020-02-24)

## `regTable()` changes
* Removed class restrictions on objects passed to `regTable()`. `regTable()`
  now works with almost every kind of regression object.
* Added `[.regTable` method for intelligent subsetting of regTable objects (#27).
* Added global option `Bullock.print.regTable.dp` on package load so that 
  users can change default number of digits that are displayed when they
  `print` a regTable object.
* Added `str.regTable()`.
* In `print.regTable()`, use "crayon" package, when available, to render 
  column-pair headings in bold (#23). 

## Other enhancements
* Added `PDF_crop()`.
* `latexTablePDF()` is now able to write PDF files even when the latexTable 
  objects that it is processing were created with 
  `latexTable(..., callCommand = FALSE)`.
* Added `envir` argument to `lsos()` and `.ls.objects()` (#6).
* Improved vignette formatting (#19).

## Bug fix
* Fixed bug in latexTable footers when `SE_table` is `FALSE` (#24).
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.1.0 (2020-01-21)

## `regTable()` changes
* Added support for glm objects to `regTable()`.  
* Allowed `regTable()` to work with regressions of different classes (i.e., 
  "lm" and "ivreg") when clustering SEs.
* Improved `print.regTable()` output (#11, #18).
* Improved checking of classes of `objList` elements.

## `latexTable()` changes
* In `latexTable()`, changed `spaceBetweenColNameRows` argument from `TRUE` 
  to `-.025in`. Users can now arbitrarily adjust the vertical space between
  column-name rows by specifying any LaTeX length as the 
  `spaceBetweenColNameRows` argument.   
* Improved `latexTable()` output when `formatNumbers` is `FALSE`.
* Added `c.latexTable()` method (#12).
* Better `latexTable()` warning message when `colNames[[1]]` has the wrong length.

## Other enhancements
* Reduced `lNAv()` output from two lines to 1 (#16).
* If one goes to `help.start()` and then clicks on the "Bullock" entry, 
  "Bullock-package" is now the first entry in the help index.
* Added examples to documentation of stack utilities (#5) and missing-value 
  utilities.
* Small improvements to documentation formatting.

## Bug fixes
* Fixed failure to export `print.regTable()`. Printing of "regTable" objects  
  to screen will now look better. Also updated vignette with new regTable output 
  (#21).
* Fixed `latexTable()` calls for which `decimalPlaces` is 0 (#8).
* Fixed bug in `latexTable()` calls that arose when `SE_table` was `FALSE` and
  an element of the `colNames` list had a length less than `ncol(mat)`.
* Fixed float placement of landscaped tables made by `latexTable()`.
&nbsp;  
&nbsp;  
&nbsp;  



# Version 2.0.0 (2020-01-08)
* Added `regTable()`, `latexTable()`, and `latexTablePDF()`.
* Modified behavior of `lNA()` so that it no longer prints any output by 
  default. The new `lNAv()` behaves like the previous version of `lNA()`.
* Adopted camelCase for function names and arguments throughout the package.
  `modal_value()` is now `modalValue()`, and so on.
* Rewrote `moveToDF()` (formerly `move.to.df()`) from scratch.
* Rewrote the stack utilities: `pop()`, `push()`, `shift()`, and `unshift()`. 
* Removed `factorToDummyMatrix()`. See `caret::class2ind` or 
  `makedummies::makedummies` for a replacement.
* Removed ancient functions: `latable()`, `noNAmatrix()`, `split_fac()`.
* Converted to roxygen for documentation of most functions.
&nbsp;  
&nbsp;  
&nbsp;  



# Pre-release versions.
* Version 1.19.1. Updated contact email address in DESCRIPTION.
* Version 1.19.  Minor update to modal_value, which seems to make it faster.
* Version 1.18.  Minor update to `rescale()` so that it works with tibble columns.
* Version 1.17.  Added `factorToDummyMatrix()`.
* Version 1.16.  Minor update to `qw()`.
* Version 1.15.  Substantial revisions to `move.to.df()`.
* Version 1.14.  Added `%IN%`, a binary match operator that can return NA.
* Version 1.13.  Added `qw()`, a Perl-like string-quoting function.
* Version 1.12.  Changed default argument to `rescale()`.
* Version 1.11.  Improved `lsos()` documentation.
* Version 1.10.  Added `lsos()` to the package.
* Version 1.09.  Adjusted `merge_fac()` so that it can work with variables in 
               environments other than the global environment. 
* Version 1.08.  Added stack utilities (`push(), pop(), shift()`, and `unshift()`) 
               that are based on Jeffrey A. Ryan's code at
               [http://www.lemnica.com/esotericR/Introducing-Closures/].
* Version 1.07.  Fixed bug in `latable()` handling of `ivreg()` output.  (The AER
               package contains an `ivreg()` function for instrumental-variables
               regression.)
* Version 1.06.  Added `rows.to.remove` argument to `latable()` so that 
               `latable()` can return "incomplete" regression tables.
* Version 1.05.  Added NAMESPACE to make the Windows binary installable in R 2.14.
* Version 1.04.  Added `reliability()` to compute Cronbach's alpha for batteries of survey items or other measurements.
* Version 1.03.  Added `modal_value()` and fixed a related bug in `move.to.df()`.
* Version 1.02.  Added `merge_fac()`.
