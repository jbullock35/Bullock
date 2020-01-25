# Version 2.1.0.9000
* Removed class restrictions on objects passed to `regTable()`. `regTable()`
  now works with almost every kind of regression object.
* Added global option `Bullock.print.regTable.dp` on package load so that 
  users can change default number of digits that are displayed when they
  `print` a regTable object.
* Added `envir` argument to `lsos()` and `.ls.objects()` (#6).
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
