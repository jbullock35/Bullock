# Version 2.0
* Added `regTable()`.


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