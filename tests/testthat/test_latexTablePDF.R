# test_latexTablePDF.R
# Created on 2019-12-31
# Created by John Bullock


context("Check that latexTablePDF() can write files.")

# Check default behavior
lT1 <- latexTable(matrix(1:16, nrow=4))
lT1_filenameStem <- tempfile(tmpdir = '')  
lT1_filenameStem <- gsub('^([/|\\\\]*)', '', lT1_filenameStem)  # eliminate initial slashes
latexTablePDF(
  lT1, 
  outputFilenameStem = lT1_filenameStem,
  openPDFOnExit      = FALSE)
latexTablePDF(
  lT1, 
  outputFilenameStem = lT1_filenameStem,
  writePDF           = FALSE,
  writeTex           = TRUE)


# Check that default behavior works with a list of tables, rather than 
# just one table.
lT2 <- latexTable(matrix(1:16, nrow=4), commandName = 'secondTable')
lT2_filenameStem <- tempfile(tmpdir = '')
lT2_filenameStem <- gsub('^([/|\\\\]*)', '', lT2_filenameStem)  # eliminate initial slashes
latexTablePDF(
  list(lT1, lT2), 
  outputFilenameStem = lT2_filenameStem,
  openPDFOnExit      = FALSE)
latexTablePDF(
  list(lT1, lT2), 
  outputFilenameStem = lT2_filenameStem,
  writePDF           = FALSE,
  writeTex           = TRUE)



checkPdflatex <- function () {
  if (!nzchar(Sys.which("pdflatex"))) {
    skip("pdflatex doesn't seem to be on your path.")
  }
}
checkLatexPackages <- function () {
  kpsewhichExists <- nzchar(Sys.which("kpsewhich"))  # LaTeX package-checking tool
  if (!kpsewhichExists) {
    skip("Cannot locate kpsewhich.")
  }
  requiredPackageList  <- qw("array booktabs caption fancyhdr geometry numprint ragged2e")
  installedPackageList <- system2(
    command = "kpsewhich", 
    args    = paste0(requiredPackageList, ".sty", collapse=' '), 
    stdout  = TRUE)
  if (length(installedPackageList) < length(requiredPackageList)) {  # if a package is missing
    skip(
      stringr::str_wrap(
        paste(
          "Can't locate one of these LaTeX packages: ",
          paste(requiredPackageList, collapse = ', ')
        )
      )
    )
  }
}

 

test_that("latexTablePDF() can write PDF files to disk", {
  checkPdflatex()
  checkLatexPackages()
  expect_true(
    file.exists(paste0(lT1_filenameStem, '.pdf'))  # single table
  )
  expect_true(
    file.exists(paste0(lT2_filenameStem, '.pdf'))  # list of tables
  )  
  expect_true(
    exists(".pdflatex_found")
  )
  expect_true(
    exists(".latex_packages_found"))
})



test_that("latexTablePDF() can write .tex files to disk", {
  expect_true(
    file.exists(paste0(lT1_filenameStem, '.tex'))  # single table
  )
  expect_true(
    file.exists(paste0(lT2_filenameStem, '.tex'))  # list of tables
  )  
})



# Clean up
if (file.exists(paste0(lT1_filenameStem, '.pdf'))) 
  unlink(paste0(lT1_filenameStem, '.pdf'))
if (file.exists(paste0(lT1_filenameStem, '.tex'))) 
  unlink(paste0(lT1_filenameStem, '.tex'))
if (file.exists(paste0(lT2_filenameStem, '.pdf'))) 
  unlink(paste0(lT2_filenameStem, '.pdf'))
if (file.exists(paste0(lT2_filenameStem, '.tex'))) 
  unlink(paste0(lT2_filenameStem, '.tex'))
