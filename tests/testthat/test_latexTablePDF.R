# test_latexTablePDF.R
# Created on 2019-12-31
# Created by John Bullock


context("Check that latexTablePDF() can write files.")

lT1 <- latexTable(matrix(1:16, nrow=4))
filenameStem <- tempfile(tmpdir = '')
filenameStem <- gsub('^(\\\\*)', '', filename)  # eliminate initial slashes
latexTablePDF(lT1, outputFilenameStem = filenameStem, openPDFOnExit = FALSE)


test_that("latexTablePDF() can write a PDF file to disk", {
  expect_true(
    file.exists(paste0(filenameStem, '.pdf'))
  )
})


# Clean up
if (file.exists(paste0(filenameStem, '.pdf'))) 
  unlink(paste0(filenameStem, '.pdf'))
