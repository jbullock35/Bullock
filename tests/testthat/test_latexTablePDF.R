# test_latexTablePDF.R
# Created on 2019-12-31
# Created by John Bullock


context("Check that latexTablePDF() can write files.")

# Check default behavior
lT1 <- latexTable(matrix(1:16, nrow=4))
lT1_filenameStem <- tempfile(tmpdir = '') 
lT1_filenameStem <- gsub('^(\\\\*)', '', lT1_filenameStem)  # eliminate initial slashes
latexTablePDF(lT1, outputFilenameStem = lT1_filenameStem, openPDFOnExit = FALSE)


# Check that default behavior works with a list of tables, rather than 
# just one table.
lT2 <- latexTable(matrix(1:16, nrow=4), commandName = 'secondTable')
lT2_filenameStem <- tempfile(tmpdir = '')
lT2_filenameStem <- gsub('^(\\\\*)', '', lT2_filenameStem)  # eliminate initial slashes
latexTablePDF(
  list(lT1, lT2), 
  outputFilenameStem = lT2_filenameStem, 
  openPDFOnExit      = FALSE)



test_that("latexTablePDF() can write PDF files to disk", {
  expect_true(
    file.exists(paste0(lT1_filenameStem, '.pdf'))
  )
  expect_true(
    file.exists(paste0(lT2_filenameStem, '.pdf'))
  )  
})


# Clean up
if (file.exists(paste0(lT1_filenameStem, '.pdf'))) 
  unlink(paste0(lT1_filenameStem, '.pdf'))
if (file.exists(paste0(lT2_filenameStem, '.pdf'))) 
  unlink(paste0(lT2_filenameStem, '.pdf'))
