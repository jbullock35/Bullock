# test_PDF_crop.R
# Created on 2020-02-15
# Created by John Bullock


context("Check that PDF_crop() can crop PDF files.")


check_pdfcrop_presence <- function () {
  if (!nzchar(Sys.which("pdfcrop"))) {
    skip("pdfcrop doesn't seem to be on your path.")
  }
}



test_that("PDF_crop() can handle different kinds of paths", {
  check_pdfcrop_presence()

  tmpFile <- tempfile(fileext = ".pdf")
  tmpDir  <- paste0(dirname(tmpFile), "/test_PDF_crop")
  if (!dir.exists(tmpDir)) dir.create(tmpDir)  
  pdf(tmpFile)
    plot(1:10, 1:10)
  dev.off()

  # PROVIDE FULL PATH IN input ARGUMENT
  expect_error(PDF_crop(tmpFile, deleteOriginal = FALSE), regexp = NA)
  
  # PROVIDE ONLY FILENAME IN input ARGUMENT
  setwd(dirname(tmpFile))
  expect_error(PDF_crop(basename(tmpFile), deleteOriginal = FALSE), regexp = NA)
  
  # OUTPUT CROPPED FILE TO A DIFFERENT DIRECTORY
  expect_error(
    PDF_crop(basename(tmpFile), dirOutput = tmpDir, deleteOriginal = FALSE), 
    regexp = NA)
  
  expect_error(
    PDF_crop(basename(tmpFile), dirOutput = "test_PDF_crop", deleteOriginal = FALSE), 
    regexp = NA)
  expect_error(
    PDF_crop(basename(tmpFile), dirOutput = "./test_PDF_crop", deleteOriginal = FALSE), 
    regexp = NA)
  expect_error(
    PDF_crop(basename(tmpFile), dirOutput = "test_PDF_crop/", deleteOriginal = FALSE), 
    regexp = NA)
  if (Sys.info()['sysname']=='Windows') {
    expect_error(
      PDF_crop(basename(tmpFile), dirOutput = "test_PDF_crop\\", deleteOriginal = FALSE), 
      regexp = NA)
  }
})
