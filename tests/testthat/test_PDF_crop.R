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
  skip_on_travis()  # See issue #44 - these tests don't work on Travis
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


test_that("PDF_crop() can handle paths that contain spaces", {
  skip_on_travis()  # See issue #44 - these tests don't work on Travis
  skip_if_not(
    condition = requireNamespace("dplyr", quietly = TRUE),
    message = "dplyr package not available, so skipping the 'PDF_crop() can handle paths that contain spaces' test"
  )
   
  check_pdfcrop_presence()

  # input file is in a directory that has spaces in its name
  tmpDir <- dplyr::case_when(
    !is.null(Sys.getenv("R_SESSION_TMPDIR")) ~ Sys.getenv("R_SESSION_TMPDIR"),
    !is.null(Sys.getenv("TEMP"))             ~ Sys.getenv("TEMP"),
    !is.null(Sys.getenv("TMP"))              ~ Sys.getenv("TMP"),
    TRUE ~ "/tmp"
  )
  tmpDir <- paste0(tmpDir, "/test of PDF_crop")
  tmpDir <- normalizePath(tmpDir, mustWork = FALSE)
  if (!dir.exists(tmpDir)) dir.create(tmpDir)  
  tmpFile <-  tempfile(fileext = ".pdf", tmpdir = tmpDir)
  pdf(tmpFile)
    plot(1:10, 1:10)
  dev.off()
  PDF_crop(tmpFile, deleteOriginal = FALSE)
  expect_true(file.exists(sub('.pdf', '_crop.pdf', tmpFile)))
  
  # output to a different directory, which also has spaces in its name
  newOutputDir <- paste(dirname(tmpFile), 2)
  if (!dir.exists(newOutputDir)) dir.create(newOutputDir)  
  PDF_crop(tmpFile, newOutputDir)
  expect_true(file.exists(sub('.pdf', '_crop.pdf', paste0(newOutputDir, "/", basename(tmpFile)))))
})