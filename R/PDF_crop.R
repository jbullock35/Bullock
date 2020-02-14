#' Crop white margins of a PDF file.
#' 
#' \code{PDF_crop()} removes ("crops") the white margins around a PDF. It 
#' requires that pdfcrop.exe be installed and in your path. To get pdfcrop.exe,
#' install MikTeX or Tex Live, or just use \code{tinytex::tlmgr_install('pdfcrop')}.

#' @param input A string. Path and name of file to be cropped. If \code{input}
#' does not end with ".pdf", ".pdf" will be appended to it. This is the only 
#' required argument.
#' 
#' @param dirOutput A string. Path of directory into which cropped file should 
#' be placed.
#' 
#' @param outputExtension A string. The new file will have the same name as
#' the old file, except that \code{outputExtension} will be inserted just 
#' before the ".pdf" extension.
#' 
#' @param deleteOriginal Logical, defaulting to \code{TRUE}. Should the
#' original file be deleted?
#' 
#' @param openCropped Logical, defaulting to \code{FALSE}. After the new file
#' is produced, should it be opened in your default PDF viewer? Ignored on 
#' non-Windows, non-Macintosh systems.
#' 
#' @param verbose Logical, defaulting to \code{FALSE}. If it is \code{TRUE},
#' two lines of information from \code{pdfcrop.exe} will be printed. They will
#' show your version of \code{pdfcrop.exe}, the number of pages in the file
#' that it produced, and the path and filename of the file that it produced.
#' 
#' @seealso \code{\link[knitr:plot_crop]{knitr::plot_crop()}}, which is less
#' flexible in its handling of PDF files but can handle other file formats.
#' 
#' @examples
#' tmpFile <- tempfile(fileext = ".pdf")
#' pdf(tmpFile)
#'   plot(1:10, 1:10)
#' dev.off()
#' PDF_crop(tmpFile)


#' @export
PDF_crop <- function (
  
  input, 
  dirOutput = NULL, 
  outputExtension = '_crop',
  deleteOriginal = TRUE,
  openCropped = FALSE,
  verbose = FALSE) {
  
  
  if (!nzchar(Sys.which("pdfcrop"))) stop("'pdfcrop' could not be found")
  
  
  # NORMALIZE INPUT
  if (!grepl('\\.pdf$', input)) input <- paste0(input, '.pdf')
  input <- normalizePath(input, mustWork = TRUE)
  
  
  # GET OUTPUT DIRECTORY
  if (is.null(dirOutput)) dirOutput <- dirname(input)
  dirOutput <- sub('([\\d\\w])$', '\\1/', dirOutput, perl = TRUE)  # add trailing slash
  dirOutput <- normalizePath(dirOutput, mustWork = TRUE)
  
  
  # CROP THE FILE
  # On Windows, pdfcrop.exe can't be called from system() or system2(). A 
  # shell must be invoked, so I use shell(). (I don't know why it is this way.)
  #   On Linux, and perhaps on Mac, there is no shell() command. So we use 
  # system2() to call pdfcrop.exe from those machines.  [2020 02 16]
  outputBaseName <- sub('\\.pdf$', paste0(outputExtension, '.pdf'), basename(input))
  if (Sys.info()['sysname'] == 'Windows') {
    shell(
      paste(
        'pdfcrop', 
        input, 
        paste0(dirOutput, outputBaseName)),
      ignore.stdout = !verbose)
  }
  else {
    system2(
      'pdfcrop',
      c(input, paste0(dirOutput, outputBaseName)),
      stdout = if (verbose) "" else FALSE)
  }
  
  if (deleteOriginal) file.remove(input)
  
  
  # OPEN FILE
  # Open in default viewer for Windows or Mac
  if (openCropped && Sys.info()['sysname']=='Windows') 
    shell.exec(paste0(dirOutput, outputBaseName))
  else if (openCropped && Sys.info()['sysname']=='Darwin')  
    system2('open', args = paste0(dirOutput, outputBaseName), wait = FALSE)
} 

