# Define scMaSigPro functions for read-write operations

#' @title Write scMaSigPro object to file.
#'
#' @description Write scMaSigPro object to various file formats.
#'
#' @param object scMaSigPro object.
#' @param filename filename. Please donot provide extensions.
#' @param filepath File path.
#' @param format File format. Default is 'rds'.
#' @param overwrite Overwrite existing file. Default is FALSE.
#' @param verbose Print verbose output. Default is FALSE.
#'
#' @return Write a scMaSigPro object to file.
#'
#' @author Priyansh Srivastava \email{spriyansh29@@gmail.com}.
#'
#' @export
#'

# write function declaration
scmp_write <- function(scmpObj,
                       filename,
                       filepath = NULL,
                       format = "rds",
                       overwrite = FALSE) {
  # library(assertthat)
  # library(scMaSigPro)
  # library(rhdf5)
  # library(parallel)
  # library(parallelly)
  # scmpObj <- scmp.ob
  # format <- "h5"
  # filename <- "scmpObj_file"
  # path <- "test_dev/"
  # overwrite <- TRUE

  # Check if supplied object is correct
  assert_that(is(scmpObj, "ScMaSigPro"),
    msg = "Please provide object of class 'ScMaSigPro'."
  )

  # Check expected format
  format <- tolower(format)
  assert_that(all(format %in% c("rds", "h5")),
    msg = "Please provide a valid file format. Available formats are 'RDS' and 'H5'."
  )

  # Check if the filepath is writable
  assert_that(file.exists(path),
    msg = "Please provide a valid file path."
  )

  # Remove trailing "/"
  path <- gsub("/$", "", path)

  # Create file path
  file_path <- file.path(path, paste0(filename, ".", format))

  # Check if file already exists
  if (file.exists(file_path) & !overwrite) {
    stop("File already exists. Please set overwrite = TRUE.")
  }


  source("R/scmp_write_h5.R")
  scmp_write_h5(scmpObj, file_path, overwrite = overwrite, verbose = verbose)


  # Check for the format
  if (format == "rds") {
    saveRDS(scmpObj, file_path)
  } else if (format == "h5") {
    scmp_write_h5(scmpObj, file_path, overwrite = overwrite)
  } else {
    stop("Please provide a valid file format.")
  }
}
