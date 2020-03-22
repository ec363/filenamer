#' filename_replicatenumber
#'
#' Append index to end of filename (before the filetype extension such as .fcs)
#' to show its replicate number, eg. "conditionX_1.fcs", "conditionX_2.fcs".
#' Assumes triplicate data that is ordered: replicate number appended depends
#' entirely on each file's position in files (1st = _1, 2nd = _2, 3rd = _3, 4th
#' = _1, and so on).
#'
#' @param filepath Path to files. Defaults to current directory.
#' @param verbose Logical. Should it print descriptions of any changes?
#'
#' @export
#' @examples
#' folder <- "FCS_trimmed"
#' filename_replicatenumber(filepath = folder)

filename_replicatenumber <- function(filepath = ".", verbose = TRUE){

  files <- list.files(path = filepath, pattern = utils::glob2rx("*.fcs"),
                      full.names = T, recursive = F, include.dirs = F)

  for (file in files) {

    if (verbose) { print(paste("Using this file: ", basename(file), sep="")) }

    # Find index
    # Which nth element is this?
    index <- which(files == file)[[1]]
    if (verbose) { print(paste("Index of which is: ", index, sep="")) }

    # rep1
    if (index%%3 == 1) {
      appendix <- "_1.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      if (verbose) { print(paste("New filename: ", newfilename, sep="")) }

      file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(newfilename)))
    }

    # rep2
    if (index%%3 == 2) {
      appendix <- "_2.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      if (verbose) { print(paste("New filename: ", newfilename, sep="")) }

      file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(newfilename)))
    }

    # rep3
    if (index%%3 == 0) {
      appendix <- "_3.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      if (verbose) { print(paste("New filename: ", newfilename, sep="")) }

      file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(newfilename)))
    }

    if (verbose) { print("...") }

  }

}
