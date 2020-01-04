#' filename_replicatenumber
#'
#' Append index to end of filename (before the filetype extension such as .fcs)
#' to show its replicate number, eg. "conditionX_1.fcs", "conditionX_2.fcs".
#' Assumes triplicate data that is ordered: replicate number appended depends
#' entirely on each file's position in files (1st = _1, 2nd = _2, 3rd = _3, 4th
#' = _1, and so on).
#' @param files List of files. Defaults to files.
#' @export
#' @examples
#' files <- list.files(path = ".")
#' filename_replicatenumber(files=files)

filename_replicatenumber <- function(files = files){

  for (file in files) {

    print(paste("Using this file: ", basename(file), sep=""))

    # Find index
    # Which nth element is this?
    index <- which(files == file)[[1]]
    print(paste("Index of which is: ", index, sep=""))

    # rep1
    if (index%%3 == 1) {
      appendix <- "_1.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      print(paste("New filename: ", newfilename, sep=""))

      file.rename(from = basename(file), to = basename(newfilename))
    }

    # rep2
    if (index%%3 == 2) {
      appendix <- "_2.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      print(paste("New filename: ", newfilename, sep=""))

      file.rename(from = basename(file), to = basename(newfilename))
    }

    # rep3
    if (index%%3 == 0) {
      appendix <- "_3.fcs"
      trunc_name <- (unlist(strsplit(basename(file), ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      newfilename <- paste(trunc_name, appendix, sep="")
      print(paste("New filename: ", newfilename, sep=""))

      file.rename(from = basename(file), to = basename(newfilename))
    }

    print("...")

  }

}
