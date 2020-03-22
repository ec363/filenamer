#' filename_extension
#'
#' Iterates through the provided list of files (alphabetically). If a filename
#' has part length length_with_extension (as defined by strsplit using "_"), it
#' is assigned as having an extension/appendix. This function takes the
#' identified extension/appendix part of the filename and appends it to
#' following filenames identified as missing an extension/appendix. Designed for
#' triplicate files in which first file of each group is comprehensively
#' labelled and the others are not, but should work with any number of
#' replicates, provided the filepath contains them in the correct order.
#'
#' @param filepath Path to files. Defaults to current directory.
#' @param length_with_extension Number of parts to the basename of a file WITH an extension after splitting by "_". Defaults to a.
#' @param length_noextension Number of parts to the basename of a file WITHOUT an extension after splitting by "_". Defaults to b.
#' @param verbose Logical. Should it print descriptions of any changes?
#'
#' @export
#' @examples
#' folder <- "FCS_trimmed"
#' a <- length( unlist(strsplit(basename(list.files(folder)[1]), "_")) )
#' b <- length( unlist(strsplit(basename(list.files(folder)[2]), "_")) )
#' filename_extension(filepath = folder, length_with_extension = a, length_noextension = b)

filename_extension <- function(filepath = ".", length_with_extension = a, length_noextension = b, verbose = TRUE){

  # Length of appendix
  appendix_n <- length_with_extension-length_noextension
  if(appendix_n < 0){
    # If file2 shorter than file1 - error and stop.
    if (verbose) { print(paste("Error - looks like second filename is longer than the first. Stopping...", sep="")) }
    return()
  }
  if(appendix_n==0){
    # If file1 and file2 are the same length - stop.
    if (verbose) { print(paste("No extension detected. Stopping...", sep="")) }
    return()
  }

  files <- list.files(path = filepath, pattern = utils::glob2rx("*.fcs"),
                      full.names = T, recursive = F, include.dirs = F)

  for (file in files) {

    # define files as those with an appendix already, and those without

    if ( length( unlist(strsplit(basename(file), "_")) ) == length_with_extension ) {

      # APPENDIX FILES
      # If file has an appendix, extract appendix

      if (verbose) { print(paste("Using this file: ", basename(file), sep="")) }

      # Now take appendix
      split.file.with.extension <- unlist(strsplit(basename(file), "_"))
      split.file.with.extension

      # # New version # moved up
      # appendix_n <- length_with_extension-length_noextension

      if(appendix_n==1){
        # If appendix has only one part:
        # appendix = the last element of the split filename
        appendix <- paste("_", split.file.with.extension[length_with_extension], sep="")
        if (verbose) { print(paste("Appendix is ", appendix, sep="")) }
      }
      if(appendix_n > 1){
        # If appendix has multiple parts, it gets complicated:

        # start of appendix:
        appendix.start.position <- length_with_extension + 1 - appendix_n # +1 needed to get position
        # first part of appendix:
        appendix <- paste("_", split.file.with.extension[appendix.start.position], sep="")
        for(part in 2:appendix_n){
          next_part <- split.file.with.extension[appendix.start.position-1+part] # -1 needed to get second position..
          appendix <- paste(appendix, "_", next_part, sep="")
        }

        if (verbose) { print(paste("Appendix is ", appendix, sep="")) }
      }

      # Filename remains unchanged
      if (verbose) { print("...") }

    } else {
      # If file does not have an appendix, should carry over value of appendix from previous file
      # Apply appendix to these files

      if (verbose) { print(paste("Applying appendix to this file: ", basename(file), sep="")) }

      replicate <- basename(file)
      trunc_name <- (unlist(strsplit(replicate, ".fcs"))) # splitting by .fcs just cuts it off, don't need [1]
      replicate.with.appendix <- paste(trunc_name, appendix, sep="")
      if (verbose) { print(paste("New filename: ", replicate.with.appendix, sep="")) }

      file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(replicate.with.appendix)))

      if (verbose) { print("...") }

    }
  }

}
