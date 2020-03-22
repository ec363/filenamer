#' filename_B02
#'
#' Replaces all multiwell plate 'well' labels in the form 'B2' to the
#' double-digit form 'B02'. This enables correct ordering of filenames which
#' include numbers over 10. (Though file systems won't always have a problem,
#' files in a folder are ordered alphabetically as strings, so B2 is placed
#' after B10: using B02 solves this problem). Enables correct use of other
#' functions, such as filename_extension and filename_replicatenumber.
#'
#' @param filepath Path to files. Defaults to current directory.
#' @param verbose Logical. Should it print descriptions of any changes?
#'
#' @export
#' @examples
#' folder <- "FCS_trimmed"
#' filename_B02(filepath = folder)

filename_B02 <- function(filepath = ".", verbose = TRUE){

  files <- list.files(path = filepath, pattern = utils::glob2rx("*.fcs"),
                      full.names = T, recursive = F, include.dirs = F)

  for (file in files) {
    if ( grepl("_([A-H]){1}([[:digit:]]){1}[[:punct:]]", file, ignore.case = FALSE) ) {

      if ( verbose ) { print(paste("Old filename: ", basename(file), sep="")) }
      if ( verbose ) { print("Does match pattern.") }

      matches <- regmatches(file, regexpr("_([A-H]){1}([[:digit:]]){1}[[:punct:]]", file))
      if ( verbose ) { print(paste("matches = ", matches, sep="")) }
      matches_atomic <- unlist(strsplit(matches, split="")) # not defining split results in single characters
      if ( verbose ) { print("split matches = ") }
      if ( verbose ) { print(matches_atomic) }
      replacement <- paste(matches_atomic[1], matches_atomic[2], "0", matches_atomic[3], matches_atomic[4], sep="")
      if ( verbose ) { print(paste("replacement = ", replacement, sep="")) }

      # replacement for FILENAME
      newfilename <- sub(pattern="_([A-H]){1}([[:digit:]]){1}[[:punct:]]", replacement=replacement, x=basename(file))
      if ( verbose ) { print(paste("New filename: ", basename(newfilename), sep = "")) }
      file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(newfilename)))

    } else {
      if ( verbose ) { print(paste("Old filename: ", basename(file), sep="")) }
      if ( verbose ) { print("Does NOT match pattern.") }
    }

    # Spacer
    if ( verbose ) { print("...") }
  }

}
