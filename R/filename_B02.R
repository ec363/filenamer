#' filename_B02
#'
#' Replaces all multiwell plate 'well' labels in the form 'B2' to the
#' double-digit form 'B02'. This enables correct ordering of filenames which
#' include numbers over 10. (Though file systems won't always have a problem,
#' files in a folder are ordered alphabetically as strings, so B2 is placed
#' after B10: using B02 solves this problem). Enables correct use of other
#' functions, such as filename_extension and filename_replicatenumber.
#' @param files List of files. Defaults to files.
#' @export
#' @examples
#' files <- list.files(path = ".")
#' filename_B02(files=files)

filename_B02 <- function(files = files){

for (file in files) {
  if ( grepl("_([A-H]){1}([[:digit:]]){1}[[:punct:]]", file, ignore.case = FALSE) ) {

    print(paste("Old filename: ", basename(file), sep=""))
    print("Does match pattern.")

    matches <- regmatches(file, regexpr("_([A-H]){1}([[:digit:]]){1}[[:punct:]]", file))
    print(paste("matches = ", matches, sep=""))
    matches_atomic <- unlist(strsplit(matches, split="")) # not defining split results in single characters
    print("split matches = ")
    print(matches_atomic)
    replacement <- paste(matches_atomic[1], matches_atomic[2], "0", matches_atomic[3], matches_atomic[4], sep="")
    print(paste("replacement = ", replacement, sep=""))

    # replacement for FILENAME
    newfilename <- sub(pattern="_([A-H]){1}([[:digit:]]){1}[[:punct:]]", replacement=replacement, x=basename(file))
    print(paste("New filename: ", basename(newfilename), sep = ""))
    file.rename(from = basename(file), to = basename(newfilename))
  } else {
    print(paste("Old filename: ", basename(file), sep=""))
    print("Does NOT match pattern.")
  }
}

}
