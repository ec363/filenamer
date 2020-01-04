#' filename_replace
#'
#' Replace a string in a filename with another string.
#' @param files List of files. Defaults to files.
#' @param from String to find.
#' @param to String to replace it with.
#' @export
#' @examples
#' files <- list.files(path = ".")
#' filename_replace(files=files, from="iptg", to="IPTG")
#' filename_replace(files=files, from="_something_", to="_")

filename_replace <- function(files = files, from, to){

  for (file in files) {
    print(paste("Old filename: ", basename(file), sep = ""))
    newfilename <- sub(pattern=from, replacement=to, file, ignore.case = TRUE)
    print(paste("New filename: ", basename(newfilename), sep = ""))
    file.rename(from = basename(file), to = basename(newfilename))
  }

}
