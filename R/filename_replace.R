#' filename_replace
#'
#' Replace a string in a filename with another string.
#'
#' @param filepath Path to files. Defaults to current directory.
#' @param from String to find.
#' @param to String to replace it with.
#' @param verbose Logical. Should it print descriptions of any changes?
#'
#' @export
#' @examples
#' folder <- "FCS_trimmed"
#' filename_replace(filepath = folder, from="iptg", to="IPTG")
#' filename_replace(filepath = folder, from="100ngmlatc", to="100")
#' filename_replace(filepath = folder, from="_something_", to="_")

filename_replace <- function(filepath = ".", from, to, verbose = TRUE){

  files <- list.files(path = filepath, pattern = utils::glob2rx("*.fcs"),
                      full.names = T, recursive = F, include.dirs = F)

  for (file in files) {
    if ( verbose ) { print(paste("Old filename: ", basename(file), sep = "")) }
    newfilename <- sub(pattern=from, replacement=to, file, ignore.case = TRUE)
    if ( verbose ) { print(paste("New filename: ", basename(newfilename), sep = "")) }
    file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(newfilename)))

    # Spacer
    if ( verbose ) { print("...") }
  }

}
