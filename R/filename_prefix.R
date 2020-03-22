#' filename_prefix
#'
#' Removes prefixes of specified size in filenames.
#'
#' @param filepath Path to files. Defaults to current directory.
#' @param prefix_n Length of prefix, measured in parts after splitting by "_". Defaults to 3.
#' @param verbose Logical. Should it print descriptions of any changes?
#'
#' @export
#' @examples
#' folder <- "FCS_trimmed"
#' filename_prefix(filepath = folder, prefix_n = 4)

filename_prefix <- function(filepath = ".", prefix_n = 3, verbose = TRUE){

  # if no prefix exists, do nothing
  if (prefix_n == 0) {
    if ( verbose ) { print("No prefix to remove.") }
    return()
  }

  files <- list.files(path = filepath, pattern = utils::glob2rx("*.fcs"),
                      full.names = T, recursive = F, include.dirs = F)

  for ( file in files ) {

    if ( verbose ) { print(paste("Using this file: ", basename(file), sep="")) }

    ## 1. Split it up
    split.old.filename <- unlist(strsplit(basename(file), "_"))

    # 2. Identify prefix & stitching
    stitch.prefix <- split.old.filename[1] # first part
    for (part in 2:prefix_n) { # add parts[2:end]
      stitch.prefix <- paste(stitch.prefix, "_", split.old.filename[part], sep="")
    }
    stitch.prefix <- paste(stitch.prefix, "_", sep="")
    if ( verbose ) { print(paste("Prefix is: ", stitch.prefix, sep="")) }

    ## 3. Assemble new file name
    if ( verbose ) { print(paste("Removing prefix", sep="")) }

    ## 3A. Selects parts to keep
    lastpart.old <- length(split.old.filename) # count old parts to get # of last part

    a <- prefix_n+1
    b <- lastpart.old
    parts.new.filename <- split.old.filename[a:b]

    ## 3B. Assemble new name
    lastpart.new <- length(parts.new.filename) # count new parts for where to stop for loop
    stitch.new.filename <- parts.new.filename[1] # first part
    for (parts in parts.new.filename[2:lastpart.new]) { # add parts[2:end]
      stitch.new.filename <- paste(stitch.new.filename, "_", parts, sep="")
    }
    if ( verbose ) { print(paste("New filename: ", stitch.new.filename, sep="")) }

    ## 4. Rename
    file.rename(from = paste0(filepath, "/", basename(file)), to = paste0(filepath, "/", basename(stitch.new.filename)))

    # Spacer
    if ( verbose ) { print("...") }
  }

}
