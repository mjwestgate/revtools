# soft-deprecated functions

#' Import bibliographic data
#'
#' Import standard formats from academic search engines and referencing
#' software. \strong{This function has been deprecated and will be removed
#' from future versions of \code{revtools}. Please use \code{synthesisr::read_refs()}
#' instead}.
#'
#'
#' @param filename A vector or list containing paths to one or more
#' bibliographic files.
#' @param return_df Logical; should the object returned be a data.frame?
#' Defaults to TRUE. If FALSE, returns an object of class \code{bibliography}.
#' @param tag_naming Specify the type of ris tags used.
#' @param verbose Logical; should progress information be printed?
#' @return Returns an object of class \code{data.frame} if \code{return_df} is
#' \code{TRUE}; otherwise an object of class \code{bibliography}.
#' @examples
#'
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location)
#' class(x) # = data.frame
#' x <- read_bibliography(file_location, return_df = FALSE)
#' class(x) # = bibliography
#' summary(x)
#'
#' @export read_bibliography
read_bibliography <- function(
  filename,
  return_df = TRUE,
  tag_naming = "best_guess",
  verbose = FALSE
){
  read_refs(
    filename = filename,
    return_df = return_df,
    tag_naming = tag_naming,
    verbose = verbose
  )
}


#' Export imported bibliographic data as .bib or .ris formats
#'
#' Basic function to export bibliographic information for use in other
#' programs. \strong{This function has been deprecated and will be removed
#' from future versions of \code{revtools}. Please use \code{synthesisr::write_refs()}
#' instead}.
#'
#'
#' @param x A \code{data.frame} or object of class \code{bibliography}
#' @param filename Name of the exported file. Should ideally match 'format',
#' but this is not enforced
#' @param format Format of the exported file. Should be either "ris" (default)
#' or "bib"
#' @param tag_naming Type of tags to use in writing the output file
#' @return exports results as a .ris or .bib file.
#' @examples
#'
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location, return_df = FALSE)
#'
#' # export a subset of entries as a new file
#' write_bibliography(x[1:5],
#'   filename = paste0(tempdir(), "/x_out.ris"),
#'   format = "ris")
#'
#' @export write_bibliography
write_bibliography <- function(
  x,
  filename,
  format = "ris",
  tag_naming = "synthesisr"
){
  write_refs(
    x,
    file = filename,
    format = format,
    tag_naming = tag_naming
  )
} #  end function