\name{write_bibliography}
\alias{write_bibliography}
\title{Export imported bibliographic data as .bib or .ris formats}
\description{Basic function to export bibliographic information for use in other programs. Work in progress. Very little error checking or advanced formatting in this version}
\usage{
write_bibliography(x, filename, format = "ris")
}
\arguments{
  \item{x}{An object of class 'bibliography', such as imported using read_bibliography}
  \item{filename}{Name of the exported file. Should ideally match 'format', but this is not enforced}
  \item{format}{Format of the exported file. Should be either "ris" (default) or "bib"}
}
\value{
exports results as a .ris or .bib file.
}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location, return_df = FALSE)

# export a subset of entries as a new file
write_bibliography(x[1:5],
  filename = paste0(tempdir(), "/x_out.ris"),
  format = "ris")
}