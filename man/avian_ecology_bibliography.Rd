\name{avian_ecology_bibliography}
\alias{avian_ecology_bibliography}
\title{Bibliographic data from 20 papers on avian ecology}
\description{This dataset lists basic information (title, authors, keywords etc.) for 20 scientific articles on avian ecology, stored in .ris format.}
\format{A list of length 20, containing lists of named attributes for each article.}
\source{Originally downloaded from Scopus.}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
summary(x)
}