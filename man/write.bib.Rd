\name{write_bib}
\alias{write.bib}
\title{Export imported bibliographic data as .bib}
\description{Basic function to export bibliographic information for use in other programs. Work in progress. Very little error checking or advanced formatting in this version}
\usage{
write.bib(x, filename)
}
\arguments{
  \item{x}{An object of class 'bibviewr', such as imported using import.bib}
  \item{filename}{Name of the exported file. Should probably end in .bib, but this is not enforced.}
}
\value{
exports results as a txt file of the correct format for .bib files.
}