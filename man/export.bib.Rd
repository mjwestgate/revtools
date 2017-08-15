\name{export.bib}
\alias{export.bib}
\title{Export imported bibliographic data as .bib}
\description{Basic function to export bibliographic information for use in other programs. Work in progress. Very little error checking or advanced formatting in this version}
\usage{
export.bib(x, filename, format)
}
\arguments{
  \item{x}{An object of class 'bibviewr', such as imported using import.bib}
  \item{filename}{Name of the exported file. Should ideally match 'format', but this is not enforced}
  \item{format}{Format of the exported file. Should be either "ris" (default) or "bib"}
}
\value{
exports results as a txt file of the correct format for .ris or .bib files.
}