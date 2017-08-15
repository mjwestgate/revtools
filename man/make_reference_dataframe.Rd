\name{make_reference_dataframe}
\alias{make_reference_dataframe}
\title{Convert bibliographic information to a data.frame}
\description{This function take the list-based format returned by import.bib (class "bibdata") and converts it to a simple data.frame. }
\usage{
make_reference_dataframe(x)
}
\arguments{
  \item{x}{an object of class 'bibdata', as returned by import.bib}
}
\value{
a data.frame containing basic information for each reference (row)
}
\note{
This is a simple wrapper function that does not attempt to be comprehensive, meaning that some data will be lost from most formats. This is not the case with import.bib which preserves all data in the source file.
}