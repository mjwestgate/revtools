\name{make_reference_dataframe}
\alias{make_reference_dataframe}
\title{Convert bibliographic information to a data.frame}
\description{This function take an object of class 'bibliography' and converts it to a data.frame.}
\usage{
make_reference_dataframe(x, abstracts)
}
\arguments{
  \item{x}{an object of class 'bibliography', as returned by read_bibliography}
  \item{abstracts}{Logical - should article abstracts be included? Defaults to TRUE.}
}
\value{
a data.frame containing basic information for each reference (row)
}
\note{
This is a simple wrapper function that does not attempt to be comprehensive, meaning that some data will be lost from most formats. This is not the case with read_bibliography which preserves all data in the source file.
}