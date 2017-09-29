\name{summary.bibliography}
\alias{summary.bibliography}
\title{Summary method for class 'bibliography'}
\description{Display basic information on object of class 'bibliography'.}
\usage{
\method{summary}{bibliography}(object, ...)
}
\arguments{
  \item{object}{An object of class 'bibliography'}
  \item{...}{Any further information}
}
\value{
Prints useful information to the console.
}
\note{
Class 'bibliography' is the default format into which data are imported in revtools (via 'read_bibliography'). It is basically a nested list, in which each entry is a reference, and each reference is a(nother) list with separate entries for each piece of biliographic information. Use function 'make_reference_dataframe' to convert to a data.frame.
}