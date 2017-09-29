\name{print.bibliography}
\alias{print.bibliography}
\title{Methods for class 'bibliography'}
\description{Print information contained within object of class 'bibliography'.}
\usage{
print(x, ...)
}
\arguments{
  \item{x}{An object of class 'bibliography'}
  \item{...}{Any further information}
}
\value{
Prints useful information to the console.
}
\note{
Class 'bibliography' is the default format into which data are imported in revtools (via 'read_bibliography'). It is basically a nested list, in which each entry is a reference, and each reference is a(nother) list with separate entries for each piece of biliographic information. Use function 'make_reference_dataframe' to convert to a data.frame.
}