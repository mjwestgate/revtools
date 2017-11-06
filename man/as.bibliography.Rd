\name{as.bibliography}
\alias{as.bibliography}
\title{Convert data.frame to bibliographic information}
\description{This function take an object of class 'data.frame' and converts it to a object of class 'bibliography'.}
\usage{
as.bibliography(x, ...)
}
\arguments{
  \item{x}{an object of class 'bibliography', as returned by read_bibliography}
  \item{...}{Any further arguments (none defined at present).}
}
\value{
an object of class 'bibliography'.
}
\note{
Inverse to as.data.frame, which (unsurprisingly) converts a bibliography to a data.frame.
}