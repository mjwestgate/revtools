\name{summary.review_info)}
\alias{summary.review_info}
\title{Methods for class 'review_info'}
\description{Tools to display useful information on class 'review_info'.}
\usage{
summary(object, ...)
}
\arguments{
  \item{object}{An object of class 'review_info'}
  \item{...}{Any further information}
}
\value{
Prints useful information to the workspace.
}
\note{
Class 'bibliography' is a format for exporting large quantities of data during reviews. It is typically stored within a .rds file in the working directory. When re-imported to R using readRDS, this file will contain an object of class 'review_info'.
}