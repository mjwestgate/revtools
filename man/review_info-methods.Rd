\name{review_info-methods)}
\alias{review_info-methods}
\alias{summary.review_info}
\title{Methods for class 'review_info'}
\description{Tools to display useful information on class \code{review_info}.}
\usage{
\method{summary}{review_info}(object, ...)
}
\arguments{
  \item{object}{An object of class 'review_info'}
  \item{...}{Any further information}
}
\value{
Prints useful information to the workspace.
}
\note{
Class \code{review_info} is a format for exporting large quantities of data during reviews. It is typically stored within a .rds file in the working directory. When re-imported to R using \code{readRDS}, this file will contain an object of class \code{review_info}.
}