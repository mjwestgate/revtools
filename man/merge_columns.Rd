\name{merge_columns}
\alias{merge_columns}
\title{rbind two or more data frames with different columns}
\description{Take two data.frames with arbitraily different numbers and names of columns, and rbind them together.}
\usage{
merge_columns(x, y)
}
\arguments{
  \item{x}{A data.frame, or a list containing multiple data.frames.}
  \item{y}{An optional second data.frame. Only used if x is not a list.}
}
\value{
An object of class 'data.frame', containing data from all input data frames. That is, all unique columns are preserved, while rows that are missing data for a given column are assigned NA.
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
y <- x[, 1:3]
z <- merge_columns(x, y)
# or equivalently
z <- merge_columns(list(x, y))
}