\name{add_line_breaks}
\alias{add_line_breaks}
\title{Add line breaks to one or more strings}
\description{This function takes a vector of strings and adds line breaks every \code{n} characters. Primarily built to be called internally by \code{format_citation}, this function has been made available as it can be useful in other contexts.}
\usage{
add_line_breaks(x, n = 50, html = FALSE)
}
\arguments{
  \item{x}{Either a string or a vector; if the vector is not of class \code{character} if will be coerced to one using \code{as.character}.}
  \item{n}{Numeric: The number of characters that should separate consecutive line breaks.}
  \item{html}{Logical: Should the function add HTML line breaks (\code{<br>})? Defaults to FALSE, in which case the newline sequence is used instead (escape-n).}
}
\value{
Returns the input vector unaltered except for the addition of line breaks.
}
\details{
Line breaks are only added between words, so the value of \code{n} is acutally a threshold value rather than being matched exactly.
}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
add_line_breaks(x$title)
}
