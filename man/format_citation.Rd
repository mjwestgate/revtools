\name{format_citation}
\alias{format_citation}
\alias{format_citation.bibliography}
\alias{format_citation.list}
\alias{format_citation.data.frame}
\title{Format a citation}
\description{takes an object of class \code{data.frame} or \code{bibliography} and returns a formatted citation.}
\usage{
format_citation(data, details = TRUE, abstract = FALSE,
  add_html = FALSE, line_breaks = FALSE, \dots)
}
\arguments{
  \item{data}{An object of class \code{data.frame} or \code{bibliography}.}
  \item{details}{Logical: Should identifying information such as author names & journal titles be displayed? Defaults to TRUE.}
  \item{abstract}{Logical: Should the abstract be shown (if available)? Defaults to FALSE.}
  \item{add_html}{Logical: Should the journal title be italicized using html codes? Defaults to FALSE.}
  \item{line_breaks}{Either logical, stating whether line breaks should be added, or numeric stating how many characters should separate consecutive line breaks. Defaults to FALSE.}
  \item{\dots}{any other arguments.}
}
\value{
a string of length == length(x), containing formatted citations.
}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location, return_df = FALSE)
format_citation(x[[1]])
format_citation(as.data.frame(x)[1, ]) # same result
}
