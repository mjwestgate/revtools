\name{revwords}
\alias{revwords}
\title{Load a set of stopwords}
\description{Generates a list of stopwords, consisting of all the terms given by \code{tm::stopwords}, plus some extra terms (mainly words that designate numbers).}
\usage{
revwords()
}
\value{
A vector of stopwords in English.
}
\note{
This is primarily an internal function, but may be useful in other contexts.
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)

# construct a document-term matrix
x_dtm <- make_dtm(x$title,
  stop_words = revwords())
# Note that make_dtm calls revwords by default, so this is technically redundant
}