\name{revtools_stopwords}
\alias{revtools_stopwords}
\title{Load a set of stopwords}
\description{Generates a list of stopwords, consisting of all the terms given by \code{tm::stopwords}, plus some extra terms (mainly words that designate numbers).}
\usage{
revtools_stopwords()
}
\value{
A vector of stopwords.
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
x_DTM <- make_DTM(x,
  stop_words = revtools_stopwords())
# Note that make_DTM calls revtools_stopwords by default, so this is technically redundant
}