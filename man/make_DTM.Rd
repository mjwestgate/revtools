\name{make_DTM}
\alias{make_DTM}
\title{Construct a document-term matrix (DTM)}
\description{Takes bibliographic data and converts it to a DTM for passing to topic models.}
\usage{
make_DTM(x, stop_words)
}
\arguments{
  \item{x}{an object of class \code{bibliography} or \code{data.frame} containing bibliographic data}
  \item{stop_words}{optional vector of strings, listing terms to be removed from the DTM prior to analysis}
}
\value{
An object of class 'matrix', listing the terms (columns) present in each reference (rows)
}
\note{
This is primarily intended to be called internally by \code{start_review_window()}, but is made available for users to generate their own topic models with the same properties as those in revtools. It bascially takes any words in the title, keywords and abstracts of the supplied references, and uses them to construct a DTM.

This function uses some standard tools like stemming, converting words to lower case, and removal of numbers or punctuation. It also replaces stemmed words with the most common full word, which doesn't affect the calculations, but makes the resulting analyses easier to interpret. It doesn't use part-of-speech tagging.
}
\examples{
# import some data
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-read_bibliography(file_location)

# construct a document-term matrix
# note: this can take a long time to run for large datasets
x_DTM<-make_DTM(x)
dim(x_DTM) # 20 articles, 1069 words
}