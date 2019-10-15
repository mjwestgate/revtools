\name{make_dtm}
\alias{make_dtm}
\alias{make_DTM}
\title{Construct a document-term matrix (DTM)}
\description{Takes bibliographic data and converts it to a DTM for passing to topic models.}
\usage{
make_dtm(x, stop_words, min_freq = 0.01, max_freq = 0.85,
  ngram_check = TRUE, ngram_quantile = 0.8, return_matrix = FALSE)
}
\arguments{
  \item{x}{a vector of strings}
  \item{stop_words}{optional vector of strings, listing terms to be removed from the DTM prior to analysis}
  \item{min_freq}{minimum proportion of entries that a term must be found in to be retained in the analysis. Defaults to 0.01.}
  \item{max_freq}{maximum proportion of entries that a term must be found in to be retained in the analysis. Defaults to 0.85.}
  \item{ngram_check}{logical: should ngrams be searched for?}
  \item{ngram_quantile}{what quantile of ngrams should be retained. Defaults to 0.8; i.e. the 80th percentile of bigram frequencies after removing all bigrams with frequencies <=2.}
  \item{return_matrix}{logical: should \code{make_dtm} return a \code{matrix}? Defaults to FALSE, in which case a \code{simple_triplet_matrix} is returned instead.}
}
\value{
An object of class 'matrix', listing the terms (columns) present in each string (rows).
}
\details{
This is primarily intended to be called internally by \code{screen_topics}, but is made available for users to generate their own topic models with the same properties as those in revtools.

This function uses some standard tools like stemming, converting words to lower case, and removal of numbers or punctuation. It also replaces stemmed words with the shortest version of all terms that share the same stem, which doesn't affect the calculations, but makes the resulting analyses easier to interpret. It doesn't use part-of-speech tagging.

Words that occur in 2 entries or fewer are always removed by \code{make_dtm}, so values of \code{min_freq} that result in a threshold below this will not affect the result. Arguments to \code{max_freq} are passed as is. Similarly words consisting of three letters or fewer are removed.

This function is synonymous with the earlier function \code{make_DTM}, which will be removed from future versions of \code{revtools}.
}
\seealso{
	\code{\link{run_topic_model}}, \code{\link{screen_topics}}
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)

# construct a document-term matrix
# note: this can take a long time to run for large datasets
x_dtm <- make_dtm(x$title)
dim(x_dtm) # 20 articles, 10 words
}