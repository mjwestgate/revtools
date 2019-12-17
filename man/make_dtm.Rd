\name{make_dtm}
\alias{make_dtm}
\title{Construct a document-term matrix (DTM)}
\description{Takes bibliographic data and converts it to a DTM for passing to topic models.}
\usage{
make_dtm(x, stop_words, min_freq = 0.01, max_freq = 0.85,
  bigram_check = TRUE, bigram_quantile = 0.8,
  retain_empty_rows = FALSE)
}
\arguments{
  \item{x}{a vector or \code{data.frame} containing text}
  \item{stop_words}{optional vector of strings, listing terms to be removed from the DTM prior to analysis. Defaults to \code{revwords()}.}
  \item{min_freq}{minimum proportion of entries that a term must be found in to be retained in the analysis. Defaults to 0.01.}
  \item{max_freq}{maximum proportion of entries that a term must be found in to be retained in the analysis. Defaults to 0.85.}
  \item{bigram_check}{logical: should ngrams be searched for?}
  \item{bigram_quantile}{what quantile of ngrams should be retained. Defaults to 0.8; i.e. the 80th percentile of bigram frequencies after removing all bigrams with frequencies <=2.}
  \item{retain_empty_rows}{logical: should the function return an object with the same length as the input string (TRUE), or remove rows that contain no text after filtering rare & common words etc? (FALSE, the default). The latter is the default because this is required by \code{run_topic_model}.}
}
\value{
An object of class \code{simple_triplet_matrix}, listing the terms (columns) present in each row or string.
}
\details{
This is primarily intended to be called internally by \code{screen_topics}, but is made available for users to generate their own topic models with the same properties as those in revtools.

This function uses some standard tools like stemming, converting words to lower case, and removal of numbers or punctuation. It also replaces stemmed words with the shortest version of all terms that share the same stem, which doesn't affect the calculations, but makes the resulting analyses easier to interpret. It doesn't use part-of-speech tagging.

Words that occur in 2 entries or fewer are always removed by \code{make_dtm}, so values of \code{min_freq} that result in a threshold below this will not affect the result. Arguments to \code{max_freq} are passed as is. Similarly words consisting of three letters or fewer are removed.

If \code{retain_empty_rows} is FALSE (the default) and the object returned is named \code{z}, then \code{as.numeric(z$dimnames$Docs)} provides an index to which entries have been retained from the input vector (\code{x}).
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

# to return a matrix where length(output) == nrow(x)
x_dtm <- make_dtm(x$title, retain_empty_rows = TRUE)
x_dtm <- as.matrix(x_dtm) # to convert to class 'matrix'
dim(x_dtm) # 20 articles, 10 words
}