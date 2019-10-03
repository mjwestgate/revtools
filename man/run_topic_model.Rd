\name{run_topic_model}
\alias{run_topic_model}
\alias{run_LDA}
\title{Calculate a topic model}
\description{Calculate a topic model using Latent Dirichlet Allocation (\code{LDA}) or Correlated Topic Models (\code{CTM}), using the \code{topicmodels} package.}
\usage{
run_topic_model(dtm, type = "lda", n_topics = 5, iterations = 2000)
}
\arguments{
  \item{dtm}{a Document Term Matrix (DTM)}
  \item{type}{string specififying the type of model to run. Either 'lda' (the default) or 'ctm'.}
  \item{n_topics}{Number of topics to calculate}
  \item{iterations}{The number of iterations. Only relevant for LDA.}
}
\value{
A topic model with the specified parameters.
}
\note{
This is a basic wrapper function designed to allow consistent specification of model parameters within \code{shiny} apps. \code{run_LDA} is a synonym for \code{run_topic_model} and will be removed from future versions.
}
\seealso{
	\code{\link{make_dtm}} for constructing data to pass to this function; \code{\link{screen_topics}} for interactive visualisation of topic model results.
}
\examples{
# import data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools"
)
x <- read_bibliography(file_location)

# run a topic model based on these data
# note: the following lines can take a very long time to run, especially for large datasets!
x_dtm <- make_dtm(x$title)
\dontrun{x_lda <- run_topic_model(x_dtm, "lda", 5, 5000)}
}