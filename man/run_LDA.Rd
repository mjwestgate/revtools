\name{run_LDA}
\alias{run_LDA}
\title{Calculate a topic model}
\description{Run a topic model using either \code{LDA} or \code{CTM} from the \code{topicmodels} package.}
\usage{
run_LDA(x, topic_model, n_topics, iterations)
}
\arguments{
  \item{x}{a Document Term Matrix (DTM)}
  \item{topic_model}{string specififying the type of model to run. Either 'lda' (the default) or 'ctm'.}
  \item{n_topics}{Number of topics to calculate}
  \item{iterations}{The number of iterations. Only relevant for LDA.}
}
\value{
A topic model with the specified parameters.
}
\note{
This is a basic wrapper function designed to allow consistent specification of model parameters within \code{shiny} apps. It doesn't do anything very clever.
}
\examples{
# import data
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-read_bibliography(file_location)

# run a topic model based on these data
# note: the following lines can take a very long time to run, especially for large datasets!
x_DTM<-make_DTM(x)
\dontrun{x_LDA<-run_LDA(x_DTM, "lda", 5, 5000)}
}