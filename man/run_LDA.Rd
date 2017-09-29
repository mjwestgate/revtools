\name{run_LDA}
\alias{run_LDA}
\title{Calculate a topic model}
\description{Run a topic model using either 'LDA' or 'CTM' from the topicmodel package.}
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
\note{
This is a basic wrapper function designed to allow consistent specification of model parameters within Shiny apps. It doesn't do anything very clever.
}