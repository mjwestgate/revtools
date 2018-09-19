\name{screen_topics_progress-methods}
\alias{screen_topics_progress-methods}
\alias{summary.screen_topics_progress}
\title{Methods for class 'screen_topics_progress'}
\description{Tools to display useful information on class \code{screen_topics_progress}.}
\usage{
\method{summary}{screen_topics_progress}(object, ...)
}
\arguments{
  \item{object}{An object of class 'screen_topics_progress'}
  \item{...}{Any further information}
}
\value{
Prints useful information to the workspace.
}
\note{
Class \code{screen_topics_progress} is a format for exporting large quantities of data during reviews. It is typically stored within a .rds file in the working directory. When re-imported to R using \code{readRDS}, this file will contain an object of class \code{screen_topics_progress}.
}