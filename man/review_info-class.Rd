\name{review_info-class}
\title{Description of class 'review_info'}
\alias{review_info-class}
\description{Class 'review' is an S3 class designed to store data from the shiny app launched by \code{start_review_window}. This is important because it stores the many decisions that users might make about inclusion or exclusion of individual references from the corpus, information that will be needed for later stages of the review. Class review_info can also be passed to \code{start_review_window} to continue working on a previously altered dataset.}
\section{slots}{
Class 'review_info' has five slots containing the following information:
	\itemize{
		\item \strong{info} duplicate of data passed to \code{start_review_window}
		\item \strong{dtm} document-term matrix, created by \code{make_DTM}
		\item \strong{model} most recent topic model
		\item \strong{plotinfo} data listing points to be plotted in \code{start_review_window}
		\item \strong{infostore} data listing decisions about inclusion/exclusion of references, words or topics
	}		
}