\name{screen_topics_progress-class}
\title{Description of class 'screen_topics_progress'}
\alias{screen_topics_progress-class}
\description{\code{screen_topics_progress} is an S3 class designed to store data from \code{screen_topics}, allowing the user to re-load a previously calculated topic model. If you just want to save your decisions on article inclusion/exclusion, along with your notes, then this is probably overkill as that information can simply be exported as a .csv file.}
\section{slots}{
Class 'screen_topics_progress' has seven slots containing the following information:
	\itemize{
		\item \strong{raw} duplicate of data passed to \code{screen_topics}
    \item \strong{stopwords} vector of words excluded from the dtm
    \item \strong{columns} vector of column names in the original dataset
    \item \strong{grouped} a data.frame showing grouped data as specified by the user
		\item \strong{dtm} document-term matrix, created by \code{make_DTM}
		\item \strong{model} most recent topic model, created by \code{run_LDA}
    \item \strong{plot_ready} data needed for the main plot (coordinates etc.)
	}
}