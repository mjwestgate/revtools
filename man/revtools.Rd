\docType{package}
\name{revtools-package}
\alias{revtools}
\title{revtools: Tools to support reviews and evidence synthesis}
\description{Researchers commonly need to summarize scientific information, a process known as 'evidence synthesis'. The first stage of a synthesis process (such as a systematic review or meta-analysis) is to download a list of references from academic search engines such as 'Web of Knowledge' or 'Scopus'. This information can be sorted manually (the traditional approach to systematic review), or the user can draw on tools from machine learning to help them visualise patterns in the corpus. \code{revtools} uses topic models to render ordinations of text drawn from article titles, keywords and abstracts, and allows the user to interactively select or exclude individual references, words or topics. \code{revtools} does not currently provide tools for analysis of data drawn from those references, features that are available in other packages such as \code{metafor}.}
\section{Functions}{
	\strong{Import & export}
	\itemize{
		\item \code{\link{read_bibliography}} Import bibliographic data
		\item \code{\link{write_bibliography}} Export bibliographic data
	}
	\strong{Data storage and manipulation}
	\itemize{
		\item \code{\link{bibliography-class}} Format for storing bibliographic data
		\item \code{\link{bibliography-methods}} Print, summary, as.bibliography, as.data.frame and [ methods for class 'bibliography'
	}
	\strong{Duplicate detection}
	\itemize{
    \item \code{\link{fuzzywuzzy_functions}} Fuzzy string matching
		\item \code{\link{find_duplicates}} Locate potentially duplicated references
		\item \code{\link{extract_unique_references}} return a data.frame with only 'unique' references
	}
	\strong{Topic modelling and visualisation}
	\itemize{
		\item \code{\link{make_DTM}} Construct a Document-Term Matrix from bibliographic data
		\item \code{\link{run_LDA}} Wrapper function for topic models
		\item \code{\link{screen_visual}} Launch a Shiny app for reference sorting
	}
}
