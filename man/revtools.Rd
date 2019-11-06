\docType{package}
\name{revtools-package}
\alias{revtools}
\title{revtools: Tools to support reviews and evidence synthesis}
\description{Researchers commonly need to summarize scientific information, a process known as 'evidence synthesis'. The first stage of a synthesis process (such as a systematic review or meta-analysis) is to download a list of references from academic search engines such as 'Web of Knowledge' or 'Scopus'. The traditional approach to systematic review is then to sort these data manually, first by locating and removing duplicated entries, and then screening to remove irrelevant content by viewing titles and abstracts (in that order). 'revtools' provides interfaces for each of these tasks. An alternative approach, however, is to draw on tools from machine learning to visualise patterns in the corpus. In this case, you can use 'revtools' to render ordinations of text drawn from article titles, keywords and abstracts, and interactively select or exclude individual references, words or topics.}
\section{Functions}{
	\strong{Article screening}
  \itemize{
		\item \code{\link{screen_duplicates}} Screen for duplicates
  	\item \code{\link{screen_titles}} Screen articles by title
    \item \code{\link{screen_abstracts}} Screen articles by abstract
		\item \code{\link{screen_topics}} Screen data by topic
  }
	\strong{Bibliographic methods}
	\itemize{
		\item \code{\link{read_bibliography}} Import bibliographic data
		\item \code{\link{write_bibliography}} Export bibliographic data
		\item \code{\link{bibliography-class}} Format for storing bibliographic data
		\item \code{\link{bibliography-methods}} Print, summary, as.bibliography, as.data.frame and [ methods for class 'bibliography'
    \item \code{\link{tag_lookup}} Lookup table for ris tag transformations
    \item \code{\link{merge_columns}} rbind two data.frames with different numbers of columns
    \item \code{\link{format_citation}} Return a clean citation from a bibliography or data.frame
    \item \code{\link{add_line_breaks}} Set a maximum character width for strings in a vector
  }
  \strong{Distributing tasks among a team}
  \itemize{
    \item \code{\link{allocate_effort}} Specify how to distribute screening effort among a team of reviewers
    \item \code{\link{distribute_tasks}} Split a dataset among a team of reviewers
    \item \code{\link{aggregate_tasks}} Combine screening results from a team of reviewers
	}
	\strong{Duplicate detection}
	\itemize{
    \item \code{\link{fuzz_functions}} Fuzzy string matching
		\item \code{\link{find_duplicates}} Locate potentially duplicated references
		\item \code{\link{extract_unique_references}} Return a data.frame with only 'unique' references
		\item \code{\link{screen_duplicates}} Screen for duplicates
	}
	\strong{Text mining}
	\itemize{
    \item \code{\link{revwords}} Stopwords used in revtools functions
		\item \code{\link{make_dtm}} Construct a Document-Term Matrix from bibliographic data
		\item \code{\link{run_topic_model}} Wrapper function for topic models
		\item \code{\link{screen_topics}} Screen data by topic
	}
}
