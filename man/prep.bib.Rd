\name{prep.bib}
\alias{prep_bib}
\title{Prepare bibliographic data for interactive plotting}
\usage{
prep.bib(x, stop.words)
}
\arguments{
  \item{x}{a list of bibliographic data imported using read.bib}
  \item{stop.words}{A list of stopwords to be removed from the document term matrix (and therefore from analysis by LDA). Default is to use tm::stopwords("english")}
}
\value{
Returns a list with three components:
  \describe{
    \item{bibliography}{a copy of the input (x)}
	\item{dtm}{a document (rows) by term (cols) matrix, listing words found in the abstracts of each article}
	\item{coauthors}{a data.frame listing links between entries from bibligrpahy that appear to share authors. This code is under devopment, but currently attempts to account for differences in presentation of first and middle names or initials.}
}}
\description{
Performs some simple analyses to accumulate data in the correct format. Kept separate from draw.bib for two reasons: 1. to speed up plotting by Shiny, 2. to allow user checkng of data supplied to visualisation (i.e. to alow exclusion of words, correction of author lists etc.).
}