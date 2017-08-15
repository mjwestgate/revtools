\name{pretty.citations}
\alias{pretty.citations}
\title{Write references in a clean format}
\description{Basic function todisplay information on each reference in a readily interpreable way (currently based on the Harvard system)}
\usage{
pretty.citations(x)
}
\arguments{
  \item{x}{a bibdata list, or a single entry from the same}
}
\value{
a vector of same length as x showing the references in question. If length(x)>5, only the first 5 references will be shown
}