\name{format_citation}
\alias{format_citation}
\title{Write references in a clean format}
\description{Basic function to display information on each reference in a readily interpreable way (currently based on the Harvard system)}
\usage{
format_citation(x, abstract=FALSE, details=TRUE)
}
\arguments{
  \item{x}{A single entry from an object of class 'bibliography'}
  \item{abstract}{Logical; should the abstract be shown, if present? Defaults to FALSE.}
  \item{details}{Logical; should name, year and journal be displayed? Defaults to TRUE. If FALSE simply returns article title}
}
\value{
formatted text giving the details of the reference in question. Primarily used as an internal function within print.bibliography and summary.review_info.
}