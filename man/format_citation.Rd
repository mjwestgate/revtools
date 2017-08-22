\name{format_citation}
\alias{format_citation}
\title{Write references in a clean format}
\description{Basic function to display information on each reference in a readily interpreable way (currently based on the Harvard system)}
\usage{
format_citation(x)
}
\arguments{
  \item{x}{A single entry from an object of class 'bibliography'}
}
\value{
formatted text giving the details of the reference in question. Primarily used as an internal function within print.bibliography and review_info.
}