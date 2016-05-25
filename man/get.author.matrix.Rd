\name{get_author_matrix}
\alias{get.author.matrix}
\title{Calculate a coauthor matrix}
\description{
Function to calculate matrix showing authorship of each article. Includes some basic tools to merge data from different spellings of the same name. Generally called by prep.bib, but available here for data checking etc.
}
\usage{
get.author.matrix(x)
}
\arguments{
  \item{x}{Bibliography, e.g. as imported by import.bib}
}
\value{
Binary matrix showing which documents (rows) were (co)authored by which people (cols)
}
