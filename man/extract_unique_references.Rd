\name{extract_unique_references}
\alias{extract_unique_references}
\title{Create a de-duplicated data.frame}
\description{Take a data.frame of bibliographic information showing potential duplicates (as returned by find_duplicates), and return a data.frame of unique references }
\usage{
extract_unique_references(x, show_source=FALSE)
}
\arguments{
  \item{x}{a data.frame as returned by find_duplicates}
  \item{show_source}{Logical; should a matrix of source values be returned? Defaults to FALSE}
}
\value{
a data.frame containing basic information for each reference (row)
}
\note{
This function creates a simplified version of that given by find_duplicates, by extracting the first reference from each group of unique references. There is no additional processing to ensure this is the 'best' reference from that list.
}