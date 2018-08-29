\name{extract_unique_references}
\alias{extract_unique_references}
\title{Create a de-duplicated data.frame}
\description{Take a data.frame of bibliographic information showing potential duplicates (as returned by \code{find_duplicates}), and return a data.frame of unique references.}
\usage{
extract_unique_references(x, matches)
}
\arguments{
  \item{x}{a data.frame as returned by find_duplicates}
  \item{matches}{a vector of matches, e.g. as returned from \code{\link{find_duplicates}}}
}
\value{
a data.frame containing basic information for each reference (row)
}
\note{
This function creates a simplified version of \code{x}, by extracting the reference from each group of 'identical' references that contains the most text. It is assumed that this is the most 'complete' record of those available in the dataset. This function does not merge data from multiple 'identical' records due to the potential for mis-matching that this approach would create.
}
\seealso{
	\code{\link{find_duplicates}}
}
\examples{
# import data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools"
)
x <- as.data.frame(read_bibliography(file_location))

# generate duplicated references (for example purposes)
x_duplicated <- rbind(x, x[1:5,])

# locate and extract unique references
x_check <- find_duplicates(x_duplicated)
x_unique <- extract_unique_references(x_duplicated, matches = x_check)
}