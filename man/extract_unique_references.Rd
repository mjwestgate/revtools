\name{extract_unique_references}
\alias{extract_unique_references}
\title{Create a de-duplicated data.frame}
\description{Take a data.frame of bibliographic information showing potential duplicates (as returned by \code{find_duplicates}), and return a data.frame of unique references }
\usage{
extract_unique_references(x, show_source=FALSE)
}
\arguments{
  \item{x}{a data.frame as returned by find_duplicates}
  \item{show_source}{If x contains a column named 'source', selecting TRUE will cause the output to contain extra columns. Specifically, each added column will be named for a unique value of x$source, and each row will list the source(s) that contained that reference. Defaults to FALSE}
}
\value{
a data.frame containing basic information for each reference (row)
}
\note{
This function creates a simplified version of that given by find_duplicates, by extracting the first reference from each group of unique references. There is no additional processing to ensure this is the 'best' reference from that list.
}
\seealso{\link{\code{find_duplicates}}}
\examples{
# import data
data("example_bibliography")

# generate some 'fake' duplicates
x<-as.data.frame(example_bibliography)
x_duplicated<-rbind(x, x[1:5,])

# look for duplicates
x_check<-find_duplicates(x_duplicated)

# if any are found, extract unique values
x_unique<-extract_unique_references(x_check)
}