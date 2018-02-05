\name{find_duplicates}
\alias{find_duplicates}
\title{Locate duplicated references within a data.frame}
\description{Identify potential duplicates within a \code{data.frame} containing title, journal and year data for each reference. Such a \code{data.frame} can be created by calling \code{as.data.frame} on an object of class bibliography (e.g. as returned by \code{read_bibliography()}).}
\usage{
find_duplicates(x)
}
\arguments{
  \item{x}{a \code{data.frame} containing title, journal and year data for each reference}
}
\value{
a  \code{data.frame} with the same columns as the initial data, with a numeric variable named 'group'; rows with the same value are probable duplicates.}
\note{
This function has a few odd features. It uses  \code{stringdist} to locate article titles that are similar to one another, rather than exact matching. This is done within a  \code{while} loop; in each run of the loop, rows that have similar journal titles or publication years are checked for potential duplication, while rows with missing values of these variables are tested in every run. This makes the code fairly comprehensive at the cost of speed. 

A method that tests string distances between all pairs would locate more duplicates, but would be substantially slower. Conversely, a method that split the dataset into mutually exclusive groups could be vectorized and would be correspondingly faster, but as the identity of these groups is inherently ambigious, it would probably reduce the hit rate.
}
\examples{
# import data
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-as.data.frame(read_bibliography(file_location)) 

# generate then locate some 'fake' duplicates
x_duplicated<-rbind(x, x[1:5,])
x_check<-find_duplicates(x_duplicated)
# returns a data.frame with an added 'group' column
}