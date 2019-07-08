\name{distribute_tasks}
\alias{distribute_tasks}
\title{Divide a set of articles among two or more reviewers}
\description{A common task in systematic review is to divide a dataset of articles located by a search (typically involving >1 databases) and distributing them amongst a team of reviewers for screening. This function takes a dataset divides it among the specified number of reviewers, returning the resulting data.frames either in a list to the workspace, or (by default) as a set of .csv files in the specified directory. The resulting files can be passed to any of the screening functions provided by revtools, i.e. \code{\link{screen_titles}}, \code{\link{screen_abstracts}}, or \code{\link{screen_topics}}.}
\usage{
distribute_tasks(data, reviewers, write_csv = TRUE,
    file_name = "reviewer.csv", return_data = FALSE, ...)
}
\arguments{
  \item{data}{a vector of strings}
  \item{reviewers}{Either a \code{data.frame} as returned by \code{allocate_effort}, an integer giving the number of reviewers, or a vector of strings giving reviewer names.}
  \item{write_csv}{Logical - should the function write a set of csv files (1 per reviewer)? Defaults to TRUE}
  \item{file_name}{a file path & name showing where .csv files should be saved. Ignored if \code{write_csv} is FALSE. Defaults to 'reviewer_[name].csv'.}
  \item{return_data}{Logical - should a list be (invisibly) returned, in which each entry is the data sent to a single reviewer? Defaults to FALSE.}
  \item{...}{Further arguments passed to \code{allocate_effort}}
}
\value{
Invisibly returns a list of data.frames, each with same columns as \code{data} but containing only a subset of rows.
}
\details{
The dataset is allocated each author in the proportion of articles specified by \code{\link{allocate_effort}}, with the identity of articles passed to reviewer being chosen by \code{rnorm}. As a result, this function is very sensitive to the inputs provided to \code{\link{allocate_effort}}, so it is often worth running that function first and checking the results to be certain that effort is being distributed in a way that you are happy with.
}
\seealso{
	\code{\link{allocate_effort}} for a detailed description of how the division among reviewers is accomplished.
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
result <- distribute_tasks(x, 4, write_csv = FALSE) # split evenly among 4 reviewers
}