\name{allocate_effort}
\alias{allocate_effort}
\title{Determine optimal way to divide articles among 2 or more reviewers}
\description{This function takes information on the number (and optionally, identity) of reviewers and combines it with data on how it should be allocated to return a data.frame showing the proportion of articles to be assessed by each person.}
\usage{
allocate_effort(reviewers, effort, proportion_checked,
    max_reviewers = 3, precision = 2, quiet = TRUE)
}
\arguments{
  \item{reviewers}{Either an integer giving the number of reviewers, or a vector of strings giving reviewer names.}
  \item{effort}{Either a single number giving the proportion of articles to be reviewed by all reviewers, or a numeric vector giving a unique proportion for each reviewer. If the latter must be consistent with number given by 'reviewers' above.}
  \item{proportion_checked}{Numeric value giving the proportion of entries that should be screened by two or more reviewers.}
  \item{max_reviewers}{the maximum number of reviewers that should screen any single article. Useful for avoiding redundancy when working with large teams.}
  \item{precision}{Number of decimal places with which to report results. Defaults to 2.}
  \item{quiet}{Logical - should the function return a summary of the proportion of articles allocated to each reviewer? Defaults to TRUE.}
}
\value{
Invisibly returns a data.frame giving one column per reviewer plus an extra column of proportions. The reviewer columns are binary and show which proportions apply to each person or combination of people.
}
\details{
This function makes an attempt to return a sensible distribution of effort among a number of reviewers. If effort is given as a single value (or not provided at all), then the result is calculated exactly such that all proportions sum to 1. Conversely, if effort is given as a numeric vector of length >1 and contains a range of values, then the function tries to optimize the proportion of articles allocated to each person, while also matching constrains given by \code{proportion_checked} and \code{max_reviewers}. In this case, and depending on the inputs, it is possible that no perfect solution will exist, meaning that some reviewers may be allocated a higher proportion of articles than anticipated. For this reason it is often worth setting \code{quiet = FALSE} when running \code{allocate_effort} with variation in reviewer effort, to allow assessment of the results.
}
\seealso{
	\code{\link{distribute_tasks}} for how to use the output of \code{allocate_effort} to split a dataset.
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
# simple case - split evenly among 4 reviewers
result <- allocate_effort(4, quiet = TRUE)
# more complex - specify names and amount of overlap
result <- allocate_effort(
  reviewers = c("john", "paul", "george", "ringo"),
  proportion_checked = 0.2,
  max_reviewers = 3,
  quiet = TRUE
  )
# most complex - specify uneven effort among reviewers (experimental)
result <- allocate_effort(
  reviewers = 4,
  effort = c(0.9, 0.7, 0.5, 0.3),
  max_reviewers = 3,
  quiet = TRUE
  )
}