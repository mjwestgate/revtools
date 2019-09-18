\name{screen_duplicates}
\alias{screen_duplicates}
\title{Shiny app for locating and excluding duplicated entries in a dataset}
\description{This is a simple app for calculating, displaying and screening potential duplicates in bibliographic data}
\usage{
screen_duplicates(x, max_file_size)
}
\arguments{
  \item{x}{An (optional) object of class \code{data.frame} or \code{bibliography} to open in the browser. If empty, the app will launch with no data. Data can be added within the app via the 'import' button.}
  \item{max_file_size}{Optional argument to set the maximum file size (in MB) that the app will accept.}
}
\value{
This function launches a Shiny app in the users' default browser, allowing the user to customize their parameters for duplicate detection, and visualise the results.
}
\details{This app is effectively a wrapper for \code{\link{find_duplicates}}, with the added option to manually screen pairs of duplicates to check the results. Consequently, this is a more reliable method than \code{\link{extract_unique_references}} of dealing with the duplicates identified by \code{\link{find_duplicates}}, and for testing whether that function has returned sensible results for a given dataset.
}
\seealso{
	\code{\link{screen_titles}} or \code{\link{screen_abstracts}} for manual screening of individual articles.
}
\examples{
# to run the app and upload data interactively
\dontrun{screen_duplicates()}
# or to specify data from the workspace
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
# to run the app using these data:
\dontrun{screen_duplicates(x)}
# or to run the app & save results to the workspace:
\dontrun{result <- screen_duplicates(x)}
}
