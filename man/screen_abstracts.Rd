\name{screen_abstracts}
\alias{screen_abstracts}
\title{Shiny app for screening articles by their abstracts}
\description{This is a simple app for displaying bibliographic data one entry at a time, and manually selecting or excluding them. Articles can be ordered as in the input dataset, alphabetically by title, or in random order (the default).
}
\usage{
screen_abstracts(x, max_file_size)
}
\arguments{
  \item{x}{An (optional) object of class \code{data.frame} or \code{bibliography} to open in the browser. If empty, the app will launch with no data. Data can be added within the app via the 'import' button.}
  \item{max_file_size}{Optional argument to set the maximum file size (in MB) that the app will accept.}
}
\value{
This function launches a Shiny app in the users' default browser, allowing the user to select or exclude individual articles.
}
\seealso{
	\code{\link{screen_titles}} for screening articles in groups rather than individually; \code{\link{screen_topics}} to view articles as a point cloud.
}
\examples{
# to run the app and upload data interactively
\dontrun{screen_abstracts()}
# or to specify data from the workspace
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
# to run the app using these data:
\dontrun{screen_abstracts(x)}
# or to run the app & save results to the workspace:
\dontrun{result <- screen_abstracts(x)}
}
