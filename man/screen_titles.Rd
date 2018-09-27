\name{screen_titles}
\alias{screen_titles}
\title{Shiny app for screening articles by their titles}
\description{This is a simple app for displaying the titles in a bibliographic dataset in small groups, and manually selecting or excluding them. Articles can be ordered as in the input dataset, alphabetically by title, or in random order (the default).
}
\usage{
screen_titles(x)
}
\arguments{
  \item{x}{An (optional) object of class \code{data.frame} or \code{bibliography} to open in the browser. If empty, the app will launch with no data. Data can be added within the app via the 'import' button.}
}
\value{
This function launches a Shiny app in the users' default browser, allowing the user to select or exclude articles.
}
\seealso{
	\code{\link{screen_abstracts}} for screening articles one at a time rather than in groups; \code{\link{screen_topics}} to view articles as a point cloud.
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
\dontrun{screen_abstracts(x)}
}
