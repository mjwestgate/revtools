\name{screen_duplicates}
\alias{screen_duplicates}
\title{Shiny app for screening for duplicated articles}
\description{In development
}
\usage{
screen_duplicates(x)
}
\arguments{
  \item{x}{Optionally import bibliographic data from a \code{.csv} file or a bibliographic format that can be imported by \code{read_bibliography}.}
}
\value{
This function launches a Shiny app in the users' default browser, allowing the user to customize their parameters for duplicate detection, and visualise the results.
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
\dontrun{screen_duplicates(x)}
}
