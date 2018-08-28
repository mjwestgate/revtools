\name{screen_titles}
\alias{screen_titles}
\title{Shiny app for screening articles by their titles}
\description{This is a simple app for displaying bibliographic data one entry at a time, and manually selecting or excluding them. Articles can be ordered as in the input dataset, alphabetically by title, or in random order (the default).
}
\usage{
screen_titles(x)
}
\arguments{
  \item{x}{Optionally import bibliographic data from a \code{.csv} file or a bibliographic format that can be imported by \code{read_bibliography}.}
}
\value{
This function launches a Shiny app in the users' default browser, allowing the user to select or exclude individual articles in order.
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
