\name{preload_screen_abstracts}
\alias{preload_screen_abstracts}
\title{Preload an app for screening article abstracts}
\description{This function takes some data and generates a self-contained app, stored in a .RData file, that can be forwarded to collaborators for screening. A series of options can be pre-set to customize user experience. Once launched, these options cannot be altered by the user.}
\usage{
preload_screen_abstracts(x, file_out = "screen_abstracts_preloaded.RData", app_control)
}
\arguments{
  \item{x}{A \code{data.frame} containing bibliographic information.}
  \item{file_out}{Name of the output \code{RData} file.}
  \item{app_control}{A list containing additional information to customize UI. See details for what options can be set.}
}
\value{
Returns an \code{.RData} file containing an app with the specified options and data. That file contains two objects: \code{app}, which is a Shiny app that can be run by calling \code{print(app)}; and \code{data}, which stores the underlying dataset including any user decisions, notes and timing information.
}
\details{
List entries that can be specified within \code{app_control} are as follows:
\itemize{
  \item{\code{show_identifying_info} - Logical: should author names, publication year and journal title be displayed? Defaults to FALSE.}
  \item{\code{time_responses} - Logical: should the app record timing information? If TRUE (the default) record the time spent on each page, and the time at which the page was loaded.}
  \item{\code{keyword_highlighting} - Logical: should the app highlight user-specified keywords? Defaults to FALSE.}
  \item{\code{keywords} - Optional set of keywords to be highlighted in article titles and abstracts.}
  \item{\code{highlight_color} - Optional color for keyword highlighting. Defaults to "red".}
  \item{\code{rank_by} - Specify how entries should be ordered. Should be one of \code{"initial"} (the default) to maintain the order of \code{x}, \code{"random"} to randomize order via \code{rnorm}, \code{"alphabetical"} for alphabetical by article title, or \code{"relevance"} for ranking by number of matched keywords.}
  \item{\code{save_csv} - Logical: should the app save a \code{csv} file as well as a \code{.RData} file? Defaults to FALSE.}
}
}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)

\dontrun{
preload_screen_abstracts(data,
  app_control = list(
    keyword_highlighting = TRUE,
    highlight_color = "red",
    keywords = c("management", "frog", "bird", "fire"),
    save_csv = FALSE
  )
)
# after re-loading output .RData file, run by using:
library(revtools)
print(screen_abstracts_preloaded)
runApp(screen_abstracts_preloaded)
# the above options are equivalent
}
}
