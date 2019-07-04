\name{screen_topics}
\alias{screen_topics}
\alias{start_review_window}
\title{Shiny app for screening bibliographies using topic models}
\description{Screening is usually achieved by manually sorting titles or abstracts one at a time. \code{screen_topics} offers an alternative by allowing the user to group data by any column in the input dataset, and running a topic model on the resulting data. This allows a great deal of flexibility to locate patterns in journals, years, or authors, rather than just articles. Data points can be selected or excluded individually, or by topic.
}
\usage{
screen_topics(x, remove_words, max_file_size)
}
\arguments{
  \item{x}{An (optional) object of class \code{data.frame} or \code{bibliography} to open in the browser. If empty, the app will launch with no data. Data can be added within the app via the 'import' button.}
  \item{remove_words}{Optional vector of words to be removed from consideration by the Topic Model. If none are given, \code{screen_topics} will use \code{\link{revwords}}. Note that this vector will be converted to lower case before processing, so the algorithm is not case sensitive.}
  \item{max_file_size}{Optional argument to set the maximum file size (in MB) that the app will accept.}
}
\value{This function launches a Shiny app in the users' default browser.}
\details{
The display space is divided into three parts. From left to right, these are the sidebar; the plot window; and the selection panel.

The sidebar shows a series of drop-down menus that can be used to customize or recalculate the central plot. It can be hidden when not in use. Note that the default settings for LDA (5 topics, 10,000 iterations) prioritize speed over reliability - higher numbers of iterations will give more reliable results.

The plot window shows an ordination of article weights calculated using LDA, with articles colored by their highest-weighted topic. Hovering over a point shows the title and abstract below the plot; clicking allows selection or deselection of that article (and optionally displays co-authorship data). Selecting a region of the plot and clicking zooms on the selected region; double-clicking without selecting a region returns the plot to its full extent.

The selection panel gives information on progress in selecting/deselecting articles. It also contains windwos for displaying topic-level infromation and article abstracts. All boxes in this panel can be minimized when not required.

Ordinations are calculated using LDA (library \code{"topicmodels"}) and are displayed using \code{shiny} and \code{plotly}.

When you have finished viewing/screening, you can export information to a .csv or .rda file (saved to the working directory) using the 'Save' tab.

Note that "\code{start_review_window}" is the earlier form of this function; this has been deprecated and will be removed from future versions of \code{revtools}.
}
\seealso{
	\code{\link{screen_titles}} or \code{\link{screen_abstracts}} for manual screening; \code{\link{screen_topics_progress-class}} for saving and restoring progress in \code{screen_topics}.
}
\examples{
# to run the app and upload data interactively
\dontrun{screen_topics()}
# or to specify data from the workspace
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
# to run the app using these data:
\dontrun{screen_topics(x)}
# or to run the app & save results to the workspace:
\dontrun{result <- screen_topics(x)}
}
