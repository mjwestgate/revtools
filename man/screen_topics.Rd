\name{screen_topics}
\alias{screen_topics}
\alias{start_review_window}
\title{Shiny app for screening bibliographies using topic models}
\description{Screening is usually achieved by manually sorting titles or abstracts one at a time. \code{screen_topics} offers an alternative by allowing the user to group data by any column in the input dataset, and running a topic model on the resulting data. This allows a great deal of flexibility to locate patterns in journals, years, or authors, rather than just articles. Data points can be selected or excluded individually, or by topic.
}
\usage{
screen_topics(x, remove_words)
}
\arguments{
  \item{x}{Optionally import bibliographic data from a \code{.csv} file or a bibliographic format that can be imported by \code{read_bibliography}.}
  \item{remove_words}{Optional vector of words to be removed from consideration by the Topic Model. If none are given, \code{start_review_window} will use \code{tm::stopwords()}. Note that this vector will be converted to lower case before processing, so the algorithm is not case sensitive.}
}
\value{
This function launches a Shiny app in the users' default browser.The display space is divided into three parts. From left to right, these are the sidebar; the plot window; and the selection panel.

The sidebar shows a series of drop-down menus that can be used to customize or recalculate the central plot. It can be minimized when not in use. Note that the default settings for LDA (5 topics, 1000 iterations) prioritize speed over reliability - higher numbers of iterations will give more reliable results.

The plot window shows an ordination of article weights calculated using LDA, with articles colored by their highest-weighted topic. Hovering over a point shows the title and abstract below the plot; clicking allows selection or deselection of that article (and optionally displays co-authorship data). Selecting a region of the plot and clicking zooms on the selected region; double-clicking without selecting a region returns the plot to its full extent.

The selection panel gives information on progress in selecting/deselecting articles. It also contains windwos for displaying topic-level infromation and article abstracts. All boxes in this panel can be minimized when not required.

Ordinations are calculated using LDA (library \code{"topicmodels"}) and are displayed using \code{shiny} and \code{plotly}.

When you have finished viewing/screening, you can export information to a .csv or .rda file (saved to the working directory) using the 'Save' tab.

Note that \code{"start_review_window()"} is the earlier form of this function; this has been deprecated and will be removed from future versions of \code{revtools}.
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
\dontrun{screen_topics(x)}
}
