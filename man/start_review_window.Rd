\name{start_review_window}
\alias{start_review_window}
\title{Interactive visualisation of bibliographic data}
\description{Draw an interactive plot of a bibliography. Ordinations are calculated using LDA (library \code{"topicmodels"}) and are displayed using \code{shiny} and \code{plotly}.}
\usage{
start_review_window(x, remove_words)
}
\arguments{
  \item{x}{Bibliographic data, in one of three formats: a list returned by \code{read_bibliography()}; a \code{data.frame}; or a previously saved output from \code{start_review_window} (\code{class("review_info")}).}
  \item{remove_words}{vector of words to be removed from consideration by the Topic Model. If none are given, \code{start_review_window} will use \code{tm::stopwords()}. Note that this vector will be converted to lower case before processing, so the algorithm is not case sensitive.}
}
\value{
This function launches a Shiny app in the users' default browser. 

The display space is divided into three parts. From left to right, these are the sidebar; the plot window; and the selection panel. 

The sidebar shows a series of drop-down menus that can be used to customize or recalculate the central plot. It can be minimized when not in use. Note that the default settings for LDA (5 topics, 1000 iterations) prioritize speed over reliability - higher numbers of iterations will give more reliable results. 

The plot window shows an ordination of article weights calculated using LDA, with articles colored by their highest-weighted topic. Hovering over a point shows the title and abstract below the plot; clicking allows selection or deselection of that article (and optionally displays co-authorship data). Selecting a region of the plot and clicking zooms on the selected region; double-clicking without selecting a region returns the plot to its full extent.

The selection panel gives information on progress in selecting/deselecting articles. It also contains windwos for displaying topic-level infromation and article abstracts. All boxes in this panel can be minimized when not required.

Upon completion, the user can export information to a .csv or .rda file (saved to the working directory) using the 'Save' tab.
}
\examples{
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-read_bibliography(file_location)
\dontrun{start_review_window(x)}
}