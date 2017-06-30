\name{draw_bib}
\alias{draw.bib}
\title{Interactive visualisation of bibliographic data}
\description{Draw an interactive plot of a bibliography using LDA and Shiny}
\usage{
draw.bib(x)
}
\arguments{
  \item{x}{A list returned by import.bib}
}
\value{
This function launches a Shiny app in the users' default browser. 

The display space is divided into three parts. From left to right, these are the control panel; the plot window; and the selection panel. 

The control window is the only part of the display that is drawn when the app is launched. It consists of a button to calculate LDA, and a series of drop-down menus to alter the apperance of the remaining panels. The default is to suppress most bibliographic information (i.e. data on article authors, years and journals, as well as coauthorship and citation information). Note that the default settings for LDA (5 topics, 1000 iterations) prioritize speed over reliability - higher numbers of iterations will give more reliable results. 

The plot window shows an ordination of article weights calculated using LDA, with articles colored by their highest-weighted topic. Hovering over a point shows the title and abstract below the plot; clicking allows selection or deselection of that article (and optionally displays co-authorship data). Selecting a region of the plot and double-clicking zooms on the selected region; double-clicking without selecting a region returns the plot to its full extent.

The selection panel gives information on progress in selecting/deselecting articles.

Upon completion, the user can export information to a .csv file (saved to the working directory) using the 'export options' tab.
}