revtools v0.2.3
==========

Tools to support literature review and evidence synthesis in R, including import, de-duplication and interactive display of bibliographic data.

# Example:
```
# install
install.packages("revtools") # from CRAN
devtools::install_github("mjwestgate/revtools") # from GitHub
library(revtools) # load

# import and explore some data
file_location <- system.file("extdata",
  "avian_ecology_bibliography.ris",
  package="revtools")

# run interactive figure with shiny & plotly
screen_visual()
# this app has a drag-and-drop function, so you can add data interactively

# add data from the R workspace
x <- read_bibliography(file_location)
  summary(x) # show number of entries, sources, etc.
  print(x) # show first 5 entries
screen_visual(x) # runs using your data

# to reload a previously saved version
y <- readRDS("saved_object.rds")
screen_visual(y)

```
