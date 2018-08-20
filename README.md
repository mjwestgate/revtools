<img src="https://github.com/mjwestgate/revtools_website/blob/master/assets/img/revtools_hex.png",
alt="revtools logo"
width="150">
## revtools v0.3.0 (in development)

Tools to support literature review and evidence synthesis in R, including import, de-duplication and interactive display of bibliographic data.

# Example
```
# install
install.packages("revtools") # from CRAN
devtools::install_github("mjwestgate/revtools") # from GitHub
library(revtools) # load

# import and explore some data
file_location <- system.file("extdata",
  "avian_ecology_bibliography.ris",
  package="revtools")

# screen articles manually, using only their titles
screen_titles()

# screen articles manually, using titles and abstracts
screen_abstracts()

# run interactive figure with shiny & plotly
screen_topics()
# this app has a drag-and-drop function, so you can add data interactively

# add data from the R workspace
x <- read_bibliography(file_location)
  summary(x) # show number of entries, sources, etc.
  print(x) # show first 5 entries
screen_topics(x) # runs using your data

# to reload a previously saved version
y <- readRDS("saved_object.rds")
screen_topics(y)

```
