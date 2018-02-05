revtools
==========

Tools to support literature review and evidence synthesis in R, including import, de-duplication and interactive display of bibliographic data.

# Example:
```
# install and load package
library(devtools)
install_github("mjwestgate/revtools")
library(revtools)

# import and explore some data
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-read_bibliography(file_location)
summary(x) # show number of entries, sources, etc.
print(x) # show first 5 entries

start_review_window(x) # run interactive figure with shiny & plotly

# to reload a previously saved version
y<-readRDS("saved_object.rds")
start_review_window(y)

```
