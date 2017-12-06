revtools
==========

Tools to support literature review and evidence synthesis in R, including import, de-duplication and interactive display of bibliographic data.

# Example:
```
# install and load package
library(devtools)
install_github("mjwestgate/revtools")
library(revtools)

x<-read_bibliography("example_bibliography.ris")  # import
summary(x) # show number of entries, sources, etc.
print(x) # show first 5 entries

start_review_window(x) # run interactive figure with Shiny

# to reload a previously saved version
y<-readRDS("saved_object.rds")
start_review_window(y)

```
