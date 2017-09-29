revtools
==========

Tools to support literature review and evidence synthesis in R.

Note: This package includes tools to import, de-duplicate, and display bibliographic data. It is in the early stages of development, so use at your own risk!

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
