revtools
==========

Tools to support systematic review and meta-analysis in R

Note: This package includes tools to import, de-duplicate, and display bibliographic data. It is in the early stages of development, so use at your own risk!

# Example:
```
# install and load package
library(devtools)
install_github("mjwestgate/revtools")
library(revtools)

x<-import.bib("example_bibliography.ris")  # import
summary(x) # show number of entries, sources, etc.
print(x) # show first 5 entries

draw.bib(x) # run interactive figure with Shiny

# to reload a previously saved version
y<-readRDS("saved_object.rds")
draw.bib(y)

```
