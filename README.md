bibviewr
==========

Import and interactive display tools for bibliographic data

# Example:
```
# install and load package
library(devtools)
install_github("mjwestgate/bibviewr")
library(bibviewr)

x<-import.bib("example_bibliography.ris")  # import
summary(x) # show number of entries, sources, etc.
print(x) # show first 5 entries

draw.bib(x) # run interactive figure with Shiny

# to reload a previously saved version
y<-readRDS("saved_object.rds")
draw.bib(y)

```
