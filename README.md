bibviewr
==========

Import and interactive display tools for bibliographic data

# Example:
```
# install and load package
library(devtools)
install_github("mjwestgate/bibviewr")
library(bibviewr)

bib.data<-import.bib("example_bibliography.ris")  # import

# write custom list of stopwords (optional)
to.remove<-sort(unique(stopwords("english"),
    c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",	
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "use", "can", "may", "high", "higher", "low", "lower", 
    "also", "howev", "show", "found", "due") 
    ))
bib.list<-prep.bib(bib.data, to.remove) # generate data in correct format - SLOW
# creates a list with three entries:
    # $bibliography - copy of bib.data
    # $dtm - document term matrix
    # $coauthors - list of pairs of articles that share >0 authors

draw.bib(bib.list) # run interactive figure with Shiny
```
