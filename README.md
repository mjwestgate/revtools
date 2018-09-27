<img align="left" height="120" src="https://github.com/mjwestgate/revtools_website/blob/master/assets/img/revtools_hex.png"><b>revtools v0.3.0</b>

Tools to support literature review and evidence synthesis in R, including import, de-duplication and interactive display of bibliographic data.

For a complete introduction to revtools you can check out the [user manual](https://revtools.net/user_manual/1_introduction.html); but to get started now you can download revtools either from this site (development version) or CRAN (stable version) as follows:
```
# install
install.packages("revtools") # from CRAN
# OR
devtools::install_github("mjwestgate/revtools") # from GitHub

# load
library(revtools)
```

Once you've installed & loaded revtools, you can use any of the inbuilt apps by loading them and drag-and-dropping in the data you want to analyse. All the apps export to csv format so you don't need to use R to investigate their results if you'd prefer not to. The apps available in revtools are:

- <code>screen_duplicates()</code> to look for duplicates within a dataset
- <code>screen_titles()</code> to screen articles by title
- <code>screen_abstracts()</code> to screen articles by abstract
- <code>screen_topics()</code> to run topic models on bibliographic data

I've you're a keen to investigate your data a bit more deeply, revtools is designed to make data import as straightforward as possible. It does this by using a single function to import bibliographic data from bib, ris, ciw or csv formats:

```
file_location <- system.file("extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")

# to import bibliographic information into a data.frame
data <- read_bibliography(file_location)
```

Then you can pass these data to your apps as you would with any other function:
```
screen_topics(data) # runs using your data

# to reload a previously saved version
y <- readRDS("saved_object.rds")
screen_topics(y)
```