# Version 0.4.1

### Improvements to apps
 - Clicking on topics in `screen_topics()` highlights all points in that topic, and renders points from non-selected topics as grey with low opacity
 - New 'processing' tab in `screen_topics()`  allows manipulation of arguments passed to `make_dtm()`
 - Apps that record screening information now do so in a unique column starting with `selected_`, allowing users to determine at which stage articles were excluded during the screening process. New column names are as follows:`screen_titles` stores data in `selected_titles`, `screen_abstracts` stores data in `selected_abstracts`, and `screen_topics` stores data in `selected_topics`
 - All apps now use the same naming convention for selection/exclusion (previously `screen_topics()`returned `TRUE`/`FALSE`)
 - `screen_abstracts()` and `screen_titles()` no longer add 'color' or 'order' columns to exported objects
 - `screen_abstracts` automatically moves to the next reference when a selection is made, and hides screened articles by default
 
### Improvements to `make_dtm`
 - Now accepts objects of class `data.frame` as an input
 - Flexible bigram detection c/o package 'ngram'
 - Chooses words for display by number of characters, rather than frequency of occurrence in the dataset
 - Allows user-controlled removal (the default) or retention of empty rows

### Processing functions
 - `read_bibliography()` now recognises `ris` tags from Web of Science (`.ciw` format)
 - `add_line_breaks()` now available as a standalone function, as well as being an argument in `format_citation()`
 - objects returned by `find_duplicates()` now include information on the call that created them (via `attributes`)

### Changed defaults
 - `find_duplicates()` now searches for exact matches of DOIs by default, or fuzzy matching of article titles if DOIs are missing
 - `make_dtm()` now returns an object of `slam::simple_triplet_matrix()` rather than `matrix()`. This can be coerced to a matrix by `as.matrix()`

### Bug fixes
 - `merge_columns()` should return columns in the same order regardless of order that objects are provided
 - Importing multiple files at once no longer generates non-unique 'label' entries, which could cause failure of article citations in `screen_topics`.
 - csv files now import with correct text encoding using `read_bibliography()`
 - `screen_duplicates()` loads data.frames correctly from the command line
 - data.frames returned by `read_bibliography()` always have snake case column names  
 - `make_dtm()` now removes all words of three letters or fewer
 - Improved behaviour of `format_citation()`

In other news, this is likely to be the last version of `revtools` to contain 
it's own import code, which is being outsourced to package `synthesisr`. The 
function `read_bibliography()` will be retained, but as a wrapper to code from 
`synthesisr`. More to follow.


# Version 0.4.0

There are four major changes in this version.

### New functions to support teamwork
Version 0.4.0 introduces three new functions for working with a team. This will help people who want to run systematic reviews as a group, particularly if you want to customize the amount of overlap between reviewers or have different reviewers screen different numbers of articles. These functions are:
 - `allocate_effort()` determines what proportion of articles are given to which reviewers
 - `distribute_tasks()` splits a dataset into sub-sections for each reviewer
 - `aggregate_tasks()` re-joins results from different reviewers


### Updated displays
There have been several updates to how data is displayed in revtools shiny apps:

 - all have a `max_file_size` argument allowing the user to upload larger files than previously
 - all apps now accept drag-and-drop of multiple files simultaneously
 - `screen_titles()` and `screen_abstracts()` now allow user-defined sorting (by choosing a column to sort by)
 - accept/reject, navigation buttons, and progress indicators have been relocated to the header bar
 - `screen_abstracts()` now allows navigation by +/- 10 articles

### Better importing
`read_bibliography()` has had some changes to improve functionality and versatility. The biggest change is that this function can now accept a list of file paths and  return a single object (by default this is a `data.frame`). It also will no longer group unknown tags into a single column labelled 'further_info'; instead it will import these tags as columns without changing their labels. This makes for larger data frames with less interpretable names, but is more true to the nature of the source data.

### Bug fix: stopword removal
Version 0.4.0 fixes a bug in the code passed to the `tm` library when building document-term matrices with `make_dtm()`. Specifically, the section of the code that removed rare terms was non-functional, leading to articles containing rare terms to drift towards the extremes of the main scatterplot in `screen_topics()`. This has now been corrected and supplemented with an option to also exclude very common terms. Consequently, if you re-run your analyses you should see a more even distribution of points with fewer extreme values. If you're feeling adventurous, you might wish to consider adjusting the upper and lower thresholds for rare/common term exclusion and see what effect it has on the resulting models, as preliminary testing suggests adjusting the defaults can improve fit in some cases.


# Version 0.3.0

This is a big change from the previous version, and so while there are many 
useful new features, there are also some changes to the API. Consequently, you 
should be careful about upgrading if you are using `revtools` for an ongoing 
project. *Most importantly, the save function in `start_review_window()` is not 
reverse-compatible*, so updating will mean you lose the ability to reload your 
progress from earlier versions.

### Overview
 - new colour scheme and logo for shiny apps
 - new apps for screening duplicates, titles or abstracts
 - import functions always return a `data.frame` by default
 - more versatile duplicate detection, including new string matching algorithms
 - improved topic model control to allow users to interactively select included text

### New functions
 - apps: `screen_titles()`, `screen_abstracts()` & `screen_duplicates()`
 - `fuzzdist()`: Fuzzy string matching based on the Python library `Fuzzywuzzy`
 - `revwords()`: Stopwords, but including words that describe numbers

### API changes
Old function names have been retained where possible, but may be deleted from 
later versions.

 - all function names are now lower case: `make_DTM()` is now `make_dtm()`; `run_LDA` is now `run_topic_model()`.
 - `start_review_window()` is now `screen_topics()` for consistency with other apps
 - `read_bibliography()` now returns a `data.frame` by default, but returns an object of class `bibliography` if `return_df = FALSE`. This function also accepts `.csv` files now.
 - arguments to `find_duplicates()` are different
 - `extract_unique_references` returns a `vector`, not a `data.frame`


# Version 0.2.2

### Updates
 - `revtools` now uses modals to show when new topic models are being calculated, or when results are saved. This is a marked improvement that lets the user know when the GUI is temporarily inoperative.
 - Cleaner import of `.ris` files.
 - New argument `remove_words` allows the user to exclude specific words from the topic model when calling `start_review_window()`. The default is to use `tm::stopwords` (this was true in v. 0.2.1 as well), so there is no need to add basic stop words each time.
 
### Bug fixes
 - Interactively excluding words in `start_review_window()` then running a topic model used to cause mismatches between hover and selected text - this has been fixed.

### Changed behaviour
 - `revtools` no longer installs the `SnowballC` package by default, so if you don't have it installed already, you'll now be prompted to install it whenever you run either of the functions `make_DTM()` or `start_review_window()`.


# Version 0.2.1

Today, version 0.2.1 of revtools was accepted by CRAN, meaning you can download 
it directly from R `install.packages("revtools")`

This is quite an exciting day for me, as it's taken 21 months of work to get to 
this point. To be specific, I put the first 1090 lines of revtools code (then 
called 'bibviewr') on GitHub on 12th May 2016. Admittedly, I haven't been working 
on it full-time, but it feels like an achievement just the same.

Of course, just because revtools is on CRAN doesn't mean that it's finished. 
I've got a bunch of ideas about what features to add next, which I'll be adding 
to the 'issues' section of the GitHub page in the next couple of weeks. You can 
do the same by following 
<a href="https://github.com/mjwestgate/revtools/issues" target="_blank" rel="noopener">this link</a>, 
or by dropping me an <a href="mailto:martinjwestgate@gmail.com">email</a> about 
features you'd like added. I can't make any guarantees that I'll be able to 
implement them all, but I'll do my best!
  
You can visit the CRAN homepage for revtools 
<a href="https://cran.r-project.org/package=revtools" target="_blank" rel="noopener">here</a>. 
Thanks to everyone who helped along the way.