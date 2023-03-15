---
title: revtools
description: Tools for Evidence Synthesis in R
---

<h1>Tools for Evidence Synthesis</h1>

<img class="hex" src="man/figures/revtools_hex.png">  `revtools` is an R package to support researchers working on evidence synthesis projects. It provides a free, easy-to-use, open-source environment to conduct your literature review or meta-analysis. You can use it to visualise patterns in bibliographic data, interactively select or exclude individual articles or words, and save the results for later analysis.

You can download `revtools` from <a href="https://cran.r-project.org/package=revtools" target="_blank" rel="noopener">CRAN</a>, view the development version on <a href="https://github.com/mjwestgate/revtools" target="_blank" rel="noopener">GitHub</a>, or read about it in this paper:

> <font color="#6e6e6e">MJ Westgate (2019) revtools: An R package to support article screening for evidence synthesis. <i>Research Synthesis Methods</i>  <a href="http://doi.org/10.1002/jrsm.1374" target="_blank" rel="noopener">http://doi.org/10.1002/jrsm.1374</a></font><div data-badge-popover="right" data-badge-type="2" data-doi="10.1002/jrsm.1374" data-hide-no-mentions="true" class="altmetric-embed"></div>

<iframe
  id="revtools_example"
  src="reference/figures/revtools_example.html"
  width="100%"
  height="600px"
  >
  You should see some plots here!
</iframe>
<font color="#999999"><i>Simplified example of article visualisation (left) and topic counts (right) as displayed by the `screen_topics()` function in revtools.</i></font>
<br>


## What is evidence synthesis?
Evidence synthesis is the process of searching, collating and interpreting scientific information. Of course, this definition covers a whole range of possible  activities, from simple literature searches on a single topic or question, right through to a full systematic review or meta-analysis. Whatever their goal, however, many researchers are finding it increasingly difficult to keep track of the amount of literature that they must read to gain an overview of a given research field. This is where `revtools` can help.

In practical terms, `revtools` provides some simple functions for importing and managing 'bibliographic' data; i.e. lists of books or articles, when they were published, and who they were authored and published by. Rather than just import these data, however, revtools also includes a number of user interfaces to help you interactively sort and categorize that content. These functions run in your browser, and support a range of tasks from manually sorting article titles or abstracts, right through to automated cluster identification using topic models. By default, each interface hides identifying information of each article, meaning that revtools is consistent with the screening guidelines for systematic reviews given by <a href="https://www.cochrane.org" target="_blank" rel="noopener">Cochrane</a>, the <a href="https://campbellcollaboration.org" target="_blank" rel="noopener">Campbell Collaboration</a>, or the <a href="http://www.environmentalevidence.org" target="_blank" rel="noopener">Collaboration for Environmental Evidence</a>.

This site gives you some basic information on how to import, de-duplicate, and screen articles using `revtools`.

## Installation
From R, you can install the current CRAN version of `revtools` using this code:
```
install.packages("revtools")
library(revtools)
```

Alternatively, you can install from GitHub using this code:
```
install.packages("remotes")
remotes::install_github("mjwestgate/revtools")
library(revtools)
```

<iframe
  id="revtools_downloads"
  src="reference/figures/revtools_downloads.html"
  width="100%"
  height="450px"
  >
  You should see some plots here!
</iframe>
<font color="#999999"><i>Cumulative downloads of `revtools` since its' release 
in February 2018.</i>