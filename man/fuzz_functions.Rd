\name{fuzzywuzzy_functions}
\alias{fuzzywuzzy_functions}
\alias{fuzz_m_ratio}
\alias{fuzz_partial_ratio}
\alias{fuzz_token_sort_ratio}
\alias{fuzz_token_set_ratio}
\title{Functions for fuzzy string matching}
\description{Duplicate of functions from the Python library fuzzywuzzy (\url{https://github.com/seatgeek/fuzzywuzzy}). These functions have been recoded from scratch based on the description given \href{http://chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python}{here}. For consistency with \code{stringdist}, however, these functions are now computed as distances rather than similarities; i.e. low values signify similar strings.}
\usage{
fuzz_m_ratio(x, y)
fuzz_partial_ratio(x, y)
fuzz_token_sort_ratio(x, y)
fuzz_token_set_ratio(x, y)
}
\arguments{
  \item{x}{a string}
  \item{y}{a vector containing one or more strings}
}
\value{
A score of same length as y, giving the proportional dissimilarity between x and y.
}
\examples{
fuzz_m_ratio("NEW YORK METS", "NEW YORK MEATS")
fuzz_partial_ratio(
  "YANKEES",
  c("NEW YORK YANKEES", "something else", "YNAKEES")
)
fuzz_token_sort_ratio("New York Mets vs Atlanta Braves", "Atlanta Braves vs New York Melts")
fuzz_token_set_ratio(
  x = "mariners vs angels other words",
  y = c("los angeles angels of anaheim at seattle mariners", "angeles angels of anaheim ")
)
}
