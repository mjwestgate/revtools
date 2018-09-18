\name{fuzz_functions}
\alias{fuzz_functions}
\alias{fuzzdist}
\alias{fuzz_m_ratio}
\alias{fuzz_partial_ratio}
\alias{fuzz_token_sort_ratio}
\alias{fuzz_token_set_ratio}
\title{Functions for fuzzy string matching}
\description{Duplicate of functions from the Python library 'fuzzywuzzy' (\url{https://github.com/seatgeek/fuzzywuzzy}). These functions have been recoded from scratch based on the description given \href{http://chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python}{here}. For consistency with \code{stringdist}, however, these functions are computed as distances rather than similarities; i.e. low values signify similar strings.}
\usage{
fuzzdist(a, b, method)
fuzz_m_ratio(a, b)
fuzz_partial_ratio(a, b)
fuzz_token_sort_ratio(a, b)
fuzz_token_set_ratio(a, b)
}
\arguments{
  \item{a}{a string}
  \item{b}{a vector containing one or more strings}
  \item{method}{a function to be called by 'fuzzdist'}
}
\value{
A score of same length as y, giving the proportional dissimilarity between x and y.
}
\note{
\code{fuzz_m_ratio} is a measure of the number of letters that match between two strings. It is calculated as one minus two times the number of matched characters, divided by the number of characters in both strings.

\code{fuzz_partial_ratio} calculates the extent to which one string is a subset of the other. If one string is a perfect subset, then this will be zero.

\code{fuzz_token_sort_ratio} sorts the words in both strings into alphabetical order, and checks their similarity using \code{fuzz_m_ratio}.

\code{fuzz_token_set_ratio} is similar to \code{fuzz_token_sort_ratio}, but compares both sorted strings to
each other, and to a third group made of words common to both strings. It then returns the maximum value of \code{fuzz_m_ratio} from these comparisons.

\code{fuzzdist} is a wrapper function, for compatability with stringdist.
}
\examples{
fuzz_m_ratio("NEW YORK METS", "NEW YORK MEATS")
fuzz_partial_ratio(
  "YANKEES",
  c("NEW YORK YANKEES", "something else", "YNAKEES")
)
fuzz_token_sort_ratio("New York Mets vs Atlanta Braves", "Atlanta Braves vs New York Melts")
fuzz_token_set_ratio(
  a = "mariners vs angels other words",
  b = c("los angeles angels of anaheim at seattle mariners", "angeles angels of anaheim ")
)
}
