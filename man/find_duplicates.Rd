\name{find_duplicates}
\alias{find_duplicates}
\title{Locate duplicated information within a data.frame}
\description{Identify potential duplicates within a \code{data.frame}.}
\usage{
find_duplicates(data, match_variable = "title", group_variables = NULL,
  match_function = "fuzzdist", method = "fuzz_m_ratio", threshold = 0.1,
  to_lower = TRUE, remove_punctuation = FALSE)
}
\arguments{
  \item{data}{a \code{data.frame} containing data to be matched}
  \item{match_variable}{a length-1 integer or string listing the column in which duplicates should be sought}
  \item{group_variables}{an optional vector listing the columns to use as grouping variables; that is, categories withing which duplicates should be sought (see 'note'). Optionally NULL to compare all entries against one another.}
  \item{match_function}{a function to calculate dissimilarity between strings. Defaults to \code{fuzzdist}.}
  \item{method}{the required 'method' option that corresponds with match_function. Defaults to \code{fuzz_m_ratio}.}
  \item{threshold}{an upper limit above which similar articles are not recognized as duplicates. Defaults to 0.1.}
  \item{to_lower}{logical: should text be made lower case prior to searching? Defaults to TRUE.}
  \item{remove_punctuation}{logical: should punctuation be removed prior to searching? Defaults to FALSE.}
}
\value{
an integer vector, in which entries with the same integer have been selected as duplicates by the selected algorithm.}
\note{
\code{find_duplicates} runs a \code{while} loop. It starts by checking the first entry of \code{data} against every other entry for potential duplicates. If any matches are found, those entries are excluded from consideration. The loop then continues until all entries have been checked. In order to work, this function requires the \code{data} and \code{match_variable} arguments be specified. The remaining arguments affects how duplicates are identified, and can also strongly influence the speed of the outcome.

The argument \code{group_variables} specifies variables that contain supplementary information that can reduce the number of entries that need to be searched. For example, you might want to only match article titles if they occur within the same journal, or in the same year. The more variables you specify, the fewer pairs of matches that have to be tested to locate duplicates, greatly increasing the speed of the algorithm. Conversely, if no variables are specified, then each entry is checked against every other entry that has yet to be excluded from the dataset. This is fine for small datasets, but massively increases computation time for large datasets.

Missing values are handled differently. Entries that are \code{NA} for \code{match_variable} are always labelled as unique values, and are not checked for duplicates against the rest of the dataset. However, entries of \code{group_variables} that are \code{NA} are included in every comparison.

\code{find_duplicates} contains three 'built-in' methods for string matching. "stringdist" calls the function of the same name from the package \code{stringdist}; ditto for "fuzzdist" which is in \code{revtools}, but based on the Python library \code{fuzzywuzzy}. "exact" simply searches for exact matches. In principle you could call any function for string matching, so long as it accepts the arguments \code{a}, \code{b} and \code{method} (see documentation on stringdist for details), and returns a measure of distance (i.e. not similarity).

Finally, \code{to_lower} and \code{remove_punctuation} specify whether to transform the text prior to searching for duplicates.
}
\seealso{
	\code{\link{screen_duplicates}} and \code{\link{extract_unique_references}} for manual and automated screening (respectively) of results from this function.
}
\examples{
# import data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)

# generate then locate some 'fake' duplicates
x_duplicated <- rbind(x, x[1:5,])
x_check <- find_duplicates(x_duplicated)
# returns a vector of potential matches
}