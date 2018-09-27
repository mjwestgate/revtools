\name{tag_lookup}
\alias{tag_lookup}
\title{Lookup table for ris tags}
\description{ris-like bibliographic data files contain codes that describe their contents, such as 'AU' in place of 'author'. This function provides lookup tables for 'ris' tags of different kinds}
\usage{tag_lookup(type = "ris")}
\arguments{
  \item{type}{Which lookup table should be returned? Accepted values are 'ris', 'ris_write' or 'medline'}
}
\value{a \code{data.frame} containing the original tag (column 'ris'), and the full-word substitution for that tag (column 'bib'). For type 'ris', there is also an added 'order' column showing the order those tags should be displayed in. 'ris_write' is a version of 'ris' with only one ris tag per bib tag.}
\details{Primarily an internal function to support \code{read_bibliography} and \code{write_bibliography}. Tag substitutions for PubMed/Medline fields are taken directly from the NIH (\href{https://www.nlm.nih.gov/bsd/mms/medlineelements.html}{available here}). Substitutions for other ris-like formats are based on common examples, but are much less consistently documented.
}
\seealso{
	\code{\link{bibliography-class}}, \code{\link{read_bibliography}}
}
\examples{
tag_lookup("ris") # standard ris format
tag_lookup("medline") # PubMed files
}