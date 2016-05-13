\name{import.bib}
\title{Import bibliographic data}
\description{
Import standard formats from academic search engines and referencing software to R.
}
\usage{
import.bib(x)
}
\arguments{
  \item{x}{Filename of a bibliographic file in .ciw (Web of Science/Knowledge) or .ris format. Other formats to follow}
}
\value{
Returns a list, where each entry is a reference, containing a sublist of standard data on that reference (author, year, etc).
}
