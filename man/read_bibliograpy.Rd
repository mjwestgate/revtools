\name{read_bibliography}
\alias{read_bibliography}
\title{Import bibliographic data}
\description{Import standard formats from academic search engines and referencing software.}
\usage{read_bibliography(x)}
\arguments{
  \item{x}{Path to a bibliographic file}
}
\value{Returns an object of class \code{bibliography}}
\details{
This function auto-detects document formatting, first by detecting whether the document is ris-like or bib-like, and then running an appropriate import function depending on the result. In the case of ris-like files (including files from 'Endnote' & 'Web of Science'), this involves attempting to detect both the delimiter between successive entries, and the means of separating tag labels (e.g. 'AU', 'TI') from their information content. Attempts have been made to ensure consistency with .ris, .bib, medline (.nbib) or web of science (.ciw) formats. File extensions are not used to determine file type, and are ignored except to locate the file.

If the imported file is not in bib format, then the object returned by \code{read_bibliography} will have different headings from the source document. This feature attempts to ensure consistency across file types. Tag substitutions are made using a lookup table, which can be viewed by calling \code{\link{tag_lookup()}}. Unrecognized tags are grouped in the resulting \code{bibliography} object under the heading 'further_info'.
}
\seealso{
	\code{\link{bibliography-class}}, \code{\link{tag_lookup}}
}
\examples{
  file_location <- system.file(
    "extdata",
    "avian_ecology_bibliography.ris",
    package = "revtools")
x <- read_bibliography(file_location)
summary(x)
}