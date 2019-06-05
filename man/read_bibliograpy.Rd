\name{read_bibliography}
\alias{read_bibliography}
\title{Import bibliographic data}
\description{Import standard formats from academic search engines and referencing software.}
\usage{read_bibliography(filename, return_df = TRUE)}
\arguments{
  \item{filename}{A vector or list containing paths to one or more bibliographic files.}
  \item{return_df}{logical; should the object returned be a data.frame? Defaults to TRUE. If FALSE, returns an object of class \code{bibliography}.}
}
\details{
This function aims to import bibliographic data from a range of formats in a consistent manner.

If the user provides a number of paths in a vector or list, then files are imported in sequence and joined into a single \code{data.frame} (or a \code{list} if \code{return_df = FALSE}) using \code{\link{merge_columns}}. If \code{return_df = TRUE} (the default) then an extra column called 'file_name' is appended to the resulting \code{data.frame} to show the file in which each entry originated.

If the file is in .csv format, then \code{read_bibliography} will import the file using \code{read.csv}, with three changes. First, it ensures that the first column contains an index (i.e. a unique value for each row), and creates one if it is absent. Second, it converts column names to lower case and switches all delimiters to underscores. Third, it ensures that author names are delimited by 'and' for consistency with \code{format_citation}.

If the file is of any type other than .csv, \code{read_bibliography} auto-detects document formatting, first by detecting whether the document is ris-like or bib-like, and then running an appropriate import function depending on the result. In the case of ris-like files (including files from 'Endnote' & 'Web of Science'), this involves attempting to detect both the delimiter between successive entries, and the means of separating tag labels (e.g. 'AU', 'TI') from their information content. Attempts have been made to ensure consistency with .ris, .bib, medline (.nbib) or web of science (.ciw) formats. Except for .csv, file extensions are not used to determine file type, and are ignored except to locate the file.

If the imported file is in a ris-like format, then the object returned by \code{read_bibliography} will have different headings from the source document. This feature attempts to ensure consistency across file types. Tag substitutions are made using a lookup table, which can be viewed by calling \code{\link{tag_lookup}}. Unrecognized tags are grouped in the resulting \code{bibliography} object under the heading 'further_info'.
}
\value{Returns an object of class \code{data.frame} if \code{return_df} is \code{TRUE}; otherwise an object of class \code{bibliography}.}
\seealso{
	\code{\link{bibliography-class}}, \code{\link{tag_lookup}}
}
\examples{
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location)
class(x) # = data.frame
x <- read_bibliography(file_location, return_df = FALSE)
class(x) # = bibliography
summary(x)
}