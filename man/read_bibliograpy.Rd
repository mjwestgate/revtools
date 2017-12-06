\name{read_bibliography}
\alias{read_bibliography}
\title{Import bibliographic data}
\description{Import standard formats from academic search engines and referencing software to R.}
\usage{read_bibliography(x, path)}
\arguments{
  \item{x}{Filename of a bibliographic file. Supported formats include .ris, .bib, medline (.nbib) or web of science (.ciw)}
  \item{path}{Path to specified file}
}
\value{Returns an object of class 'bibliography', which is a list where each entry is a list containing data on each reference. This function auto-detects document formatting, meaning that specified file formats are ignored except to locate the file.}
\examples{
# import data
x<-read_bibliography("example_bibliography.ris")

# view basic info on x
summary(x)
print(x)
}