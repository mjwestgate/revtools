\name{read_bibliography}
\alias{read_bibliography}
\title{Import bibliographic data}
\description{Import standard formats from academic search engines and referencing software.}
\usage{read_bibliography(x, path)}
\arguments{
  \item{x}{Filename of a bibliographic file. Supported formats include .ris, .bib, medline (.nbib) or web of science (.ciw)}
  \item{path}{Path to specified file}
}
\value{Returns an object of class \code{bibliography}, which is a list where each entry is a list containing data on each reference. This function auto-detects document formatting, meaning that specified file formats are ignored except to locate the file.}
\examples{
file_location<-system.file("extdata", "avian_ecology_bibliography.ris", package="revtools")
x<-read_bibliography(file_location)
summary(x)
}