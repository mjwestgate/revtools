\name{bibliography-methods}
\title{Methods for class 'bibliography'}
\alias{as.bibliography}
\alias{c.bibliography}
\alias{as.data.frame.bibliography}
\alias{print.bibliography}
\alias{summary.bibliography}
\alias{[.bibliography}
\alias{bibliography-methods}
\description{This is a small number of standard methods for interacting with class 'bibliography'. More may be added later.}
\usage{
as.bibliography(x, ...)
	\method{as.data.frame}{bibliography}(x, ...)
	\method{[}{bibliography}(x, n)
	\method{c}{bibliography}(...)
	\method{print}{bibliography}(x, n, ...)
	\method{summary}{bibliography}(object, ...)
}
\arguments{
  \item{x}{An object of class 'bibliography'}
  \item{object}{An object of class 'bibliography'}
  \item{n}{Number of items to select/print}
  \item{...}{Any further information}
}
\examples{
# import some data
file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
x <- read_bibliography(file_location, return_df = FALSE)

# basic descriptions
summary(x)
print(x)
x[1]

# append two bibliography objects
y <- c(x[1:5], x[2:4])

# conversion to and from data.frame
y <- as.data.frame(x)
x_new <- as.bibliography(y)
}