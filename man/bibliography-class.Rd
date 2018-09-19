\name{bibliography-class}
\title{Description of class 'bibliography'}
\alias{bibliography-class}
\description{An S3 class designed to store data from common bibliographic formats in a standard way}
\details{Class 'bibliography' is a nested list format; each object is a list containing multiple references, where each reference is a list with information on author, journal etc. Each entry has a unique name that is preserved in the 'label' column if the user subsequently calls \code{as.data.frame}.

}
\seealso{
	\code{\link{read_bibliography}}, \code{\link{write_bibliography}}
}