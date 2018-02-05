\name{bibliography-class}
\title{Description of class 'bibliography'}
\alias{bibliography-class}
\description{Class 'bibliography' is an S3 class designed to store data from common bibliographic formats in a standard way. It is a nested list format; each object is a list containing multiple references, where each reference is a list with information on author, journal etc. Because different formats code their information differently, class bibliography uses bib-like headings to ensure that reference names give a meaningful description of its' content (i.e. 'author' instead of 'AU'). This means that an .ris or medline and pubmed files have their tags converted on import by \code{read_bibliography}, while .bib tags are not altered.}
\section{slots}{
Class 'bibliography' will import all information given in a file, and will attempt to assign it to a sensible heading. Possible entry names are (in this order):
	\itemize{
		\item \strong{type} tag 'TY'
		\item \strong{author} tags 'AU' or 'A' followed by 1:5
		\item \strong{year} tags 'PY'or 'Y1'
		\item \strong{title} tags 'TI' or 'T1'
		\item \strong{journal} tags 'JO', 'T2', 'T3', 'SO', 'JT', 'JF' or 'JA'
		\item \strong{volume} tag 'VL'
		\item \strong{issue} tag 'IS'
		\item \strong{pages} one or more of tags 'EP', 'BP' or 'SP'
		\item \strong{abstract} tags 'AB' or 'N2'
		\item \strong{keywords} tags 'KW' or 'DE'
		\item \strong{doi} tag 'DO'
		\item \strong{call} tag 'CN'
		\item \strong{issn} tag 'SN'
		\item \strong{url} tag 'UR'
		\item \strong{accession} tag 'AN'
		\item \strong{institution} tag 'CY'
		\item \strong{publisher} tag 'PB'
		\item \strong{pubplace} tag 'PP'
		\item \strong{address} tag 'AD'
		\item \strong{editor} tag 'ED'
		\item \strong{edition} tag 'ET'
		\item \strong{language} tag 'LA'
		\item \strong{further_info} any unallocated information
	}		
}