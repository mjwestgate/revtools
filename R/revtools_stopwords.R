#' Load a set of stopwords
#'
#' Generates a list of stopwords, consisting of all the terms given by
#' \code{tm::stopwords}, plus some extra terms (mainly words that designate
#' numbers).
#'
#'
#' @return A vector of stopwords in English.
#' @note This is primarily an internal function, but may be useful in other
#' contexts.
#' @examples
#'
#' # import some data
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location)
#'
#' # construct a document-term matrix
#' x_dtm <- make_dtm(x$title,
#'   stop_words = revwords())
#' # Note that make_dtm calls revwords by default, so this is technically redundant
#'
#' @export revwords

revwords <- function(){
  c(
    tm::stopwords(),
    # add words for numbers
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
    "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety",
    "hundred", "thousand", "million", "billion", "trillion",
    # add different endings
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth",
    "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth",
    "sixteenth", "seventeenth", "eighteenth", "nineteenth",
    "twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth",
    "seventieth", "eightieth", "ninetieth",
    "hundredth", "thousandth", "millionth", "billionth"
  )
}