# function to take a data.frame with bibliographic information, extract useful information, and make a DTM


#' Construct a document-term matrix (DTM)
#'
#' Takes bibliographic data and converts it to a DTM for passing to topic
#' models.
#'
#' Formerly a unique function, but now a wrapper to code{quanteda} functions. Maintained for backward-compatability.
#'
#' @param x a vector or \code{data.frame} containing text
#' @param stop_words optional vector of strings, listing terms to be removed
#' from the DTM prior to analysis. Defaults to \code{revwords()}.
#' @param min_freq minimum proportion of entries that a term must be found in
#' to be retained in the analysis. Defaults to 0.01.
#' @param max_freq maximum proportion of entries that a term must be found in
#' to be retained in the analysis. Defaults to 0.85.
# @param meta: Optional list passed to quanteda::corpus 'meta' argument.
#' @param verbose logical: should details be printed to the screen? Defaults to FALSE
#' @return A list as described in \code{link{stm::prepDocuments}}
#' @seealso \code{\link{run_topic_model}}, \code{\link{screen_topics}}
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
#' # note: this can take a long time to run for large datasets
#' x_dtm <- make_dtm(x$title)
#'
#' # to return a matrix where length(output) == nrow(x)
#' x_dtm <- make_dtm(x$title, retain_empty_rows = TRUE)
#' x_dtm <- as.matrix(x_dtm) # to convert to class 'matrix'
#' dim(x_dtm) # 20 articles, 10 words
#'
#' @export make_dtm

make_dtm <- function(
	x,
	stop_words,
  min_freq = 0.01,
  max_freq = 0.85,
  # meta = NULL
  verbose = FALSE # no longer functional
){

  # check format
  if(!(class(x) %in% c("character", "data.frame"))){
	  stop("make_dtm only accepts arguments of class 'data.frame' or 'character'")
	}
  if(class(x) == "data.frame"){
    if(ncol(x) < 2){
      x <- as.character(x[, 1])
    }else{
      x <- apply(x, 1, function(a){paste(a, collapse = " ")})
    }
  }
  n <- length(x)

  # # sort out stop words
  if(missing(stop_words)){
    stop_words <- revwords()
  }else{
    stop_words <- unique(tolower(stop_words))
  }

  # new approach using quanteda
  # use quanteda tokeniser to get rid of basic stuff like punctuation
  # if(is.null(meta)){
     x_corpus <- quanteda::corpus(x)
  # }else{
  #   x_corpus <- quanteda::corpus(x, meta = meta)
  # }

  x_tokens <- quanteda::tokens(x_corpus,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    split_hyphens = TRUE)
  # remove short words
  x_tokens <- quanteda::tokens_select(x_tokens, min_nchar = 4)
  # remove words that contain numbers or punctuation
  x_tokens <- quanteda::tokens_remove(x_tokens,
    pattern = "[[:digit:]]+|[[:punct:]]+", valuetype = "regex")
  # convert to dfm
  x_dfm <- quanteda::dfm(x_tokens, stem = TRUE, remove = stop_words)
    # x_dfm@Dimnames$features[1:40] # check
  # remove rare terms
  x_dfm_small <- quanteda::dfm_trim(x_dfm,
    min_termfreq = min_freq * n,
    max_termfreq = max_freq * n,
    termfreq_type = "count") # proportions doesn't work for some reason
  # remove empty documents
  x_dfm_small <- quanteda::dfm_subset(x_dfm_small, quanteda::ntoken(x_dfm_small) > 0)

  # previous approach based solely on {stm}
  # x_processed <- stm::textProcessor(x,
  #   customstopwords = stop_words,
  #   verbose = verbose,
  #   ...)
  # dtm <- stm::prepDocuments(
  #   documents = x_processed$documents,
  #   vocab = x_processed$vocab,
  #   lower.thresh = min_freq * n,
  #   upper.thresh = max_freq * n,
  #   verbose = verbose)

  return(x_dfm_small)

}
