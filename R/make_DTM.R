# function to take a data.frame with bibliographic information, extract useful information, and make a DTM
make_DTM <- function(
  x, # a vector
	stop_words,
  min_freq = 0.01,
  max_freq = 0.85
){
  make_dtm(x, stop_words, min_freq, max_freq)
}

make_dtm <- function(
	x,
	stop_words,
  min_freq = 0.01,
  max_freq = 0.85
){

  # check format
  if(class(x) != "character"){
	  stop("make_dtm only accepts arguments of class 'character'")
	}
  n <- length(x)

	# sort out stop words
	if(missing(stop_words)){
    stop_words <- revwords()
	}else{
    stop_words <- unique(
      tolower(stop_words)
    )
  }

  # clean up text using tm functions
  x <- tolower(x)
  x <- gsub("-", " ", x) # all dashes (inc intraword dashes)
  x <- tm::removeWords(x, stop_words)
  x <- tm::removePunctuation(x)
  x <- tm::removeNumbers(x)
  x_stem <- stemDocument(x) # requires SnowballC

	# calculate how rare and common terms should be treated
  bound_values <- c(
    max(c(ceiling(n * min_freq), 3)),
    floor(n * max_freq)
  )
  # apply to dataset
	dtm <- tm::DocumentTermMatrix(
    x = tm::Corpus(tm::VectorSource(x_stem)),
    control = list(
      wordLengths = c(3, Inf), # default
      bounds = list(global = bound_values)
    )
  )

  # create a lookup data.frame for words and their stems
  term <- NLP::words(x)
  word_freq <- as.data.frame(
    table(term),
    stringsAsFactors = FALSE
  )
  word_freq$nchar <- nchar(word_freq$term)
  word_freq_list <- split(word_freq, tm::stemDocument(word_freq$term))

  # replace stemmed version with most common full word
  dtm$dimnames$Terms <- unlist(lapply(
    dtm$dimnames$Terms,
    function(a, lookup){
      z <- lookup[[a]]
      z <- z[order(z$nchar), ]
      z <- z[order(z$Freq, decreasing = TRUE), ]
      return(z$term[order(z$Freq, z$nchar, decreasing = TRUE)[1]])
    },
    lookup = word_freq_list
  ))

  output <- as.matrix(dtm) # convert back to matrix
  rownames(output) <- paste0(
    "V",
    seq_len(nrow(output))
  )

	return(output)
}