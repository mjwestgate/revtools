# function to take a data.frame with bibliographic information, extract useful information, and make a DTM
make_DTM <- function(
  x, # a vector
	stop_words
){
  make_dtm(x, stop_words)
}

make_dtm <- function(
	x,
	stop_words
){

  # check format
  if(class(x) != "character"){
	  stop("make_dtm only accepts arguments of class 'character'")
	}

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

	# create a lookup data.frame
	term <- unlist(lapply(x, function(a){
		result <- strsplit(a, " ")[[1]]
		result <- gsub("^\\s+|\\s+$", "", result)
		return(result[which(result != c(""))])
		}))
	word_freq <- as.data.frame(
    xtabs(~ term),
    stringsAsFactors = FALSE
  )
	word_freq$stem <- tm::stemDocument(word_freq$term)

	# use control in DTM code to do remaining work
	dtm.control <- list(
		wordLengths = c(3, Inf),
		minDocFreq = 5,
		weighting = weightTf
  )
	dtm <- tm::DocumentTermMatrix(
    x = tm::Corpus(
      tm::VectorSource(x_stem)
    ),
    control = dtm.control
  )
	dtm <- tm::removeSparseTerms(
    x = dtm,
    sparse = 0.99
  )
	output <- as.matrix(dtm) # convert back to matrix
	rownames(output) <- paste0(
    "V",
    seq_len(nrow(output))
  )

	# replace stemmed version with most common full word
	term_vec <- unlist(lapply(
    colnames(output),
    function(a, check){
  		if(any(check$stem == a)){
  			rows <- which(check$stem == a)
  			row <- rows[which.max(check$Freq[rows])]
  			return(check$term[row])
  		}else{
        return(a)
      }
		},
    check = word_freq
  ))
	colnames(output) <- term_vec

	return(output)
}