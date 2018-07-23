# function to take a data.frame with bibliographic information, extract useful information, and make a DTM
make_DTM<-function(
	x, # a vector
	stop_words
	){

  # check format
  if(class(x)!="character"){
	  stop("make_DTM only accepts arguments of class 'character'")
	}

	# sort out stop words
	if(missing(stop_words)){
    stop_words <-tm::stopwords()
	}else{
    stop_words <-unique(c(tm::stopwords(), tolower(stop_words)))
  }

	# convert to document term matrix
	corp <- tm::Corpus(tm::VectorSource(x))
		corp <- tm::tm_map(corp, content_transformer(tolower))
		corp <- tm::tm_map(corp, removePunctuation)
		corp <- tm::tm_map(corp, removeWords, stop_words)
		corp <- tm::tm_map(corp, removeNumbers)
		stem.corp <- tm::tm_map(corp, stemDocument) # SnowballC

	# create a lookup data.frame
	term<-unlist(lapply(as.list(corp), function(a){
		result<-strsplit(a, " ")[[1]]
		result <-gsub("^\\s+|\\s+$", "", result)
		return(result[which(result!=c(""))])
		}))
	word_freq<-as.data.frame(xtabs(~ term), stringsAsFactors=FALSE)
	word_freq$stem<-tm::stemDocument(word_freq$term)

	# use control in DTM code to do remaining work
	dtm.control <- list(
		wordLengths = c(3, Inf),
		minDocFreq=5,
		weighting = weightTf)
	dtm<-tm::DocumentTermMatrix(stem.corp , control= dtm.control)
	dtm<-tm::removeSparseTerms(dtm, sparse= 0.99) # remove rare terms (cols)
	output<-as.matrix(dtm) # convert back to matrix
	rownames(output)<-paste0("V", c(1:nrow(output)))

	# replace stemmed version with most common full word
	term_vec<-unlist(lapply(colnames(output), function(a, check){
		if(any(check$stem==a)){
			rows<-which(check$stem==a)
			row<-rows[which.max(check$Freq[rows])]
			return(check$term[row])
		}else{return(a)}
		}, check=word_freq))
	colnames(output)<-term_vec

	return(output)
}