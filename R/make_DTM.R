# function to take a data.frame with bibliographic information, extract useful information, and make a DTM
make_DTM<-function(
	x, # an object of class data.frame
	stop_words
	){

	# check format
	if(class(x)=="bibliography"){x<-as.data.frame(x)}

	# collate data into a single vector
	text_cols<-c("title", "keywords", "abstract")
	available_cols<-which(text_cols %in% colnames(x))
	if(any(available_cols)){
		x_textonly<-x[, text_cols[available_cols]]
	}else{
		stop("no titles, keywords or abstracts available in selected object")
	}

	text_vector<-unlist(lapply(split(x_textonly, c(1:nrow(x))), function(a){
		if(all(is.na(a))){
			return("")
		}else{
			paste(a[which(is.na(a)==FALSE)], collapse=" ")
			}
		}))

	# sort out stop words
	if(missing(stop_words)){stop_words <-tm::stopwords()
	}else{stop_words <-unique(c(tm::stopwords(), stop_words))}

	# convert to document term matrix
	corp <- tm::Corpus(tm::VectorSource(text_vector))
		corp <- tm::tm_map(corp, content_transformer(tolower))
		corp <- tm::tm_map(corp, removePunctuation)
		corp <- tm::tm_map(corp, removeWords, stop_words)
		corp <- tm::tm_map(corp, removeNumbers)
		stem.corp <- tm::tm_map(corp, stemDocument)

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
	rownames(output)<-x$ID

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