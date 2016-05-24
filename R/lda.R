
# function for cleaning abstracts for text analysis
extract.abstracts<-function(x){ # list of data from an import function
	# extract relevant data from the list
	y<-lapply(x, function(a){a$AB})

	# are any abstracts completely missing? 
	null.check<-lapply(y, is.null)
	if(any(unlist(null.check))){y[which(unlist(null.check))]<-""}

	# remove copyright notices
	# ISI lists copyright as (C)
	y <-lapply(y, function(a){strsplit(a, " \\(C\\)")[[1]][1]}) 
	# Scopus uses the actual 'copyright' symbol \u00a9
	y <-lapply(y, function(a){
		raw.vals<-charToRaw(a)
		if(any(raw.vals =="a9")){
			final.char<-sort(which(raw.vals=="a9"))[1]-3
			return(rawToChar(raw.vals[c(1:final.char)]))
		}else{return(a)}})

	# stop hyphenated words being joined
	y<-lapply(y, function(a){gsub("-", " ", a)})

	# some articles have added words in their abstracts - remove them
	rm.words<-c("Abstract: ", "Aim: ", "Aims: ", "Background: ", 
		"Capsule: ", "Context: ", "Conclusions: ", "Location: ", "Methods: ", 
		"Results: ", "Objective: ", "Main Conclusions: ", "Summary: ")
	for(i in 1:length(rm.words)){
		y<-lapply(y, function(a, word){gsub(word, "", a)}, word=rm.words[i])}

	# end
	return(unlist(y))
	}
# Note that this is not passed back to the source list; so copyright notices etc will be displayed by the app despite removal from LDA


# function to run all analyses, given a vector of excluded words
LDAfun<-function(x, n.topics=6, iter=1000, ...){ # vector of abstracts
	if(class(x)=="vector"){x<-get.dtm(x, ...)}
	LDA.control<-list(iter=iter)
	model<-topicmodels::LDA(x, k= n.topics, method="Gibbs",  control = LDA.control)
	return(model)
	}


# calculate document term matrix
get.dtm<-function(x, stop.words){ # where x is a vector of abstracts
	dtm.control <- list(
		tolower = TRUE,
		removePunctuation = TRUE,
		removeNumbers = TRUE,
		stopwords = stop.words,
		stemming = TRUE,
		wordLengths = c(3, Inf),
		minDocFreq=5,
		weighting = weightTf)
	# convert to document term matrix
	corp<-Corpus(VectorSource(x))
	dtm<-DocumentTermMatrix(corp, control= dtm.control)
	dtm<-removeSparseTerms(dtm, sparse= 0.99) # remove rare terms (cols)
	return(dtm)
	}
