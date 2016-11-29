
# function for cleaning abstracts for text analysis
extract.abstracts<-function(x){ # list of data from an import function
	# extract relevant data from the list
	y<-lapply(x, function(a){
		if(is.null(a$AB) & is.null(a$TI)){return("")
		}else{
			if(is.null(a$AB)){return(a$TI)}
			if(is.null(a$TI)){return(a$AB)}
			if(any(c(is.null(a$AB), is.null(a$TI)))==FALSE){
				return(paste(a$TI, a$AB, sep=" "))
		}}
		})

	# are any abstracts completely missing? 
	null.check<-unlist(lapply(x, function(a){is.null(a$AB)}))
	null.count<-length(which(null.check))
	null.percent<-round((100/length(x)) * null.count, 1)
	if(any(null.check)){
		warning(paste("", null.count, " of ", length(x), " entries (", 
			null.percent, "%) do not contain abstracts: only titles used", sep=""))
		}

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

	# convert to document term matrix
	corp <- Corpus(VectorSource(x))
		corp <- tm_map(corp,
			content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
			mc.cores=1)
		corp <- tm_map(corp, content_transformer(tolower), lazy=TRUE)
		corp <- tm_map(corp, removeWords, stop.words)
		corp <- tm_map(corp, removeNumbers)
		corp <- tm_map(corp, removePunctuation)
		corp <- tm_map(corp, stemDocument, lazy=TRUE)

	# use control in DTM code to do remaining work
	dtm.control <- list(
		wordLengths = c(3, Inf),
		minDocFreq=5,
		weighting = weightTf)
	dtm<-DocumentTermMatrix(corp , control= dtm.control)
	dtm<-removeSparseTerms(dtm, sparse= 0.99) # remove rare terms (cols)
	return(dtm)
	}
