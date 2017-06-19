# this replaces prep.bib

# function to call 
getDTM<-function(
	x, # a bibdata object
	stop.words,
	title=TRUE,
	abstract=TRUE,
	keyword=TRUE,
	threshold=0.05
	){

	# sort out stop words
	if(missing(stop.words)){stop.words<-tm::stopwords()
	}else{stop.words<-unique(c(tm::stopwords(), stop.words))}

	# some articles have added words in their abstracts - set list for later
	rm.words<-c("Abstract:", "Aim:", "Aims:", "Background:", 
		"Capsule:", "Context:", "Conclusions:", "Location:", "Methods:", 
		"Results:", "Objective:", "Main Conclusions:", "Summary:")

	# get relevant information
	if(title){titles<-unlist(lapply(x, function(a){
		if(any(names(a)=="title")){a$title}else{""}}))
		}else{titles<-rep("", length(x))}

	if(abstract){abstracts<-unlist(lapply(x, function(a, removables){
		if(any(names(a)=="abstract")){
			y<-a$abstract # get info

			# remove redundant terms
			y<-strsplit(y, " \\(C\\)")[[1]][1] # ISI lists copyright as (C)
			raw.vals<-charToRaw(y) # Scopus uses the actual 'copyright' symbol \u00a9
			if(any(raw.vals =="a9")){		
				final.char<-sort(which(raw.vals=="a9"))[1]-3
				y<-rawToChar(raw.vals[c(1:final.char)])}
			y<-gsub(paste(removables, collapse="|"), "", y) # generic scientific stuff

			return(y)
			}else{""}}, removables=rm.words))
		}else{abstract <-rep("", length(x))}

	if(keyword){keywords <-unlist(lapply(x, function(a){
		if(any(names(a)=="keywords")){paste(a$keywords, collapse=" ")}else{""}}))
		}else{keywords<-rep("", length(x))}

	# combine into a single string, convert to DTM
	text.vector<-paste(titles, abstracts, keywords, sep=" ")

	# convert to document term matrix
	corp <- Corpus(VectorSource(text.vector))
		corp <- tm_map(corp,
			content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))#,
			# mc.cores=1)
		corp <- tm_map(corp, content_transformer(tolower))#, lazy=TRUE)
		corp <- tm_map(corp, removeWords, stop.words)
		corp <- tm_map(corp, removeNumbers)
		corp <- tm_map(corp, removePunctuation)
		stem.corp <- tm_map(corp, stemDocument)# , lazy=TRUE)

	# create a lookup data.frame
	term<-unlist(lapply(as.list(corp), function(a){
		result<-strsplit(a, " ")[[1]]
		result <-gsub("^\\s+|\\s+$", "", result)
		return(result[which(result!=c(""))])
		}))
	word.freq<-as.data.frame(xtabs(~ term), stringsAsFactors=FALSE)
	word.freq$stem<-stemDocument(word.freq$term)

	# use control in DTM code to do remaining work
	dtm.control <- list(
		wordLengths = c(3, Inf),
		minDocFreq=5,
		weighting = weightTf)
	dtm<-DocumentTermMatrix(stem.corp , control= dtm.control)
	dtm<-removeSparseTerms(dtm, sparse= 0.99) # remove rare terms (cols)

	# next step from topicmodels intro
	# removes both very rare and very common cols
	# term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
	# dtm<-dtm[, term_tfidf>=threshold]

	output<-as.matrix(dtm)
	rownames(output)<-names(x)
	# output<-output[which(apply(output, 1, sum)>0), ] # this is done later

	# replace stemmed version with most common full word
	# this means counts are done on the stemmed words, 
	# but results are reported using original words for clarity
	term.vec<-unlist(lapply(colnames(output), function(a, check){
		if(any(check$stem==a)){
			rows<-which(check$stem==a)	
			row<-rows[which.max(check$Freq[rows])]
			return(check$term[row])
		}else{return(a)}
		}, check=word.freq))
	colnames(output)<-term.vec

	return(output)
}



# function to run the topic model in question
LDAfun<-function(
	x,  # DTM
	topic.model="LDA", n.topics=6, iter=2000, ...)
	{
	LDA.control<-list(iter=iter)
	switch(topic.model,
		"CTM"={topicmodels::CTM(x, k= n.topics)},
		"LDA"={topicmodels::LDA(x, k= n.topics, method="Gibbs",  control = LDA.control)})
	}
