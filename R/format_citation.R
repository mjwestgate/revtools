# Function to display bibliographic information on selected articles
format_citation<-function(
	x, # list of data from a standard import function
	abstract=FALSE, # option to return only the citation for an article
	details=TRUE # whether to allow or suppress bibliographic details - name, year, journal
	){
	if(details){

		if(any(names(x)=="author")){
			# author info
			# remove any additional characters that display affiliations (i.e. those after last ".")
			author.data<-unlist(lapply(strsplit(x$author, ""), function(a){
				dot.lookup<-a %in% "."
				if(any(dot.lookup)){a<-a[1:max(which(dot.lookup))]}
				return(paste(a, collapse=""))
				}))
			if(any(grepl(",", author.data))){
				author.data<-unlist(lapply(strsplit(author.data, ", "), function(a){paste(a[2], a[1], sep=" ")}))
				}
			n.authors<-length(x$author)
			if(n.authors>=4){n.authors<-4}
			author.info<-switch(as.character(n.authors), 
				"0"="Anon.",
				"1"=author.data,
				"2"={paste(author.data, collapse=" & ")},
				"3"={paste(author.data[1], ", ", author.data[2], " & ", author.data[3], sep="")},
				"4"={paste(author.data[1], ", ", author.data[2], " et al.", sep="")})
		} else{author.info<-""}		


		# paste info in the correct order
		lookup.headers<-c("year", "title", "journal", "volume", "pages")
		lookup.result<-lookup.headers %in% names(x)
		if(all(lookup.result)){
			result<-paste(author.info, " (", x$year, ") ", 
				x$title, ". ", x$journal, " ", x$volume, ": ", x$pages, sep="")
		}else{
			result<-paste(author.info, paste(x[lookup.headers[lookup.result]], collapse=" "), sep=" ")}
			# note - the above line doesn't add brackets around year

		# add abstract if required
		if(abstract){
			result<- paste(result, ".<br><br><strong>Abstract</strong><br>", x$abstract, sep="")}
	}else{
		if(abstract){result<-paste("<strong>Title:</strong> ", x$title, 
			"<br><br><strong>Abstract</strong><br>", x$abstract, sep="")
		}else{result<-paste("<strong>Title:</strong> ", x$title, sep="")
		}}

	return(result)
	}

# duplicate version for calling apply on a data.frame
format_citation_dataframe<-function(df){
	author_vector<-strsplit(df['author'], " and ")[[1]]
	if(length(author_vector)==1){author_text<-df['author']
	}else{author_text<-paste(author_vector[1], " et al.", sep="")}
	text_vector<-paste(author_text, " (", df['year'], ") ", df['title'], ". <i>", df['journal'], "</i>.", sep="")
	# now organize so that line breaks are added at word breaks every y characters
	split_vector<-strsplit(text_vector, " ")[[1]]
	result_dframe<-data.frame(
		text=split_vector,
		nchars=nchar(split_vector),
		stringsAsFactors=FALSE)
	result_dframe$sum<-cumsum(result_dframe$nchars)
	result_dframe$group<-cut(result_dframe$sum, 
		breaks=seq(0, max(result_dframe$sum)+49, 50),
		labels=FALSE)
	result_list<-split(result_dframe$text, result_dframe$group)
	paste(unlist(lapply(result_list, function(a){paste(a, collapse=" ")})), collapse="\n")
	}