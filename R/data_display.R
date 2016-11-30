# extract data on the numbers of times each article has been cited
citations.n<-function(x){
	# find citation data within the supplied list
	citation.search<-lapply(x, function(a){
		if(any(names(a)=="TC")){
			return(as.numeric(a$TC))
		}else{
			a<-a[-which(names(a)=="AB")]
			a<-a[-which(names(a)=="TI")]	
			# find text
			test1<-grep("cited by", a, ignore.case=TRUE)
			if(length(test1)<1){result<-0
			}else{
				if(length(a[[test1]])>1){
					text.val<-grep("cited by", a[[test1]], ignore.case=TRUE, value=TRUE)
				}else{text.val<-a[[test1]]}
			text.clean<-removePunctuation(text.val)
			text.split<-strsplit(text.clean, " ")[[1]] 
			split.matrix<-matrix(data=c(c(1:(length(text.split)-1)), c(2:length(text.split))), 
				nrow=length(text.split)-1, ncol=2, byrow=FALSE)
			text.pairs<-apply(split.matrix, 1, function(a, lookup){
				paste(lookup[as.numeric(a)], collapse=" ")
				}, lookup=text.split)
			text.loc<-grep("cited by", text.pairs, ignore.case=TRUE, value=FALSE)
			result<-text.split[max(split.matrix[text.loc, ])+1]		
			# grep("cited by",
			# text.split<-strsplit(text.val, ":")[[1]]
			# if(length(text.split)==1){text.split<-strsplit(text.split, " ")[[1]]}
			# result<-text.split[length(text.split)]
			}
			return(result)
		}
		})
	# convert to data.frame
	result.drame<-data.frame(
		label=names(citation.search),
		n=unlist(as.numeric(citation.search)),
		stringsAsFactors=FALSE)
	rownames(result.drame)<-NULL
return(result.drame)
}


# Function to display bibliographic information on selected articles
pretty.citations<-function(
	x, # list of data from a standard import function
	abstract=FALSE, # option to return only the citation for an article
	details=TRUE # whether to allow or suppress bibliographic details - name, year, journal
	){
	if(details){
		# author info
		# remove any additional characters that display affiliations (i.e. those after last ".")
		author.data<-unlist(lapply(strsplit(x$AU, ""), function(a){
			dot.lookup<-a %in% "."
			if(any(dot.lookup)){a<-a[1:max(which(dot.lookup))]}
			return(paste(a, collapse=""))
			}))
		author.data<-unlist(lapply(strsplit(author.data, ", "), function(a){paste(a[2], a[1], sep=" ")}))
	
		if(length(x$AU)>3){author.info<-paste(author.data[1], ", ", author.data[2], " et al.", sep="")
		}else if(length(x$AU)==3){author.info <-paste(author.data[1], ", ", 
			author.data[2], " & ", author.data[3], sep="")
		}else if(length(x$AU)==2){author.info <-paste(author.data, collapse=" & ")
		}else if(length(x$AU)==1){author.info <-author.data
		}else if(length(x$AU)==0){author.info <-"Anon."}
	
		# change to consistent format for page numbers - convert SP to BP
		if(any(names(x)=="SP")){names(x)[which(names(x)=="SP")]<-"BP"}
		page.data<-x[c(which(names(x)=="BP"), which(names(x)=="EP"))]
		if(length(page.data)<2){page.info<-""
		}else{page.info<-paste(": ", paste(page.data, collapse="-"), sep="")}
	
		# paste info in the correct order
		result<-paste(author.info, " (", x$PY, ") ", x$TI, ". ", x$SO, " ", x$VL, page.info, sep="")
	
		# add abstract if required
		if(abstract){
			result<- paste(result, ".<br><br><strong>Abstract</strong><br>", x$AB, sep="")}

	}else{
		if(abstract){result<-paste("<strong>Title:</strong> ", x$TI, 
			"<br><br><strong>Abstract</strong><br>", x$AB, sep="")
		}else{result<-paste("<strong>Title:</strong> ", x$TI, sep="")
		}}

	return(result)
	}