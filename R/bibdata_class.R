# contains info on class("bibdata")
summary.bibdata<-function(x){

	# are any abstracts completely missing? 
	null.check<-unlist(lapply(x, function(a){is.null(a$abstract)}))
	null.count<-length(x)-length(which(null.check))
	null.percent<-round((100/length(x)) * null.count, 1)

	# how many sources?
	n.sources<-unlist(lapply(x, function(a){a$journal}))
	if(is.null(n.sources)==FALSE){
		source.freq<-sort(xtabs(~n.sources), decreasing=TRUE)[1:5]
		# put text together
		result<-paste(
			paste("Object of class 'bibdata' containing", length(x), "entries.", sep=" "), "\n\t",
			paste("Number containing abstracts: ", null.count, " (", null.percent, "%)", sep="") , "\n",
			paste("Number of sources:", length(unique(n.sources)), sep=" "), "\n",
			"Most common sources:", "\n\t",
			paste(names(source.freq), " (n = ", as.numeric(source.freq), ")", sep="", collapse="\n\t"),
		sep="",
		collapse="\n")
	}else{
		result<-paste(
			paste("Object of class 'bibdata' containing", length(x), "entries.", sep=" "), "\n\t",
			paste("Number containing abstracts: ", null.count, " (", null.percent, "%)", sep="") , "\n",
		sep="",
		collapse="\n")
	}
	cat(result, sep="\n")
	}


print.bibdata<-function(x){
	if(length(x)>5){n<-5}else{n<-length(x)}
	text.tr<-lapply(x[1:n], pretty.citations)
	cat(
		# paste("Bibliography containing ", length(x), " entries, the first ", n, " of which are:\n\n", sep=""),
		paste(unlist(text.tr), collapse="\n\n"))
	}