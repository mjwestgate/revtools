# contains info on class("bibdata")
summary.bibliography<-function(object, ...){

	# are any abstracts completely missing? 
	null.check<-unlist(lapply(object, function(a){is.null(a$abstract)}))
	null.count<-length(object)-length(which(null.check))
	null.percent<-round((100/length(object)) * null.count, 1)

	# how many sources?
	n.sources<-unlist(lapply(object, function(a){a$journal}))
	if(is.null(n.sources)==FALSE){
		source.freq<-sort(xtabs(~n.sources), decreasing=TRUE)[1:5]
		# put text together
		result<-paste(
			paste("Object of class 'bibliography' containing", length(object), "entries.", sep=" "), "\n\t",
			paste("Number containing abstracts: ", null.count, " (", null.percent, "%)", sep="") , "\n",
			paste("Number of sources:", length(unique(n.sources)), sep=" "), "\n",
			"Most common sources:", "\n\t",
			paste(names(source.freq), " (n = ", as.numeric(source.freq), ")", sep="", collapse="\n\t"),
		sep="",
		collapse="\n")
	}else{
		result<-paste(
			paste("Object of class 'bibliography' containing", length(object), "entries.", sep=" "), "\n\t",
			paste("Number containing abstracts: ", null.count, " (", null.percent, "%)", sep="") , "\n",
		sep="",
		collapse="\n")
	}
	cat(result, sep="\n")
}


print.bibliography<-function(x, ...){
	if(length(x)>5){n<-5}else{n<-length(x)}
	text.tr<-lapply(x[1:n], format_citation)
	cat(paste(unlist(text.tr), collapse="\n\n"))
}