# contains info on class("bibdata")
summary.bibliography<-function(object, ...){

	# are any abstracts completely missing? 
	null.check<-unlist(lapply(object, function(a){is.null(a$abstract)}))
	null.count<-length(object)-length(which(null.check))
	null.percent<-round((100/length(object)) * null.count, 1)

	# how many sources?
	sources<-unlist(lapply(object, function(a){a$journal}))
	if(is.null(sources)==FALSE){
		n_sources<-length(unique(sources))
		source.freq<-sort(xtabs(~sources), decreasing=TRUE)[c(1:min(5, n_sources))]
		# put text together
		result<-paste(
			paste0("Object of class 'bibliography' containing ", length(object), " entries."), "\n  ",
			paste0("Number containing abstracts: ", null.count, " (", null.percent, "%)") , "\n",
			paste0("Number of sources: ", n_sources), "\n",
			"Most common sources:", "\n  ",
			paste(names(source.freq), " (n = ", as.numeric(source.freq), ")", sep="", collapse="\n  "),
		sep="",
		collapse="\n")
	}else{
		result<-paste(
			paste("Object of class 'bibliography' containing", length(object), "entries.", sep=" "), "\n  ",
			paste("Number containing abstracts: ", null.count, " (", null.percent, "%)", sep="") , "\n",
		sep="",
		collapse="\n")
	}
	cat(result, sep="\n")
}


print.bibliography<-function(x, n=5, ...){
	text.tr<-lapply(x[1:n], format_citation)
	cat(paste(unlist(text.tr), collapse="\n\n"))
}