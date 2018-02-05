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


print.bibliography<-function(x, n, ...){
	length.tr<-length(x)
	if(missing(n)){
		n<-min(c(length.tr, 5))
	}else{
		if(n>length.tr){n<-length.tr}
	}
	text.tr<-lapply(x[1:n], format_citation)
	cat(paste(unlist(text.tr), collapse="\n\n"))
}


'[.bibliography'<-function(x, n){
class(x)<-"list"
if(all(n %in% c(1:length(x)))==FALSE){stop("subset out of bounds")}
z<-x[n]
class(z)<-"bibliography"
return(z)
}


# function to convert an object of class 'bibliography' into a data.frame
as.data.frame.bibliography<-function(x, ...){

	cols<-unique(unlist(lapply(x, names)))
	cols<-cols[which(cols!="further_info")]

	x_list<-lapply(x, function(a, cols){
		result<-lapply(cols, function(b, lookup){
			if(any(names(lookup)==b)){
				data_tr<-lookup[[b]]
				if(length(data_tr)>1){data_tr<-paste0(data_tr, collapse=" and ")}
				return(data_tr)
			}else{return(NA)}
		}, lookup=a)
		names(result)<-cols
		return(as.data.frame(result, stringsAsFactors=FALSE))
		}, cols = cols)

	x_dframe<-data.frame(
		label=make.names(names(x_list), unique=TRUE),
		do.call(rbind, x_list), 
		stringsAsFactors=FALSE)
	rownames(x_dframe)<-NULL

	return(x_dframe)
}


as.bibliography<-function(x, ...){

	if(class(x)!="data.frame"){
		stop("as.bibliography can only be called for objects of class 'data.frame'")
	}

	# get labels for each entry
	x_cols<-colnames(x)
	if(any(x_cols=="label")){
		label_col<-which(x_cols=="label")
		label_vec<-x[, label_col]
		x<-x[, -label_col]
	}else{
		label_vec<-paste0("ref", c(1:nrow(x)))
	}

	x_list<-lapply(split(x, c(1:nrow(x))), function(a){
		a<-as.list(a)
		if(any(names(a)=="author")){a$author<-strsplit(a$author, " and ")[[1]]}
		if(any(names(a)=="keywords")){a$keywords <-strsplit(a$keywords, " and ")[[1]]}
		return(a)
	})
	names(x_list)<-label_vec
	class(x_list)<-"bibliography"
	return(x_list)
}