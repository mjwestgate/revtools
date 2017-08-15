# function to convert a single bibdata object, or a list of the same, into a data.frame
make_reference_dataframe<-function(x){ # x must be a list or a bibdata object

	if(class(x)=="bibdata"){x<-list(bibdata=x)}
	
	# convert each list entry into a data.frame
	unlisted.x<-lapply(x, function(a){
		columns.req<-c("author", "year", "title", "journal", "volume", "number", "pages")
		entry.standard<-lapply(a, function(b, cols){
			result<-lapply(cols, function(z, lookup){lookup[[z]]}, lookup=b)
			names(result)<-cols
			if(length(result$author)>1){result$author<-paste(result$author, collapse=" and ")}
			return(as.character(result))
			}, cols= columns.req)
		entry.dframe<-as.data.frame(do.call(rbind, entry.standard), stringsAsFactors=FALSE)
		colnames(entry.dframe)<-columns.req
		return(entry.dframe)
		})
	
	# rbind into a single data.frame
	result<-as.data.frame(do.call(rbind, unlisted.x), stringsAsFactors=FALSE)
	result$source<-rownames(result)
	rownames(result)<-NULL
	result<-result[, c(ncol(result), 1:(ncol(result)-1))]
	
	# add an ID column
	result$ID<-c(1:nrow(result))
	result<-result[, c(ncol(result), 1:(ncol(result)-1))]
	
	# remove missing values
	for(i in 3:ncol(result)){
		if(any(result[, i]=="NULL")){
			result[which(result[, i]=="NULL"), i]<-NA}}
	return(result)
}