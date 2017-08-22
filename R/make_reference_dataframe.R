# function to convert a single bibdata object, or a list of the same, into a data.frame
make_reference_dataframe<-function(
	x, # an object of class 'bibliography' or list of the same
	abstracts=TRUE
	){

	if(class(x)=="bibliography"){x<-list(entry=x)}
	columns_req<-c("author", "year", "title", "journal", "volume", "number", "pages", "keywords")
	if(abstracts){columns_req<-c(columns_req, "abstract")}

	# convert each list entry into a data.frame
	unlisted.x<-lapply(x, function(a, info){
		entry_standard<-lapply(a, function(b, cols){
			result<-lapply(cols, function(z, lookup){
				if(any(names(lookup)==z)){lookup[[z]]}else{""}}, lookup=b)
			names(result)<-cols
			if(length(result$author)>1){result$author<-paste(result$author, collapse=" and ")}
			if(length(result$keywords)>1){result$keywords<-paste(result$keywords, collapse=" and ")}
			return(as.character(result))
			}, cols= info)
		entry_dframe<-as.data.frame(
			cbind(names(entry_standard), do.call(rbind, entry_standard)), 
			stringsAsFactors=FALSE)
		rownames(entry_dframe)<-NULL
		colnames(entry_dframe)<-c("label", info)
		return(entry_dframe)
		}, info = columns_req)

	# convert from list to data.frame
	if(length(unlisted.x)==1){result<-data.frame(
		ID=c(1:nrow(unlisted.x[[1]])), unlisted.x[[1]], stringsAsFactors=FALSE)
	}else{
		names_list<-list()
		for(i in 1:length(unlisted.x)){names_list[[i]]<-rep(names(unlisted.x)[i], nrow(unlisted.x[[i]]))}
		source_vector<-do.call(c, names_list)
		result<-data.frame(
			ID=c(1:length(source_vector)),
			source= source_vector, 
			do.call(rbind, unlisted.x), 
			stringsAsFactors=FALSE)
		rownames(result)<-NULL
		}
	# remove missing values
	for(i in 4:ncol(result)){
		if(any(result[, i]=="")){result[which(result[, i]==""), i]<-NA}}
	return(result)
}