extract_unique_references<-function(
	x, # data.frame returned by make_reference_dataframe
	show_source=FALSE # should a matrix of source values be returned? Defaults to FALSE
	){
	duplicate_vals<-unique(x$duplicate_group)
	source_vals<-unique(x$source)
	x_split<-split(x, x$duplicate_group)
	x_split<-lapply(x_split, function(a, lookup){
		result<-a[1, ]
		result$n_duplicates<-nrow(a)
		if(show_source){
			lookup_result<-unlist(lapply(lookup, function(b, this_run){
				length(which(this_run==b))}, this_run=a$source))
			names(lookup_result)<-lookup
			result<-as.data.frame(c(result, lookup_result), stringsAsFactors=FALSE)
			}
		return(result)	
		}, lookup=source_vals)
	output<-as.data.frame(do.call(rbind, x_split), stringsAsFactors=FALSE)		
	return(output)
	}