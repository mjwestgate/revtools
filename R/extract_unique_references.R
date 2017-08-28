extract_unique_references<-function(
	x, # data.frame returned by make_reference_dataframe
	show_source=FALSE # should a matrix of source values be returned? Defaults to FALSE
	){

	# basic info
	duplicate_vals<-unique(x$duplicate_group)
	source_vals<-unique(x$source)

	# situation where there are no duplicates
	if(length(x$duplicate_group)==length(duplicate_vals)){
		result<-x
		result$n_duplicates<-1
		if(show_source){
			result_list<-lapply(x$source, function(a, lookup){
				result<-rep(0, length(lookup))
				names(result)<-lookup
				result[which(lookup==a)]<-1
				return(result)
				}, lookup=source_vals)
			result<-as.data.frame(cbind(result, do.call(rbind, result_list)))
			}

	}else{
		x_split<-split(x, x$duplicate_group)
		x_split<-lapply(x_split, function(a, lookup){

			if(nrow(a)==1){
				a$n_duplicates<-1
				if(show_source){
					lookup_result<-rep(0, length(lookup)); names(lookup_result)<-lookup
					lookup_result[which(lookup==a['source'])]<-1
					return(as.data.frame(c(a, lookup_result), stringsAsFactors=FALSE))
				}else{return(a)}

			}else{
				result<-a[1, ]
				result$n_duplicates<-nrow(a)
				if(show_source){
					lookup_result<-unlist(lapply(lookup, function(b, this_run){
						length(which(this_run==b))}, this_run=a$source))
					names(lookup_result)<-lookup
					result<-as.data.frame(c(result, lookup_result), stringsAsFactors=FALSE)
					}
				return(result)	
				}
			}, lookup=source_vals)
		result<-as.data.frame(do.call(rbind, x_split), stringsAsFactors=FALSE)		
		}
	return(result)
	}