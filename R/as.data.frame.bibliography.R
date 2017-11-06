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
		label=names(x_list),
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