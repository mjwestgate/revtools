# Function to export relevant articles in a standard format.
# used in shiny to allow export of selected information
get.citations<-function(x){
	result<-lapply(x, function(a){
		result.list<-lapply(c("AU", "TI", "SO", "VL", "BP", "EP"), function(b, check){
			if(any(names(check)==b)){check[which(names(check)==b)]}else{NA}}, check=a)
		result.list[[1]][[1]]<-result.list[[1]][[1]][1]
		result.vec<-unlist(result.list)
		names(result.vec)<-c("AU", "TI", "SO", "VL", "BP", "EP")
		return(result.vec)
	})
	result<-as.data.frame(do.call(rbind, result))
	return(result)
	}
# Note: At a later time, supplementary info can be added to these articles,
# such as whether they were selected manually or by algorithm, and their expected probability of relevance according to any selection algorithm.
