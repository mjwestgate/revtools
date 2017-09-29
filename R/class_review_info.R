# methods for class(review_info)
summary.review_info<-function(object, ...){
	# 1: article summary
	cat("Number of entries:\n")
		cat(paste0("  total: ", nrow(object$x), "\n"))
		cat(paste0("  sorted: ", length(which(object$x$tested)), "\n"))
		cat(paste0("  relevant: ", length(which(object$x$tested & object$x$selected)), "\n"))
		cat()
	# 2: data
	cat("Topic modelling:\n")
		if(any(grepl("LDA", object$model@call))){
			cat("  model type: LDA\n")
		}else{
			cat("  model type: CTM\n")}
		cat(paste0("  number of topics: ", object$model@k, "\n"))
		cat(paste0("  number of unique words: ", ncol(object$dtm), "\n"))
		cat(paste0("  mean number of words per article: ",  
			round(mean(apply(object$dtm, 1, sum)), 0), "\n"))	
}
# add something about content: most common words, perhaps name of the original file?