# methods for class(review_info)
summary.review_info<-function(object, ...){
	# 1: article summary
	cat("Number of entries:\n")
		cat(paste("  total: ", nrow(x$x), "\n", sep=""))
		cat(paste("  sorted: ", length(which(x$x$tested)), "\n", sep=""))
		cat(paste("  relevant: ", length(which(x$x$tested & x$x$selected)), "\n", sep=""))
			cat()
	# 2: data
	cat("Topic modelling:\n")
		if(any(grepl("LDA", x$model@call))){
			cat("  model type: LDA\n")
		}else{
			cat("  model type: CTM\n")}
		cat(paste("  number of topics: ", x$model@k, "\n", sep=""))
		cat(paste("  number of unique words: ", ncol(x$dtm), "\n", sep=""))
		cat(paste("  mean number of words per article: ",  
			round(mean(apply(x$dtm, 1, sum)), 0), "\n", sep=""))	
}
# add something about content: most common words, perhaps name of the original file?