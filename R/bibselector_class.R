# methods for class(bibselector)

summary.bibselector<-function(x){
	# 1: article summary
	cat("Number of entries:\n")
		cat(paste("  total: ", length(x$DATA), "\n", sep=""))
		cat(paste("  sorted: ", length(which(x$SEL$tested)), "\n", sep=""))
		cat(paste("  relevant: ", length(which(x$SEL$tested & x$SEL$selected)), "\n", sep=""))
			cat()
	# 2: data
	cat("Topic modelling:\n")
		if(any(grepl("LDA", x$LDA@call))){
			cat("  model type: LDA\n")
		}else{
			cat("  model type: CTM\n")}
		cat(paste("  number of topics: ", x$LDA@k, "\n", sep=""))
		cat(paste("  number of unique words: ", ncol(x$DTM), "\n", sep=""))
		cat(paste("  mean number of words per article: ",  
			round(mean(apply(x$DTM, 1, sum)), 0), "\n", sep=""))	
}

# testing
# x<-test
# class(x)<-"bibselector"
# summary(x)

# add something about content: most common words, perhaps name of the original file?