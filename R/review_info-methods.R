# methods for class(review_info)
summary.review_info<-function(object, ...){
cat(
	paste0("review_info object containing 5 entries:\n  info = data on ", 
		nrow(object$info),
		" citations sent to start_review_window\n  dtm = document term matrix (", 
		nrow(object$dtm), " rows, ", ncol(object$dtm), 
		" cols)\n  model = topic model with ",
		object$model@k, 
		" topics\n  plotinfo = list containing data for plots (x=articles, y=words, topic=topics)\n",
		"  infostore = list containing review decisions data\nProgress: ",
		length(which(object$infostore$x$selected)), " selected from ",
		length(which(object$infostore$x$tested)), " decisions"
	)
)
}