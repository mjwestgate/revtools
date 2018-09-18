# methods for class(screen_topics_progress)
summary.screen_topics_progress <- function(object, ...){
cat(
	paste0("screen_topics_progress object containing 7 entries including:\n
    raw = data on ",
		  nrow(object$raw),
		  " citations sent to screen_topics()\n
    grouped = raw data grouped into ",
      nrow(object$grouped),
      " unique ",
      colnames(object$grouped)[1],
      "s\n
    dtm = document term matrix with ",
		  nrow(object$dtm),
      " rows & ",
      ncol(object$dtm),
		  " columns\n
    model = topic model with ",
		  object$model@k,
		  " topics"
	)
)
}