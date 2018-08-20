# methods for class(screen_topics_progress)
summary.screen_topics_progress <- function(data, ...){
cat(
	paste0("screen_topics_progress object containing 7 entries including:\n
    raw = data on ",
		  nrow(data$raw),
		  " citations sent to screen_topics()\n
    grouped = raw data grouped into ",
      nrow(data$grouped),
      " unique ",
      colnames(data$grouped)[1],
      "s\n
    dtm = document term matrix with ",
		  nrow(data$dtm),
      " rows & ",
      ncol(data$dtm),
		  " columns\n
    model = topic model with ",
		  data$model@k,
		  " topics"
	)
)
}