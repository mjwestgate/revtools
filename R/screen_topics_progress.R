#' Class `screen_topics_progress`
#' 
#' `screen_topics_progress` is an S3 class designed to store data from
#' [screen_topics()], allowing the user to re-load a previously calculated
#' topic model. It is typically stored within a .rds file
#' in the working directory. When re-imported to R using [readRDS()], this
#' file will contain an object of class `screen_topics_progress.`
#' If you just want to save your decisions on article
#' inclusion/exclusion, along with your notes, then this is probably overkill
#' as that information can simply be exported as a `.csv` file.
#'
#' @rdname screen_topics_progress
#' @returns Class `screen_topics_progress` has seven slots containing
#' the following information: 
#' 
#' \itemize{
#'   \item{`raw`: duplicate of data passed to [screen_topics()]}
#'   \item{`stopwords`: vector of words excluded from the dtm}
#'   \item{`columns`: vector of column names in the original dataset}
#'   \item{`grouped`: a data.frame showing grouped data as specified by the user}
#'   \item{`dtm`: document-term matrix, created by [make_dtm()]}
#'   \item{`model`: most recent topic model, created by [run_topic_model()]}
#'   \item{`plot_ready`: data needed for the main plot (coordinates etc.)}
#' }
#' @name screen_topics_progress
NULL

#' @rdname screen_topics_progress
#' @param object An object of class `screen_topics_progress`
#' @param ... Any further information
#' @export
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