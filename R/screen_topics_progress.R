#' Methods for class 'screen_topics_progress'
#'
#' Tools to display useful information on class \code{screen_topics_progress}.
#'
#'
#' @aliases screen_topics_progress-methods summary.screen_topics_progress
#' @param object An object of class 'screen_topics_progress'
#' @param ... Any further information
#' @return Prints useful information to the workspace.
#' @note Class \code{screen_topics_progress} is a format for exporting large
#' quantities of data during reviews. It is typically stored within a .rds file
#' in the working directory. When re-imported to R using \code{readRDS}, this
#' file will contain an object of class \code{screen_topics_progress}.
#' @export summary.screen_topics_progress
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


#' Description of class 'screen_topics_progress'
#'
#' \code{screen_topics_progress} is an S3 class designed to store data from
#' \code{screen_topics}, allowing the user to re-load a previously calculated
#' topic model. If you just want to save your decisions on article
#' inclusion/exclusion, along with your notes, then this is probably overkill
#' as that information can simply be exported as a .csv file.
#'
#'
#' @section slots: Class 'screen_topics_progress' has seven slots containing
#' the following information: \itemize{ \item \strong{raw} duplicate of data
#' passed to \code{screen_topics} \item \strong{stopwords} vector of words
#' excluded from the dtm \item \strong{columns} vector of column names in the
#' original dataset \item \strong{grouped} a data.frame showing grouped data as
#' specified by the user \item \strong{dtm} document-term matrix, created by
#' \code{make_dtm} \item \strong{model} most recent topic model, created by
#' \code{run_topic_model} \item \strong{plot_ready} data needed for the main
#' plot (coordinates etc.)  }
#' @name screen_topics_progress
NULL