#' Calculate a topic model
#'
#' Calculate a topic model using Latent Dirichlet Allocation (\code{LDA}) or
#' Correlated Topic Models (\code{CTM}), using the \code{topicmodels} package.
#'
#'
#' @param dtm a Document Term Matrix (DTM)
#' @param type string specififying the type of model to run. Either 'lda' (the
#' default) or 'ctm'.
#' @param n_topics Number of topics to calculate
#' @param iterations The number of iterations. Only relevant for LDA.
#' @return A topic model with the specified parameters.
#' @note This is a basic wrapper function designed to allow consistent
#' specification of model parameters within \code{shiny} apps.
#' @seealso \code{\link{make_dtm}} for constructing data to pass to this
#' function; \code{\link{screen_topics}} for interactive visualisation of topic
#' model results.
#' @examples
#'
#' # import data
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools"
#' )
#' x <- read_bibliography(file_location)
#'
#' # run a topic model based on these data
#' # note: the following lines can take a very long time to run, especially for large datasets!
#' x_dtm <- make_dtm(x$title)
#' \dontrun{x_lda <- run_topic_model(x_dtm, "lda", 5, 5000)}
#'
#' @export run_topic_model
run_topic_model <- function(
	dtm,
	type = "lda",
	n_topics = 5,
	iterations = 2000
){
	LDA_control <- list(
    iter = iterations,
    burnin = iterations * 0.1
  )
	switch(type,
		"ctm" = {
      topicmodels::CTM(
        dtm,
        k = n_topics
      )
    },
		"lda" = {
      topicmodels::LDA(
        dtm,
        k = n_topics,
        method = "Gibbs",
        control = LDA_control
      )
    }
  )
}
