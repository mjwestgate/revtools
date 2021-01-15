#' Calculate a topic model
#'
#' Calculate a topic model using the \code{stm} package.
#'
#'
#' @param dtm a dfm as produced by \code{quanteda::dfm()} or equivalently
#' by \code{revtools::make_dtm()}.
#' @param n_topics Number of topics to calculate
#' @param iterations The maximum number of iterations
#' @return A topic model with the specified parameters.
#' @note This is synonymous with \code{stm::stm()}, but is kept for backwards-compatibility
#' with earlier versions of revtools.
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
#' \dontrun{x_lda <- run_topic_model(x_dtm, 5, 5000)}
#'
#' @export run_topic_model

run_topic_model <- function(
	dtm, # actually a dfm returned by make_dtm or quanteda::dfm()
	n_topics = 5,
	iterations = 2000,
  ...
){
  stm::stm(
    dtm,
    K = n_topics,
    max.em.its = iterations,
    ...)
}
