#' revtools: Tools to support reviews and evidence synthesis
#'
#' Researchers commonly need to summarize scientific information, a process
#' known as 'evidence synthesis'. The first stage of a synthesis process (such
#' as a systematic review or meta-analysis) is to download a list of references
#' from academic search engines such as 'Web of Knowledge' or 'Scopus'. The
#' traditional approach to systematic review is then to sort these data
#' manually, first by locating and removing duplicated entries, and then
#' screening to remove irrelevant content by viewing titles and abstracts (in
#' that order). 'revtools' provides interfaces for each of these tasks. An
#' alternative approach, however, is to draw on tools from machine learning to
#' visualise patterns in the corpus. In this case, you can use 'revtools' to
#' render ordinations of text drawn from article titles, keywords and
#' abstracts, and interactively select or exclude individual references, words
#' or topics.
#'
#' @name revtools-package
#' @aliases revtools
#' @section Functions:
#' \strong{Individual screening}
#' \itemize{
#'   \item{[screen_duplicates()] Screen for duplicates}
#'   \item{[screen_titles()] Screen articles by title}
#'   \item{[screen_abstracts()] Screen articles by abstract}
#'   \item{[screen_topics()] Screen data by topic}
#' }
#' \strong{Team screening}
#' \itemize{
#'   \item{[allocate_effort()] Specify how to distribute screening effort among a team of reviewers}
#'   \item{[distribute_tasks()] Split a dataset among a team of reviewers}
#'   \item{[aggregate_tasks()] Combine screening results from a team of reviewers}
#'   \item{[preload_screen_abstracts()] Pre-package a screening app to forward to reviewers}
#' }
#' \strong{Text mining}
#' \itemize{
#'   \item{[revwords()] Stopwords used in revtools functions}
#'   \item{[make_dtm()] Construct a Document-Term Matrix from bibliographic data}
#'   \item{[run_topic_model()] Wrapper function for topic models}
#'   \item{[screen_topics_progress()] Object class for storing and restarting topic-based screening}
#' }
#' \strong{Deprecated functions}
#' \itemize{
#'   \item{[read_bibliography()] Import bibliographic data (use [synthesisr::read_refs()] instead)}
#'   \item{[write_bibliography()] Export bibliographic data (use [synthesisr::write_refs()] instead)}
#' }
#' @keywords internal
"_PACKAGE"