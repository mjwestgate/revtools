#' Preload an app for screening article abstracts
#'
#' This function takes some data and generates a self-contained app, stored in
#' a .RData file, that can be forwarded to collaborators for screening. A
#' series of options can be pre-set to customize user experience.
#'
#' List entries that can be specified within \code{app_control} are as follows:
#' \itemize{
#'   \item\code{show_identifying_info} - Logical: should author names,
#'    publication year and journal title be displayed? Defaults to FALSE.
#'   \item\code{time_responses} - Logical: should the app record timing
#'    information? If TRUE (the default) record the time spent on each page, and
#'    the time at which the page was loaded.
#'   \item\code{keyword_highlighting} - Logical: should the app highlight
#'    user-specified keywords? Defaults to FALSE.
#'   \item\code{keywords} - Optional set of keywords to be highlighted in
#'     article titles and abstracts.  \item\code{highlight_color} - Optional color
#'     for keyword highlighting. Defaults to "red".
#'   \item\code{rank_by} - Specify how entries should be ordered. Should be one
#'     of \code{"initial"} (the default) to maintain the order of \code{x},
#'     \code{"random"} to randomize order via \code{rnorm}, \code{"alphabetical"}
#'    for alphabetical by article title, or \code{"relevance"} for ranking by
#'    number of matched keywords.
#'   \item\code{save_csv} - Logical: should the app save a \code{csv} file as
#'     well as a \code{.RData} file? Defaults to FALSE.
#' }
#'
#' @param data A \code{data.frame} containing bibliographic information.
#' @param file Name of the output \code{RData} file. Defaults to
#' 'screen_abstracts.Rdata'.
#' @param app_control A list containing additional information to customize UI.
#' See details for what options can be set.
#' @param format What format should the data be saved in? Options are
#' \code{".RData"} (the default), \code{"rds"} to save in RDS, or \code{"none"}
#' to return the object to the workspace without saving a file.
#' @return Returns file or object (as specified by the \code{format} argument)
#' containing the information needed to build an app with the required
#' specifications. Specifically, it returns an S3 object of class
#' \code{screen_abstracts_preloaded} containing three slots: \code{data}, which
#' stores the underlying dataset including any user decisions, notes and timing
#' information; \code{file} which stores the output file name; and
#' \code{app_control}.
#' @examples
#'
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location)
#'
#' \dontrun{
#' preload_screen_abstracts(data,
#'   app_control = list(
#'     keyword_highlighting = TRUE,
#'     highlight_color = "red",
#'     keywords = c("management", "frog", "bird", "fire"),
#'     save_csv = FALSE
#'   )
#' )
#' # after re-loading output .RData file, run by using:
#' library(revtools)
#' print(screen_abstracts_preloaded)
#' runApp(screen_abstracts_preloaded)
#' # the above options are equivalent
#' }
#'
#' @export preload_screen_abstracts

# NEXT TASK IS TO MAKE THIS ONLY OUTPUT A .RDS FILE
# THEN MAKE SCREEN_ABSTRACTS() ACCEPT A STRING AS A FILENAME
# AND IMPORT CORRECTLY

preload_screen_abstracts <- function(
  data,
  file,
  app_control,
  format = "RData"
){
  # error catching
  if(missing(data)){stop("No data supplied")}
  if(missing(file)){file <- "screen_abstracts.RData"}
  if(missing(app_control)){
    app_control_clean <- validate_app_control()
  }else{
    app_control_clean <- validate_app_control(app_control)
  }
  if(!any(c("none", "RDS", "RData") == format)){
    stop("'format' must be one of 'none', 'RDS' or 'RData'")
  }

  # build S3 object of class "screen_abstracts_preloaded"
  app <- list(
    data = data,
    file = file,
    app_control = app_control_clean
  )
  class(app) <- "screen_abstracts_preloaded"
  attr(app, "date_generated") <- as.character(Sys.time())

  # output in specified format
  switch(format,
    "RData" = {save(app, file = file)},
    "RDS" = {saveRDS(app, file = file)},
    "none" = {return(app)}
  )
}


# function to ensure that app_control lists are usable
# no substantive checking yet
validate_app_control <- function(app_control_list){

  app_control_default <- list(
    show_identifying_info = FALSE,
    time_responses = TRUE,
    keyword_highlighting = FALSE,
    keywords = "", # optional list of keywords for ranking or highlighting
    highlight_color = "red",
    rank_by = "initial", # c("initial", "random", "alphabetical", "relevance")
    save_csv = FALSE
  )

  if(missing(app_control_list)){
    app_control_final <- app_control_default
  }else{
    control_names <- names(app_control_list)
    default_names <- names(app_control_default)
    app_control_final <- c(
      app_control_list,
      app_control_default[!(default_names %in% control_names)]
    )
  }

  # error checking
  if(all(app_control_final$keywords == "") & app_control_final$rank_by == "relevance"){
    stop("keywords must be specified for rank_by = 'relevance' to work")
  }
  if(all(app_control_final$keywords == "") & app_control_final$keyword_highlighting == TRUE){
    stop("keywords must be specified for keyword_highlighting to work")
  }

  # return
  return(app_control_final)
}