# function to build a customized version of screen topics for sharing with a team
# note that this function simply outputs an S3 object with requisite information for app building
# screen_abstracts then uses this information to build the app on the destination machine
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