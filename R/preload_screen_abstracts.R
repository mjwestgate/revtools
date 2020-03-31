# function to build a customized version of screen topics for sharing with a team
# note that data is saved to .RData rather than .rds so that clicking the file sets the workspace
preload_screen_abstracts <- function(
  x, # data to include as a data.frame, or list of data.frames
  file_out = "screen_abstracts_preloaded.RData", # file name and path. must be in .RData format
  app_control # list containing information on how the app should function
){
  if(missing(x)){stop("No data supplied")}

  if(missing(app_control)){
    app_control_clean <- validate_app_control()
  }else{
    app_control_clean <- validate_app_control(app_control)
  }

  if(inherits(x, "list")){
    if(length(x) != length(file_out)){
      stop("if x is a list, then file_out should be of same length")
    }else{
      invisible(lapply(seq_along(x), function(a){
        screen_abstracts_preloaded <- preload_screen_abstracts_build(
          x[a],
          file_out = file_out[a],
          app_control = app_control_clean
        )
        data <- x[a]
        attr(screen_abstracts_preloaded, data, "date_generated") <- as.character(Sys.time())
        save(screen_abstracts_preloaded, file = file_out[a])
      }))
    }
  }else{ # i.e. if x is a data.frame
    screen_abstracts_preloaded <- preload_screen_abstracts_build(
      x,
      file_out = file_out,
      app_control = app_control_clean
    )
    data <- x
    attr(screen_abstracts_preloaded, "date_generated") <- as.character(Sys.time())
    save(screen_abstracts_preloaded, data, file = file_out)
  }
}


# function to ensure that app_control lists are usable
# no substantive checking yet
validate_app_control <- function(app_control_list){

  app_control_default <- list(
    show_identifying_info = FALSE,
    time_responses = TRUE,
    keyword_highlighting = FALSE,
    highlight_color = "red",
    rank_by = "initial", # c("initial", "random", "alphabetical", "relevance")
    keywords = "", # optional list of keywords for ranking or highlighting
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