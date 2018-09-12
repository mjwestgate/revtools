# function to cleanly extract colnames of interest from supplied datasets
get_topic_colnames <- function(data){
  colnames(data)[
    which(
      (colnames(data) %in%
      c("selected", "topic", "display", "notes")) == FALSE
    )
  ]
}

# csv files without messy column names
clean_names <- function(
  data # data.frame
){
  x <- colnames(data)
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- sub("s$", "", x) # remove trailing s's
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  x <- tolower(x)
  x <- make.unique(x, sep = "_")
  colnames(data) <- x
  return(data)
}


# import data within a shiny app
import_shiny <- function(
  source, # input$data_in
  current_data = NULL # existing data
  ){
  is_csv <- grepl(".csv$", source$name)
  if(is.null(current_data)){
    if(is.null(source)){
      result <- NULL
    }else{
      if(is_csv){
        result <- read.csv(
          source$datapath,
          stringsAsFactors = FALSE
        )
      }else{
        result <- as.data.frame(
          read_bibliography(source$datapath)
        )
      }
    }
  }else{
    if(is.null(source)){
      result <- current_data
    }else{
      if(is_csv){
        result <- merge_columns(
          current_data,
          read.csv(source$datapath, stringsAsFactors = FALSE)
        )
      }else{
        result <- merge_columns(
          current_data,
          as.data.frame(read_bibliography(source$datapath))
        )
      }
    }
  }

  return(clean_names(result))
}


# ditto, but for screen_topics
# this is more complex because it has to allow .rds files
import_shiny_topic_data <- function(
  source, # input$data_in
  current_data = NULL # existing data
  ){

  x <- list(
    raw = NULL,
    stopwords = revtools_stopwords(),
    columns = NULL,
    grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )

  if(!is.null(source)){
    file_type <- "neither"
    if(grepl(".rds$", source$name)){
      file_type <- "rds"
    }
    if(grepl(".csv$", source$name)){
      file_type <- "csv"
    }

    switch(file_type,
    "rds" = {
      data_in <- readRDS(source$datapath)
      if(all(names(data_in) == names(x))){
        x$raw <- data_in$raw
        x$stopwords <- data_in$stopwords
        x$columns <- data_in$columns
        x$grouped <- data_in$grouped
        x$dtm <- data_in$dtm
        x$model <- data_in$model
        x$plot_ready <- data_in$plot_ready
      }
    },
    "csv" = {
      data_in <- add_required_columns(
        clean_names(
          read.csv(
            source$datapath,
            stringsAsFactors = FALSE
          )
        )
      )
      if(is.null(current_data$raw)){
        x$raw <- data_in
        x$columns <- get_topic_colnames(x$raw)
      }else{
        x$raw <- merge_columns(current_data$raw, data_in)
        x$columns <- get_topic_colnames(x$raw)
      }
    },
    "neither" = {
      data_in <- add_required_columns(
        as.data.frame(
          read_bibliography(source$datapath)
        )
      )
      if(is.null(current_data$raw)){
        x$raw <- data_in
        x$columns <- get_topic_colnames(x$raw)
      }else{
        x$raw <- merge_columns(current_data$raw, data_in)
        x$columns <- get_topic_colnames(x$raw)
      }
    }) # end switch

  } # end if(!is.null(source))

  return(x)
}


# similar to import_shiny, but for initial load of data
load_topic_data <- function(
  data,
  stopwords
){

  x <- list(
    raw = NULL,
    stopwords = revtools_stopwords(),
    columns = NULL,
    grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )

  if(!is.null(data)){

    # throw a warning if a known file type isn't given
    accepted_inputs <- c(
      "bibliography",
      "data.frame",
      "screen_topics_progress"
    )
    if(!any(accepted_inputs == class(data))){
      stop("only classes 'bibliography', 'data.frame' or
      'screen_topics_progress' accepted by screen_topics")
    }

    # add data as necessary for that file type
    switch(class(data),
      "bibliography" = {
        x$raw <- add_required_columns(
          clean_names(
            as.data.frame(data)
          )
        )
      },
      "data.frame" = {
        x$raw <- add_required_columns(
          clean_names(data)
        )
      },
      "screen_topics_progress" = {
        x$raw <- data$raw
        x$stopwords <- data$stopwords
        x$columns <- data$columns
        x$grouped <- data$grouped
        x$dtm <- data$dtm
        x$model <- data$model
        x$plot_ready <- data$plot_ready
      }
    )

    # add colnames
    if(is.null(x$columns)){
      x$columns <- get_topic_colnames(x$raw)
    }

  } # end if(!is.null(data))

  # add user-defined stopwords if given
  if(!is.null(stopwords)){
    x$stopwords <- unique(c(
      x$stopwords,
      as.character(stopwords)
    ))
  }

  return(x)
}