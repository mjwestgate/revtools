# import data within a shiny app
import_shiny <- function(
  source, # input$data_in
  current_data = NULL # existing data
  ){
  # is_csv <- grepl(".csv$", source$name)
  if(is.null(current_data)){
    if(is.null(source)){
      result <- NULL
    }else{
      result <- read_bibliography(source$datapath)
    }
  }else{
    if(is.null(source)){
      result <- current_data
    }else{
      result <- merge_columns(
        current_data,
        read_bibliography(source$datapath)
      )
    }
  }
  return(result)
}

# ditto, but for screen_topics
# this is more complex because it has to allow .rds files
import_shiny_topic_data <- function(
  source, # input$data_in
  current_data = NULL # existing data
  ){

  x <- list(
    raw = NULL,
    stopwords = revwords(),
    columns = NULL,
    grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )

  if(grepl(".rds$", source$name)){
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
  }else{ # csv, bib, ris etc
    data_in <- add_required_columns(
      read_bibliography(source$datapath)
    )
    if(is.null(current_data$raw)){
      x$raw <- data_in
      x$columns <- get_topic_colnames(x$raw)
    }else{
      x$raw <- merge_columns(current_data$raw, data_in)
      x$columns <- get_topic_colnames(x$raw)
    }
  }
  return(x)
}