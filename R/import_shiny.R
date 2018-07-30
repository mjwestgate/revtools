# function to rbind two data.frames with different column names/orders
merge_columns<-function(x, y){
  rbind(
    data.frame(c(x, sapply(setdiff(names(y), names(x)), function(a) NA)), stringsAsFactors=FALSE),
    data.frame(c(y, sapply(setdiff(names(x), names(y)), function(a) NA)), stringsAsFactors=FALSE)
  )
}

# import data within a shiny app
import_shiny <- function(
  source, # input$data_in
  current_data # existing data
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

  return(result)
}