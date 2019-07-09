load_abstract_data <- function(data){

  x <- list(
    data = list(
      raw = NULL
    ),
    progress = list(
      current = 1,
      row = NULL
    )
  )

  if(!is.null(data)){

    # throw a warning if a known file type isn't given
    accepted_inputs <- c("bibliography", "data.frame")
    if(any(accepted_inputs == class(data)) == FALSE){
      stop("only classes 'bibliography' or 'data.frame' accepted by screen_abstracts")}

    switch(class(data),
      "bibliography" = {data <- as.data.frame(data)},
      "data.frame" = {data <- data}
    )

    data <- add_abstract_columns(data)
    colnames(data) <- tolower(colnames(data))
    x$data$raw <- data
    x$progress$row <- which(data[, "order_random"] == 1)

  } # end if is.null

  return(x)

}


add_abstract_columns <- function(df){

  # set order columns
  if(!any(colnames(df) == "order_initial")){
    df$order_initial <- seq_len(nrow(df))
  }
  if(!any(colnames(df) == "order_alphabetical")){
    if(any(colnames(df) == "title")){
      df$order_alphabetical <- base::rank(
        df$title,
        ties.method = "random"
      )
    }else{
      df$order_alphabetical <- df$order_initial
    }
  }
  if(!any(colnames(df) == "order_random")){
    df$order_random <- base::rank(
      rnorm(nrow(df)),
      ties.method = "random"
    )
  }
  if(!any(colnames(df) == "order_selected")){
    df$order_selected <- df$order_random
  }

  # set display/save columns
  df$color <- "#000000"
  if(!any(colnames(df) == "selected")){
    df$selected <- NA
  }
  if(!any(colnames(df) == "notes")){
    df$notes <- ""
  }

  return(df)
}