load_abstract_data <- function(data){

  x <- list(
    data = list(
      raw = NULL
    ),
    progress = list(
      order = NULL,
      available = 1,
      current = 1,
      row = NULL,
      max_n = NULL
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

    # set order assuming randomness and hide_screened == TRUE
    x$progress$order <- base::rank(
      rnorm(nrow(data)),
      ties.method = "random"
    )
    x$progress$available <- which(is.na(data$screened_abstracts))
    x$progress$max_n <- length(x$progress$available)
    x$progress$row <- x$progress$available[
      which.min(
        x$progress$order[x$progress$available]
      )
    ]


  } # end if is.null

  return(x)

}


add_abstract_columns <- function(df){

  if(!any(colnames(df) == "label")){
    df$label <- generate_bibliographic_names(df)
    df <- df[, c(ncol(df), seq_len(ncol(df)-1))]
  }
  if(!any(colnames(df) == "screened_abstracts")){
    df$screened_abstracts <- NA
  }
  if(!any(colnames(df) == "notes")){
    df$notes <- ""
  }

  return(df)
}


set_row_order <- function(
  df,
  order_by, # options are: random, initial, alphabetical, user_defined
  user_column # if order_by = "user_defined", this is the column name of the user selection
){
  switch(order_by,
    "random" = {
      base::rank(
        rnorm(nrow(df)),
        ties.method = "random"
      )
    },
    "initial" = {
      seq_len(nrow(df))
    },
    "alphabetical" = {
      if(any(colnames(df) == "title")){
        base::rank(
          df$title,
          ties.method = "random"
        )
      }else{
        seq_len(nrow(df))
      }
    },
    "user_defined" = {
      base::rank(
        df[, user_column],
        ties.method = "random"
      )
    }
  )
} # end function

# set progress$row when other inputs are known
choose_abstract_row <- function(
  order_vec, # vector giving order of rows (numeric). progress$order
  available_vec, # vector showing which are available (numeric). progress$available
  current # currently selected row # progress$current
){
  ordered_vals <- order_vec[available_vec]
  selected_val <- ordered_vals[order(ordered_vals)][current]
  return(which(order_vec == selected_val))
}

# set progress$current when other inputs are known
choose_abstract_current <- function(
  order_vec, # progress$order
  available_vec, # vector showing which are available (numeric). which(is.na(data$raw$screened_abstracts))
  row # currently selected row # progress$row
){
  order_current <- order_vec[row]
  ordered_vals <- order_vec[available_vec]
  result <- which(order_vec[order(order_vec)] == order_current)
  return(result)
}