# similar to import_shiny, but for initial load of data.
# should optionally accept a list and just build parts that are not provided
  # e.g. raw data mandatory, but dtm & model optional (but if model given then dfm must too)
load_topic_data <- function(
  data, # either a data.frame, bibliography, screen_topics_progress, or list
    # if list, optional columns are same as for x (below)
    # NOTE: This will require documentation
  stopwords
){

  if(missing(stopwords)){
    stopwords <- revwords()
  }else{
    if(is.null(stopwords)){
      stopwords <- revwords()
    }
  }

  x <- list(
    raw = NULL,
    stopwords = stopwords,
    columns = NULL,
    # grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )

  if(!is.null(data)){

    # throw a warning if a known file type isn't given
    accepted_inputs <- c(
      "bibliography",
      "data.frame",
      "screen_topics_progress",
      "list"
    )
    if(!any(accepted_inputs == class(data))){
      stop("only classes 'bibliography', 'data.frame', 'list' or
      'screen_topics_progress' accepted by screen_topics")
    }

    # add data as necessary for that file type
    switch(class(data),
      "bibliography" = {
        result <- as.data.frame(data)
        colnames(result) <- clean_colnames(colnames(result))
        x$raw <- add_required_columns(result)
      },
      "data.frame" = {
        colnames(data) <- clean_colnames(colnames(data))
        x$raw <- add_required_columns(data)
      },
      "screen_topics_progress" = {
        x$raw <- data$raw
        x$stopwords <- data$stopwords
        x$columns <- data$columns
        # x$grouped <- data$grouped
        x$dtm <- data$dtm
        x$model <- data$model
        x$plot_ready <- data$plot_ready
      },
      "list" = {
        x_names <- names(x)
        x <- lapply(x_names, function(a){
          if(any(names(data) == a)){data[[a]]}else{x[[a]]}})
        names(x) <- x_names
        colnames(x$raw) <- clean_colnames(colnames(x$raw))
        x$raw <- add_required_columns(x$raw)
        if(all(c("dtm", "model") %in% names(x))){
          x$plot_ready <- build_plot_data(
            info = x$raw,
            dtm = x$dtm,
            model = x$model,
            hide_names = TRUE
          )
        }
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


add_required_columns <- function(data){
  added_cols <- data.frame(
    screened_topics = NA,
    display = TRUE,
    topic = NA,
    notes = NA
  )
  col_check <- colnames(added_cols) %in% colnames(data)
  if(any(!col_check)){
    data <- as.data.frame(
      cbind(
        data,
        added_cols[, which(!col_check)]
      ),
      stringsAsFactors = FALSE
    )
  }
  return(data)
}


# function to cleanly extract colnames of interest from supplied datasets
get_topic_colnames <- function(data){
  colnames(data)[
    which(
      !(colnames(data) %in%
      c("screened_topics", "screened_titles", "screened_abstracts",
        "topic", "display", "notes"
      ))
    )
  ]
}


build_appearance <- function(plot_data, palette){
  lapply(plot_data, function(a, colours){
    result <- data.frame(
      # id = a[, 1],
      topic = a$topic,
      color = palette[a$topic],
      text_color = "#000000",
      stringsAsFactors = FALSE
    )
    return(result)
  },
  colours = palette)
}


update_appearance <- function(plot_data, palette){
  lapply(plot_data, function(a, colours){
    rows <- which(
      !(a$color %in% c("#000000", "#CCCCCC"))
    )
    a$color[rows] <- colours[a$topic[rows]]
    return(a)
    },
    colours = palette
  )
}