build_plot_data <- function(info, dtm, model, hide_names){
  x_matrix <- modeltools::posterior(model)$topics # article x topic
  y_matrix <- t(modeltools::posterior(model)$terms)

  # exclude following columns: topic, select, display
  keep_cols <- which((colnames(info) %in% c("topic", "selected", "display")) == FALSE)

  # build main plot information (x)
  x_df <- cbind(
    info[, keep_cols],
    data.frame(
      topic = apply(x_matrix, 1, which.max),
      ade4::dudi.coa(x_matrix, scannf = FALSE, nf=3)$li
    )
  )

  x_df$caption <- paste0(
    add_line_breaks(
      format_citation(
        data = x_df,
        details = (hide_names == FALSE)
      )
    ),
    "<br>[Topic #",
    x_df$topic,
    "]"
  )

  # build word plot information (y)
  y_df <- data.frame(
    term = rep(
      x = colnames(dtm),
      times = model@k
    ),
    n = rep(
      x = apply(dtm, 2, sum),
      times = model@k
    ),
    topic = rep(
      x = seq_len(model@k),
      each = ncol(dtm)
    ),
    topic_weight = as.numeric(y_matrix),
    stringsAsFactors = FALSE
  )
  y_df$caption <- paste(
    "Term:", y_df$term, "<br>Sample size = ", y_df$n,
    sep = " "
  )
  y_list <- lapply(
    split(y_df, y_df$topic),
    function(a){
      a[order(a$topic_weight, decreasing = TRUE)[1:50], ]
    }
  )
  y_df <- as.data.frame(
    do.call(rbind, y_list),
    stringsAsFactors = FALSE
  )
  y_df$selected <- TRUE

  # add topic information
  topic_df <- data.frame(
    topic = seq_len(
      ncol(y_matrix)
    ),
    n = as.numeric(
      xtabs(~ topicmodels::topics(model))
    ),
    terms_default = apply(
      topicmodels::terms(model, 5),
      2,
      function(a){paste(a, collapse = ", ")}
    ),
    terms_weighted = apply(
      y_matrix / apply(y_matrix, 1, sum), 2,
      function(a){paste(
        names(
          sort(a, decreasing = TRUE)
        )[1:5],
        collapse = ", "
      )}
    ),
    stringsAsFactors = FALSE
  )
  topic_df$caption <- paste0(
    "Most likely: ",
    topic_df$terms_default,
    "\nHighest weighted: ",
    topic_df$terms_weighted
  )

  # return
  plot_list <- list(
    x = x_df,
    y = y_df,
    topic = topic_df
  )

  return(plot_list)
}


add_required_columns <- function(data){
  added_cols <- data.frame(
    selected = NA,
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


build_appearance <- function(plot_data, palette){
  lapply(plot_data, function(a, colours){
    data.frame(
      id = a[, 1],
      topic = a$topic,
      color = palette[a$topic],
      stringsAsFactors = FALSE
    )
  }, colours = palette)
}


update_appearance <- function(plot_data, palette){
  lapply(plot_data, function(a, colours){
    rows <- which(
      (a$color %in% c("#000000", "#CCCCCC")) == FALSE
    )
    a$color[rows] <- colours[a$topic[rows]]
    return(a)
    },
    colours = palette
  )
}