# NOTE: unclear whether y data still required here
build_plot_data <- function(info, dtm, model, hide_names){
  x_matrix <- modeltools::posterior(model)$topics # article x topic
  y_matrix <- t(modeltools::posterior(model)$terms)

  # exclude following columns: topic, select, display
  keep_cols <- which((colnames(info) %in% c("topic", "selected", "display")) == FALSE)

  # build x and y plot information
  plot_list<-list(
    x = cbind(
      info[, keep_cols],
      data.frame(
        topic = apply(x_matrix, 1, which.max),
        ade4::dudi.coa(x_matrix, scannf = FALSE, nf=3)$li
      )
    ),
    y = data.frame(
      caption = rownames(y_matrix),
      topic = apply(y_matrix, 1, which.max),
      ade4::dudi.coa(y_matrix, scannf = FALSE, nf=3)$li,
      stringsAsFactors = FALSE
    )
  )
  plot_list$x$caption <- add_line_breaks(
    format_citation(plot_list$x, details = (hide_names == FALSE))
  )

  # add topic information
  plot_list$topic <- data.frame(
    topic = c(1:ncol(y_matrix)),
    count_x = as.data.frame(xtabs(~ apply(x_matrix, 1, which.max)))$Freq,
    count_y = as.data.frame(xtabs(~ apply(y_matrix, 1, which.max)))$Freq,
    terms_default = apply(topicmodels::get_terms(model, 5), 2,
      function(a){paste(a, collapse = ", ")}
    ),
    terms_weighted = apply(y_matrix / apply(y_matrix, 1, sum), 2,
      function(a){paste(names(sort(a, decreasing=TRUE))[1:5], collapse = ", ")}
    ),
    stringsAsFactors = FALSE
  )
  plot_list$topic$caption <- paste0(
    "Most likely: ", plot_list$topic$terms_default,
    "\nHighest weighted: ", plot_list$topic$terms_weighted
  )
  # return
  return(plot_list)
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