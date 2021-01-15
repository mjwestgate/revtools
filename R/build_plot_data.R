# calculate 'best topic' per article from an object returned by stm::make.dt
get_best_topic <- function(x_mat){
  apply(x_mat[, c(2:ncol(x_mat))], 1, which.max)
}


# use ade4 to create scatterplot from article x topic matrix
# note captions aren't included as they require data from other sources (raw data  + dtm)
build_article_scatterplot_data <- function(model){
  x_matrix <- as.data.frame(stm::make.dt(model))
  x_df <- data.frame(docnum = x_matrix$docnum)
  x_df <- cbind(x_df, data.frame(ade4::dudi.coa(x_matrix[, -1], scannf = FALSE, nf = 3)$li))
  k <- model$settings$dim$K
  x_df$topic <- get_best_topic(x_matrix)
  return(x_df)
}


# article counts per topic + detailed caption
build_topic_barplot_data <- function(model){
  x_matrix <- as.data.frame(stm::make.dt(model))
  topic <- get_best_topic(x_matrix)
  result_df <- as.data.frame(xtabs( ~ topic))
  colnames(result_df)[2] <- "n"
  result_df$topic <- as.numeric(as.character(result_df$topic))

  # add labels
  topic_labels <- stm::labelTopics(model, n = 7)
  names(topic_labels)[1:4] <- c("Highest Prob", "FREX", "Lift", "Score")
  metric_list <- lapply(seq_len(4), function(a){
    df <- topic_labels[[a]]
    data.frame(
      topic = seq_len(nrow(df)),
      metric = names(topic_labels)[a],
      label = paste0(names(topic_labels)[a], ": ",
        apply(df, 1, function(b){paste(b, collapse = " ")})
      ))
    })
  metric_df <- as.data.frame(do.call(rbind, metric_list))
  metric_list2 <- split(metric_df, metric_df$topic)
  result_df$caption <- unlist(lapply(seq_along(metric_list2), function(a){
    paste0(
      "<b>Topic ", a, "</b><br>",
      paste(metric_list2[[a]]$label, collapse = "<br>"))
  }))
  return(result_df)
}


# detailed words per topic, for a given metric
build_word_barplot_data <- function(model, method = "frex", n_terms = 15){
  logbeta <- model$beta$logbeta[[1]]
  wordcounts <- model$settings$dim$wcounts$x
  k <- model$settings$dim$K
  result_matrix <- switch(method,
    "prob" = {t(logbeta)},
    "frex" = {stm::calcfrex(logbeta, wordcounts = wordcounts)},
    "score" = {stm::calcscore(logbeta)},
    "lift" = {stm::calclift(logbeta, wordcounts = wordcounts)})
  rownames(result_matrix) <- model$vocab
  result_list <- apply(result_matrix, 2, function(a){
    result <- a[order(a, decreasing = TRUE)[seq_len(n_terms)]]
    data.frame(
      term = names(result),
      value = as.numeric(result))
  })
  result_df <- data.frame(topic = rep(seq_len(k), each = n_terms))
  result_df <- cbind(result_df, do.call(rbind, result_list))
  return(result_df)
}


# function to ensure articles are indexed properly
get_dtm_index <- function(dtm){as.numeric(gsub("text", "", quanteda::docnames(dtm)))}

# ensure that captions are properly indexed between models and raw data
build_caption <- function(info, dtm, hide_names){
  synthesisr::format_citation(
    info[get_dtm_index(dtm), ],
    details = (hide_names == FALSE),
    line_breaks = TRUE)
}

# function used by screen_topics to accumulate plot info
build_plot_data <- function(info, dtm, model, hide_names = TRUE){

  # build main plot information (x)
  x_df <- build_article_scatterplot_data(model)
  # add citation info
  x_df$caption <- build_caption(info, dtm, hide_names)
  if(any(colnames(info) == "abstract")){
    x_df$abstract <- info$abstract[get_dtm_index(dtm)]
  }

  # return useful information
  plot_list <- list(
    x = x_df,
    y = build_word_barplot_data(model),
    topic = build_topic_barplot_data(model)
  )
  return(plot_list)
}
