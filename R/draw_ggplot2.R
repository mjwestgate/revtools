# functions to draw ggplots from stm models
# where x is a model of class stm

plot_articles <- function(x){
  df <- build_article_scatterplot_data(x)
  p <- ggplot(df, aes(x = Axis1, y = Axis2, color = topic)) +
    geom_point() +
    scale_colour_viridis(option = "magma") +
    theme_bw()
  p
}

plot_topics <- function(x){
  df <- build_topic_barplot_data(x)
  p <- ggplot(df, aes(x = topic, y = n, fill = topic)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis(option = "magma") +
    theme_bw()
  p
}

plot_words <- function(x){
  df <- build_word_barplot_data(x)
  df$term_factor <- factor(seq_len(nrow(df)), levels = seq_len(nrow(df)), labels = df$term)
  p <- ggplot(df, aes(x = value, y = term_factor, fill = topic)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(option = "magma") +
    facet_wrap(vars(topic), scales = "free_y") +
    coord_cartesian(xlim = c(min(df$value), max(df$value))) +
    theme_bw()
  p
}