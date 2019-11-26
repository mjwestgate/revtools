plot_2D <- function(input_info, color, pointsize = 10) { #, height = 400){
  p <- plotly::plot_ly(input_info,
    x = ~ Axis1,
    y = ~ Axis2,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = pointsize,
      color = color
    ),
    hoverinfo = "text",
    hoverlabel = list(
      bgcolor = grey(0.9),
      bordercolor = grey(0.9),
      namelength = 200,
      font = list(color = "black")
    ),
    text = ~ caption,
    source = "main_plot"
    ) %>%
    plotly::add_markers() %>%
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(showticklabels=FALSE),
      yaxis = list(showticklabels=FALSE)
    )
  p
}


plot_3D <- function(input_info, color, pointsize = 6){ #, height = 400){
  p <- plotly::plot_ly(input_info,
    x = ~ Axis1,
    y = ~ Axis2,
    z = ~ Axis3,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = pointsize,
      color = color
    ),
    hoverinfo = "text",
    hoverlabel = list(
      bgcolor = grey(0.9),
      bordercolor = grey(0.9),
      namelength = 200,
      font = list(color = "black")
    ),
    text = ~ caption,
    source = "main_plot"
    ) %>%
    plotly::add_markers() %>%
    plotly::layout(
      showlegend = FALSE,
      scene = list(
        xaxis = list(showticklabels=FALSE),
        yaxis = list(showticklabels=FALSE),
        zaxis = list(showticklabels=FALSE)
      )
    )
  p
}


plot_topics <- function(x, n, color, source){
  p <- plotly::plot_ly(x,
    x = n,
    y = ~ topic,
    marker = list(color = color),
    hoverinfo = "text",
    hoverlabel = list(
      bgcolor = grey(0.9),
      bordercolor = grey(0.9),
      namelength = 200,
      font = list(color = "black")
    ),
    text = ~ caption,
    source = source,
    type = "bar",
    orientation = "h"
    ) %>%
    plotly::layout(
      xaxis = list(title = "Count"),
      yaxis = list(
        title = "Topic",
        tick0 = 1,
        dtick = 1
      )
    )
  p
}


plot_words <- function(
  input_info,
  color
){
  p <- plotly::plot_ly(
    data = input_info,
    x = ~ topic_weight,
    y = ~ term,
    marker = list(
      color = color
    ),
    hoverinfo = "text",
    hoverlabel = list(
      bgcolor = grey(0.9),
      bordercolor = grey(0.9),
      namelength = 200,
      font = list(color = "black")
    ),
    text = ~ caption,
    source = "word_plot",
    type = "bar",
    orientation = "h"
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "Weight"
      ),
      yaxis = list(
        title = "",
        tick0 = 1,
        dtick = 1
      ),
      margin = list(
        l = 100,
        r = 10,
        b = 50,
        t = 10,
        pad = 4
      )
    )
  p
}