screen_abstracts_preloaded_run <- function(x){
  if(!inherits(x, "screen_abstracts_preloaded")){
    stop("screen_abstracts_preloaded_run can only use objects of class 'screen_abstracts_preloaded'")
  }
  # load data
  data_in <- load_abstract_data_remote(
    data = x$data[
      set_row_order(x$data,
        order_by = x$app_control$rank_by,
        keywords = x$app_control$keywords
      ),
    ],
    time_responses = x$app_control$time_responses
  )
  file <- x$file
  app_control <- x$app_control

  # load server code internally to allow data_in to get called
  source(
    system.file("inst/servers/screen_abstracts_preloaded_server.R", package = "revtools"),
    local = TRUE
  )

  # run app
  print(shinyApp(
    ui = screen_abstracts_preloaded_ui(),
    server = screen_abstracts_preloaded_server
  ))
}