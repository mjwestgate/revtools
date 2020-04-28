screen_abstracts <- function(
  x = NULL,
  max_file_size
){

  # set file size if requested, ensuring to reset on exit
  if(!missing(max_file_size)){
    initial_file_size <- options("shiny.maxRequestSize")
    options(shiny.maxRequestSize = max_file_size * 1024^2)
    on.exit(options(initial_file_size))
  }

  # load data
  if(inherits(x, "screen_abstracts_preloaded")){
    screen_abstracts_preloaded_run(x)
  }else{
    data_in <- load_abstract_data(data = x)

    # create ui
    ui_data <- screen_abstracts_ui()
    ui <- shinydashboard::dashboardPage(
      title = "revtools | screen_abstracts",
    	ui_data$header,
    	ui_data$sidebar,
    	ui_data$body,
    	skin = "black"
    )

    # server
    source(
      system.file("R", "screen_abstracts_server.R", package = "revtools"),
      local = TRUE
    )

    # run app
    print(shinyApp(ui, screen_abstracts_server))
  }
}