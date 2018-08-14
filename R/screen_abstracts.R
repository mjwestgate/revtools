# NOTE: next stesp:
  # make screen_titles() & screen_abstracts(), as these are always separate tasks
  # make screen_visual() a wrapper function for these, with option type = "titles" or similar

screen_abstracts <- function(x){

  if(missing(x)){x <- NULL}
  if(!is.null(x)){

    # throw a warning if a known file type isn't given
    accepted_inputs <- c("bibliography", "data.frame")
    if(any(accepted_inputs == class(x)) == FALSE){
      stop("only classes 'bibliography' or 'data.frame' accepted by screen_visual")}

    switch(class(x),
      "bibliography" = {x <- as.data.frame(x)},
      "data.frame" = {x <- x}
    )
  }

  # create ui
  ui_data <- screen_abstracts_ui()
  ui <- shinydashboard::dashboardPage(
    title = "revtools | screen_abstracts",
  	ui_data$header,
  	ui_data$sidebar,
  	ui_data$body,
  	skin = "black"
  )

  # start server
  server<-function(input, output, session){

    # BUILD REACTIVE VALUES
    data <- reactiveValues(raw = NULL)
    progress <- reactiveValues(
      abstract_sequence = NULL,
      abstract_current = NULL
    )

    # CREATE HEADER IMAGE
    output$header <- renderPlot({
      revtools_logo(text = "screen_abstracts")
    })

    # DATA INPUT
    ## when specified, ensure input data is processed correctly
    observeEvent(input$data_in, {
      source <- input$data_in
      is_csv <- grepl(".csv$", source$name)
      if(is.null(x)){
        if(is.null(source)){
          x <- NULL
        }else{
          if(is_csv){
            x <- read.csv(source$datapath, stringsAsFactors = FALSE)
          }else{
            x <- as.data.frame(read_bibliography(source$datapath))
          }
        }
      }else{
        if(is.null(source)){
          x <- x
        }else{
          if(is_csv){
            x <- merge_columns(
              x,
              read.csv(source$datapath, stringsAsFactors = FALSE)
            )
          }else{
            x <- merge_columns(
              x,
              as.data.frame(read_bibliography(source$datapath))
            )
          }
        }
      }
      # set order columns
      x$order_initial <- c(1:nrow(x))
      if(any(colnames(x) == "title")){
        x$order_alphabetical <- rank(x$title)
      }else{
        x$order_alphabetical <- x$order_initial
      }
      x$order_random <- rank(rnorm(nrow(x)))
      # note: these columns should be deleted in exported data

      if(any(colnames(x) == "selected") == FALSE){
        x$selected <- NA
      }
      if(any(colnames(x) == "notes") == FALSE){
        x$notes <- NA
      }
      data$raw <- x
      progress$abstract_sequence <- rep(NA, length(which(is.na(x$selected))))
      progress$abstract_sequence[1] <- which(x[which(is.na(x$selected)), input$order] == 1)
      progress$abstract_current <- 1

    })

    # ABSTRACT SCREENING
    # change order of articles as necessary
    observeEvent(input$order, {
      progress$abstract_sequence[progress$abstract_current] <- which.min(
        data$raw[which(is.na(data$raw$selected)), input$order]
      )
    })

    # display text for the current entry
    # note that observe is necessary to force changes when input$order changes
    observe({
      output$citation <- renderPrint({
        if(!is.null(data$raw)){
          cat(
            format_citation(
              data$raw[progress$abstract_sequence[progress$abstract_current], ],
              abstract = FALSE,
              details = (input$hide_names == FALSE)
            )
          )
        }
      })

      output$abstract_text <- renderPrint({
        if(!is.null(data$raw)){
          if(any(colnames(data$raw) == "abstract")){
            cat(data$raw$abstract[
              progress$abstract_sequence[progress$abstract_current]
            ])
          }else{
            cat("No abstracts available")
          }
        }else{
          cat("No data available")
        }
      })
    })

    # record & respond to user inputs
    observeEvent(input$abstract_save, {
      data$raw$selected[progress$abstract_current] <- input$abstract_selector
      data$raw$notes[progress$abstract_current] <- input$abstract_notes
      progress$abstract_current <- progress$abstract_current + 1
      progress$abstract_sequence[progress$abstract_current] <- which.min(
        data$raw[which(is.na(data$raw$selected)), input$order]
      )
    })

    observeEvent(input$abstract_next, {
      data$raw$selected[progress$abstract_current] <- "skipped"
      progress$abstract_current <- progress$abstract_current + 1
      progress$abstract_sequence[progress$abstract_current] <- which.min(
        data$raw[which(is.na(data$raw$selected)), input$order]
      )
    })

    observeEvent(input$abstract_previous, {
      progress$abstract_current <- progress$abstract_current - 1
    })


  } # end server

  shinyApp(ui, server)

}