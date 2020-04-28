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
  # source(
  #   system.file("inst/servers/screen_abstracts_preloaded_server.R", package = "revtools"),
  #   local = TRUE
  # )
  # fails on installed version
  
  # start server
  screen_abstracts_preloaded_server <- function(input, output, session){

    output$header <- renderPlot({
      revtools_logo(text = "screen_abstracts_preloaded")
    })

    # build reactive values
    data <- reactiveValues(
      raw = data_in$data$raw
    )
    progress <- reactiveValues(
      row = data_in$progress$row,
      time = Sys.time()
    )

    # DISPLAY TEXT
    observe({
      output$citation <- renderPrint({
        if(any(colnames(data$raw) == "abstract")){
          abstract_text <- data$raw$abstract[progress$row]
        }else{
          abstract_text <- "<em>No abstract available</em>"
        }
        current_status <- data$raw$screened_abstracts[progress$row]
        if(is.na(current_status)){
          text_label <- ""
        }else{
          if(current_status == "excluded"){
            text_label <- "Status: Excluded<br>"
          }else{
            text_label <- "Status: Selected<br>"
          }
        }
        citation <- format_citation(
          data$raw[progress$row, ],
          abstract = FALSE,
          details = app_control$show_identifying_info,
          add_html = TRUE
        )
        citation <- gsub("\\.(\\s*)$", "", citation)
        text_tr <- paste0(
          "<br><font size='5'><b>",
          citation,
          "</b></font><br>",
          text_label,
          "<br>",
          abstract_text
        )
        if(app_control$keyword_highlighting & sum(nchar(app_control$keywords)) > 0){
          for(i in seq_along(app_control$keywords)){
            # look-ahead
            font_start <- paste0(
              " <font color='", app_control$highlight_color, "'>"
            )
            text_tr <- gsub(
              pattern = paste0(" (?=", app_control$keywords[i], ")"),
              replacement = font_start,
              x = text_tr,
              perl = TRUE,
              ignore.case = TRUE
            )
            # look behind
            text_tr <- gsub(
              pattern = paste0("(?<=", app_control$keywords[i], ")"),
              replacement = "</font>",
              x = text_tr,
              perl = TRUE,
              ignore.case = TRUE
            )
          }
        }
        cat(text_tr) # print
        progress$time <- Sys.time()
      })
    })

    # RENDER SELECTION BUTTONS
    output$selector_bar <- renderUI({
      if(!is.null(data$raw)){
        text_out <- HTML(
          paste0(
            "Entry ",
            progress$row,
            " of ",
            nrow(data$raw),
            " | ",
            length(which(data$raw$screened_abstracts == "selected")),
            " selected | ",
            length(which(data$raw$screened_abstracts == "excluded")),
            " excluded | ",
            length(which(data$raw$screened_abstracts == "unknown")),
            " unknown"
          )
        )
        abstract_selector_buttons(text_out, text_width = "400px")
      }
    })

    # NOTES
    # render notes - necessary to do in server so inputs can be viewed/editted
    output$render_notes <- renderUI({
      textAreaInput(
        inputId = "abstract_notes",
        label = "Notes",
        value = data$raw$notes[progress$row],
        resize = "both",
        width = "600px",
        height = "50px"
      )
    })

    # SELECTION & NAVIGATION
    observeEvent(input$select_yes, {
      data$raw$time_taken[progress$row] <- as.numeric(Sys.time() - progress$time)
      data$raw$screened_abstracts[progress$row] <- "selected"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$date_time[progress$row] <- as.character(Sys.time())
      }
      if(progress$row > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$select_unknown, {
      data$raw$time_taken[progress$row] <- as.numeric(Sys.time() - progress$time)
      data$raw$screened_abstracts[progress$row] <- "unknown"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$date_time[progress$row] <- as.character(Sys.time())
      }
      if(progress$row > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$select_no, {
      data$raw$time_taken[progress$row] <- as.numeric(Sys.time() - progress$time)
      data$raw$screened_abstracts[progress$row] <- "excluded"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$date_time[progress$row] <- as.character(Sys.time())
      }
      if(progress$row > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$abstract_next, {
      data$raw$notes[progress$row] <- input$abstract_notes
      if((progress$row + 1) > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$abstract_previous, {
      data$raw$notes[progress$row] <- input$abstract_notes
      if((progress$row - 1) > 0){
        progress$row <- progress$row - 1
      }
    })

    observeEvent(input$abstract_10previous, {
      data$raw$notes[progress$row] <- input$abstract_notes
      if((progress$row - 10) > 0){
        progress$row <- progress$row - 10
      }else{
        progress$row <- 1
      }
    })

    observeEvent(input$abstract_10next, {
      data$raw$notes[progress$row] <- input$abstract_notes
      if((progress$row + 10) > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 10
      }
    })

    # SAVE
    observeEvent(input$save_progress, {
      if(app_control$save_csv){
        write.csv(data$raw, "screening_data.csv")
      }
      app <- list(
        data = data$raw,
        file = file,
        app_control = app_control
      )
      app$app_control$rank_by = "initial"
      class(app) <- "screen_abstracts_preloaded"
      attr(app, "date_modified") <- as.character(Sys.time())
      # save in correct format
      if(grepl(".rds$", file)){
        saveRDS(app, file = file)
      }else{
        save(app, file = file)
      }
      # show user that file has been saved
      showModal(
        modalDialog(
          HTML("Your data have been saved to an .RData file in your working directory<br><br>"),
          modalButton("Continue Screening"),
          actionButton(
            inputId = "exit_app",
            label = "Close App"
          ),
          title = "Data saved to file",
          footer = NULL,
          easyClose = FALSE
        )
      )
    })

    observeEvent(input$exit_app, {stopApp()})

  } # end server

  # run app
  print(shinyApp(
    ui = screen_abstracts_preloaded_ui(),
    server = screen_abstracts_preloaded_server
  ))
}