preload_screen_abstracts_build <- function(
  x, # data
  file_out = "screen_abstracts_preloaded.RData", # file name
  app_control
){

  if(missing(app_control)){
    app_control <- validate_app_control()
  }

  # load data
  data_in <- load_abstract_data_remote(
    data = x[
      set_row_order(x,
        order_by = app_control$rank_by,
        keywords = app_control$keywords
      ),
    ],
    time_responses = app_control$time_responses
  )

  # start server
  app_server <- function(input, output, session){

    output$header <- renderPlot({
      revtools_logo(text = "screen_abstracts_preloaded")
    })

    # build reactive values
    data <- reactiveValues(
      raw = data_in$data$raw
    )
    progress <- reactiveValues(
      row = data_in$progress$row
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
      data$raw$screened_abstracts[progress$row] <- "selected"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$time[progress$row] <- as.character(Sys.time())
      }
      if(progress$row > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$select_unknown, {
      data$raw$screened_abstracts[progress$row] <- "unknown"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$time[progress$row] <- as.character(Sys.time())
      }
      if(progress$row > nrow(data$raw)){
        progress$row <- nrow(data$raw)
      }else{
        progress$row <- progress$row + 1
      }
    })

    observeEvent(input$select_no, {
      data$raw$screened_abstracts[progress$row] <- "excluded"
      data$raw$notes[progress$row] <- input$abstract_notes
      if(app_control$time_responses){
        data$raw$time[progress$row] <- as.character(Sys.time())
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
      app_control_out <- app_control
      app_control_out$rank_by <- "initial"
      screen_abstracts_preloaded <- preload_screen_abstracts_build(
        data$raw,
        app_control_out
      )
      attr(screen_abstracts_preloaded, "date_last_editted") <- as.character(Sys.time())
      save(screen_abstracts_preloaded, file = file_out)
      showModal(
        modalDialog(
          HTML("Click anywhere to continue screening, or return to R and hit esc to close the app"),
          title = "Data saved to file",
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

  } # end server

  result <- shinyApp(preload_screen_abstracts_ui(), app_server)
  return(result)
}