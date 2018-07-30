screen_duplicates <- function(x){

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
  ui_data <- screen_duplicates_ui()
  ui <- shinydashboard::dashboardPage(
    title = "revtools",
  	ui_data$header,
  	ui_data$sidebar,
  	ui_data$body,
  	skin = "black"
  )

  # start server
  server<-function(input, output, session){

    # BUILD REACTIVE VALUES
    data <- reactiveValues(
      raw = NULL,
      columns = NULL,
      grouped = NULL
    )

    # CREATE HEADER IMAGE
    output$header <- renderPlot({
      revtools_logo(text = "screen_duplicates")
    })

    # DATA INPUT
    observeEvent(input$data_in, {
      if(is.null(data$raw)){
        data_in <- x
      }else{
        data_in <- data$raw
      }
      import_result <- import_shiny(
        source = input$data_in,
        current_data = data_in
      )
      data$raw <- import_result
      data$columns <- colnames(import_result)
    })

    # select matching variable(s)
    # i.e. matches are searched for in these columns
    output$response_selector <- renderUI({
      if(!is.null(data$columns)){
        if(any(data$columns == "title")){
          selected <- "title"
        }else{
          selected <- data$columns[1]
        }
        selectInput(
          inputId = "response_selector_result",
          label = "Select variable to match",
          choices = data$columns,
          selected = selected
        )
      }
    })

    # select grouping variable(s)
    # i.e. possible matches can only be found in matching values of these columns
    output$group_selector <- renderUI({
      if(!is.null(data$columns)){
        lookup <- data$columns %in% c("journal", "year")
        if(any(lookup)){
          selected <- data$columns[which(lookup)]
        }else{
          data$columns[1] # unclear that this is a good idea
        }
        checkboxGroupInput(
          inputId = "group_selector_result",
          label = "Select grouping variable(s)",
          choices = data$columns,
          selected = selected
        )
      }
    })

    observe({

      output$algorithm_selector <- renderUI({
        if(input$match_function == "fuzzdist"){
          algorithm_list <- list(
            "M Ratio" = "fuzz_m_ratio",
            "Partial Ratio" = "fuzz_partial_ratio",
            "Token Sort Ratio" = "fuzz_token_sort_ratio",
            "Token Set Ratio" = "fuzz_token_set_ratio"
          )
        }else{
          algorithm_list <- c(
            "osa", "lv", "dl",
            "hamming", "lcs",
            "qgram", "cosine",
            "jaccard", "jw", "soundex"
          )
        }
        selectInput(
          inputId = "match_algorithm",
          label = "Select algorithm",
          choices = algorithm_list
        )
      })
    })

    observe({
      output$threshold_selector <- renderUI({
        if(input$match_function == "stringdist"){
          max_val <- 20
          initial_val <- 5
          step_val <- 1
        }else{
          max_val <- 1
          initial_val <- 0.1
          step_val <- 0.05
        }
        sliderInput(
          inputId = "match_threshold",
          label = "Select maximum distance",
          min = 0,
          max = max_val,
          value = initial_val,
          step = step_val
        )
      })
    })


    # Calculate duplicates
    observeEvent(input$calculate_duplicates, {

      if(length(input$response_selector_result) < 1 & length(input$group_selector_result) < 1){
        if(length(input$response_selector_result) < 1){
          showModal(
            modalDialog(
              HTML("Please select a variable to match records by<br><br>
              <em>Click anywhere to exit</em>"),
              title = "Error: insufficient data",
              footer = NULL,
              easyClose = TRUE
            )
          )
        }else{
          showModal(
            modalDialog(
              HTML("Please select 1 or more variables to group records by<br><br>
              <em>Click anywhere to exit</em>"),
              title = "Error: insufficient data",
              footer = NULL,
              easyClose = TRUE
            )
          )
        }
      }else{
        data$grouped <- find_duplicates(
          data = data$raw,
          match_variable = input$response_selector_result,
          group_variables = input$group_selector_result,
          match_function = input$match_function,
          algorithm = input$match_algorithm,
          threshold = input$match_threshold
        )
      }
    })

    # plot some text
    output$test_text <- renderPrint({
      validate(
        need(data$grouped, "Import data to continue")
      )
      colnames(data$grouped)
      # xtabs(~ data$grouped$group)
    })

  } # end server

  shinyApp(ui, server)

}