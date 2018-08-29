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
    title = "revtools | screen_duplicates",
  	ui_data$header,
  	ui_data$sidebar,
  	ui_data$body,
  	skin = "black"
  )

  # start server
  server <- function(input, output, session){

    # BUILD REACTIVE VALUES
    data <- reactiveValues(
      raw = NULL,
      columns = NULL,
      match = NULL,
      grouped = NULL
    )

    progress <- reactiveValues(
      entry = NULL,
      rows = NULL
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
      # ensure rownames are unique
      if(any(colnames(import_result) == "label")){
        import_result$label <- make.unique(
          names = import_result$label,
          sep = "_"
        )
      }else{
        import_result$label <- paste0(
          "v",
          seq_len(nrow(import_result))
        )
      }
      # find a way to track progress
      import_result$display <- TRUE
      # save in reactiveValues
      data$raw <- import_result
      data$columns <- colnames(import_result)[
        which(colnames(import_result) != "display")
      ]
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
        if(input$match_function != "exact"){
          selectInput(
            inputId = "match_algorithm",
            label = "Select method",
            choices = algorithm_list
          )
        }
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
        if(input$match_function != "exact"){
          sliderInput(
            inputId = "match_threshold",
            label = "Select maximum distance",
            min = 0,
            max = max_val,
            value = initial_val,
            step = step_val
          )
        }
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
        data$match <- find_duplicates(
          data = data$raw,
          match_variable = input$response_selector_result,
          group_variables = input$group_selector_result,
          match_function = input$match_function,
          method = input$match_algorithm,
          threshold = input$match_threshold,
          to_lower = input$match_lower,
          remove_punctuation = input$match_punctuation
        )

        # work out which duplicates to show
        group_result <- split(data$raw, data$match)
        group_result <- group_result[
          which(unlist(lapply(group_result, nrow)) > 1)
        ]
        if(length(group_result) > 0){
          progress$entry <- 1
          progress$rows <- c(1, 2)
          data$grouped <- group_result
        }else{
          progress$entry <- NULL
          progress$rows <- NULL
        }
      }
    })

    # summary text
    output$match_summary <- renderPrint({
      validate(
        need(data$raw, "Import data to continue")
      )
      if(is.null(data$grouped)){
        cat(
          paste0("<h4>Dataset with ", nrow(data$raw), " entries</h4>")
        )
      }else{
        cat(
          paste0("<h4>Dataset with ",
            nrow(data$raw),
            " entries  |  ",
            length(data$grouped),
            " duplicates remaining</h4>"
          )
        )
      }
    })

    # action buttons
    output$selector_1 <- renderUI({
      if(!is.null(progress$entry)){
        actionButton(
          inputId = "selected_1",
          label = "Select Entry #1",
          width = "100%"
        )
      }
    })
    output$selector_2 <- renderUI({
      if(!is.null(progress$entry)){
        actionButton(
          inputId = "selected_2",
          label = "Select Entry #2",
          width = "100%"
        )
      }
    })
    output$selector_none <- renderUI({
      if(!is.null(progress$entry)){
        actionButton(
          inputId = "selected_none",
          label = "Not duplicates",
          width = "100%",
          style = "background-color: #c17c7c;"
        )
      }
    })
    output$selector_previous <- renderUI({
      if(!is.null(progress$entry)){
        actionButton(
          inputId = "selected_previous",
          label = "Previous",
          width = "100%",
          style = "background-color: #6b6b6b;"
        )
      }
    })
    output$selector_next <- renderUI({
      if(!is.null(progress$entry)){
        actionButton(
          inputId = "selected_next",
          label = "Next",
          width = "100%",
          style = "background-color: #6b6b6b;"
        )
      }
    })

    # text blocks
    output$text_1 <- renderPrint({
      validate(
        need(progress$entry, "")
      )
      format_duplicates(x = data$grouped[[progress$entry]][progress$rows[1], ])
    })
    output$text_2 <- renderPrint({
      validate(
        need(progress$entry, "")
      )
      format_duplicates(x = data$grouped[[progress$entry]][progress$rows[2], ])
    })

    # respond when actionButtons are triggered
    observeEvent(input$selected_1, {
      # label_keep <- data$grouped[[progress$entry]]$label[progress$row[1]]
      label_exclude <- data$grouped[[progress$entry]]$label[progress$rows[2]]
      data$raw$display[which(data$raw$label == label_exclude)] <- FALSE
      if(nrow(data$grouped[[progress$entry]]) > 2){
        data$grouped[[progress$entry]] <- data$grouped[[progress$entry]][-progress$rows[2]]
      }else{
        data$grouped <- data$grouped[-progress$entry]
      }
    })

    observeEvent(input$selected_2, {
      label_exclude <- data$grouped[[progress$entry]]$label[progress$rows[1]]
      data$raw$display[which(data$raw$label == label_exclude)] <- FALSE
      if(nrow(data$grouped[[progress$entry]]) > 2){
        data$grouped[[progress$entry]] <- data$grouped[[progress$entry]][-progress$rows[2]]
      }else{
        data$grouped <- data$grouped[-progress$entry]
      }
    })

    observeEvent(input$selected_previous, {
      if(progress$entry > 1){
        progress$entry <- progress$entry - 1
      }
    })

    observeEvent(input$selected_next, {
      if((progress$entry + 1) <= length(data$grouped)){
        progress$entry <- progress$entry + 1
      }
    })

  } # end server

  shinyApp(ui, server)

}