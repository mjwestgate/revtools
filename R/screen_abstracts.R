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
  data_in <- load_abstract_data(
    data = x
  )

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
  server <- function(input, output, session){

    # build reactive values
    data <- reactiveValues(
      raw = data_in$data$raw
    )
    progress <- reactiveValues(
      order = data_in$progress$order,
      available = data_in$progress$available,
      current = data_in$progress$current,
      row = data_in$progress$row,
      max_n = data_in$progress$max_n
    )
    display <- reactiveValues(
      notes = FALSE,
      column = "label"
    )

    # create header image
    output$header <- renderPlot({
      revtools_logo(text = "screen_abstracts")
    })

    # DATA INPUT
    ## when specified, ensure input data is processed correctly
    observeEvent(input$data_in, {
      if(is.null(data$raw)){
        data_previous <- data_in$raw
      }else{
        data_previous <- data$raw
      }
      import_result <- import_shiny(
        source = input$data_in,
        current_data = data_previous
      )
      import_result <- add_abstract_columns(import_result)

      # export to reactiveValues
      data$raw <- import_result

      # set progress values
      progress$order <- set_row_order(
        data$raw,
        input$order,
        input$order_result
      )
      if(is.null(progress$current) | progress$current < 1){
        progress$current <- 1
      }
      if(input$hide_screened){
        progress$available <- which(is.na(data$raw$selected_abstracts))
        progress$max_n <- length(progress$available)
      }else{
        progress$max_n <- nrow(data$raw)
        progress$available <- seq_len(progress$max_n)
      }
      progress$row <- choose_abstract_row(
        progress$order, progress$available, progress$current
      )
    })

    # allow user to select order
    output$column_selector <- renderUI({
      if(input$order == "user_defined"){
        available_colnames <- colnames(data$raw)
        selectInput(
          inputId = "order_result",
          label = "Select variable to order by:",
          choices = available_colnames,
          selected = display$column
        )
      }
    })

    # ensure decisions about selected columns are retained
    observeEvent(input$order_result, {
      display$column <- input$order_result
    })

    # ABSTRACT SCREENING
    # change order of articles as necessary
    observeEvent(input$order_result_go, {
      progress$order <- set_row_order(
        data$raw,
        input$order,
        input$order_result
      )
      progress$current <- 1
      progress$row <- choose_abstract_row(
        progress$order, progress$available, progress$current
      )
    })

    # display text for the current entry
    # note that observe is necessary to force changes when input$order changes
    observe({
      output$citation <- renderPrint({
        validate(
          need(data$raw, "Import data to begin")
        )
        validate(
          need(progress$max_n > 0,
            "No unscreened data remaining\nAdd more data, or save and exit to continue")
        )
        if(any(colnames(data$raw) == "abstract")){
          abstract_text <- data$raw$abstract[progress$row]
        }else{
          abstract_text <- "<em>No abstract available</em>"
        }
        current_status <- data$raw$selected_abstracts[progress$row]
        if(is.na(current_status)){
          text_color <- "black"
          text_label <- ""
        }else{
          if(current_status == "excluded"){
            text_color <- "'#993f3f'"
            text_label <- "Status: Excluded"
          }else{
            text_color <- "'#405d99'"
            text_label <- "Status: Selected"
          }
        }
        cat(
          paste0(
            "<font color =", text_color, ">",
            format_citation(
              data$raw[progress$row, ],
              abstract = FALSE,
              details = (input$hide_names == FALSE),
              add_html = TRUE
            ),
            "<br>",
            text_label,
            "<br><br>",
           abstract_text,
           "</font>"
         )
        )
      })
    })

    # RENDER SELECTION BUTTONS
    output$selector_bar <- renderUI({
      if(!is.null(data$raw)){
        text_out <- HTML(
          paste0(
            nrow(data$raw) - length(which(is.na(data$raw$selected_abstracts))),
            " entries screened | Showing entry ",
            progress$current,
            " of ",
            progress$max_n
          )
        )

        div(
          list(
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                text-align: right;
                width: 350px",
              renderText({text_out})
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                text-align: right;
                width: 20px",
              renderText(" ")
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                width: 40px",
              actionButton(
                inputId = "abstract_10previous",
                label = "<<",
                width = "40px",
                style = "background-color: #6b6b6b;"
              )
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                width: 40px",
              actionButton(
                inputId = "abstract_previous",
                label = "<",
                width = "40px",
                style = "background-color: #6b6b6b;"
              )
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                text-align: right;
                width: 100px",
              actionButton(
                inputId = "select_yes",
                label = "Select",
                style = "
                  background-color: #7c93c1;
                  color: #fff;
                  width: 100px"
              )
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                text-align: right;
                width: 100px",
              actionButton(
                inputId = "select_no",
                label = "Exclude",
                style = "
                  background-color: #c17c7c;
                  color: #fff;
                  width: 100px"
              )
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                width: 40px",
              actionButton(
                inputId = "abstract_next",
                label = ">",
                width = "40px",
                style = "background-color: #6b6b6b;"
              )
            ),
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                width: 40px",
              actionButton(
                inputId = "abstract_10next",
                label = ">>",
                width = "40px",
                style = "background-color: #6b6b6b;"
              )
            )
          )
        )
      }
    })

    output$render_notes_toggle <- renderUI({
      if(!is.null(data$raw)){
        if(progress$max_n > 0){
          actionButton(
            inputId = "notes_toggle",
            label = "Show notes window",
            style = "
              background-color: #adadad;
              color: #fff;
              width: 200px"
          )
        }
      }
    })

    # NOTES
    # when toggle is triggered, invert display status of notes
    observeEvent(input$notes_toggle, {
      display$notes <- !display$notes
    })

    # render notes
    output$render_notes <- renderUI({
      if(display$notes){
        div(
          list(
            br(),
            textAreaInput(
              inputId = "abstract_notes",
              label = NULL,
              value = data$raw$notes[progress$row],
              resize = "both",
              width = "400px",
              height = "150px"
            ),
            actionButton(
              inputId = "notes_save",
              label = "Save Notes",
              width = "100px"
            ),
            br()
          )
        )
      }
    })

    # save notes
    observeEvent(input$notes_save, {
      data$raw$notes[progress$row] <- input$abstract_notes
    })


    # SELECTION & NAVIGATION
    observeEvent(input$select_yes, {
      data$raw$selected_abstracts[progress$row] <- "selected"
      if(input$hide_screened){ # progress$current remains the same and progress$available changes
        progress$available <- which(is.na(data$raw$selected_abstracts))
        progress$max_n <- length(progress$available)
        if(progress$current > progress$max_n){
          progress$current <- progress$max_n
        }
      }else{ # i.e. if screened elements are visible, then current is used for navigation
        if(progress$current < progress$max_n){
          progress$current <- progress$current + 1
        }
      }
    })

    observeEvent(input$select_no, {
      data$raw$selected_abstracts[progress$row] <- "excluded"
      if(input$hide_screened){
        progress$available <- which(is.na(data$raw$selected_abstracts))
        progress$max_n <- length(progress$available)
        if(progress$current > progress$max_n){
          progress$current <- progress$max_n
        }
      }else{
        if(progress$current < progress$max_n){
          progress$current <- progress$current + 1
        }
      }
    })

    observeEvent(input$abstract_next, {
      if((progress$current + 1) > progress$max_n){
        progress$current <- progress$max_n
      }else{
        progress$current <- progress$current + 1
      }
    })

    observeEvent(input$abstract_previous, {
      if((progress$current - 1) > 0){
        progress$current <- progress$current - 1
      }
    })

    observeEvent(input$abstract_10previous, {
      if((progress$current - 10) > 0){
        progress$current <- progress$current - 10
      }else{
        progress$current <- 1
      }
    })

    observeEvent(input$abstract_10next, {
      if((progress$current + 10) > progress$max_n){
        progress$current <- progress$max_n
      }else{
        progress$current <- progress$current + 10
      }
    })

    # choose then row of the next entry when progress$current is updated
    observeEvent(progress$current, {
      if(!is.null(data$raw)){
        progress$row <- choose_abstract_row(
          progress$order, progress$available, progress$current
        )
      }
    })

    # ditto if progress$available is pinged
    observeEvent(progress$available, {
      if(!is.null(data$raw)){
        progress$row <- choose_abstract_row(
          progress$order, progress$available, progress$current
        )
        progress$max_n <- length(progress$available)
      }
    })

    observeEvent(input$hide_screened, {
      if(!is.null(data$raw)){
        if(input$hide_screened){ # i.e. text were shown but are now hidden
          # ensure that - if the currently viewed row is not selected - then it stays displayed
          if(is.na(data$raw$selected_abstracts[progress$row])){
            progress$current <- choose_abstract_current(
              progress$order,
              which(is.na(data$raw$selected_abstracts)),
              progress$row
            )
            # this doesn't work at present
          }
          progress$available <- which(is.na(data$raw$selected_abstracts))
        }else{
          if(progress$current < 1){
            progress$current <- 1
          }
          progress$available <- seq_len(nrow(data$raw))
        }
      }
    })

    observeEvent(progress$max_n, {
      if(!is.null(data$raw) & progress$max_n < 1){
        showModal(
          modalDialog(
            HTML(
              "All articles have been screened. Would you like to save your progess?<br><br>
              <i>If you have specified an object in your workspace and click 'Exit App',
              your progress will be invisibly saved to that object.</i><br><br>"
            ),
            textInput("save_filename",
              label = "File Name"
            ),
            selectInput("save_data_filetype",
              label = "File Type",
              choices = c("csv", "rds")
            ),
            actionButton(
              inputId = "save_data_execute",
              label = "Save to File"
            ),
            actionButton(
              inputId = "exit_app_confirmed",
              label = "Exit App"
            ),
            modalButton("Cancel"),
            title = "Save As",
            footer = NULL,
            easyClose = FALSE
          )
        )
      }
    })

    # SAVE OPTIONS
    observeEvent(input$save_data, {
      if(is.null(data$raw)){
        showModal(
          modalDialog(
            HTML(
              "Import some data to begin<br><br>
              <em>Click anywhere to exit</em>"
            ),
            title = "Error: no data to save",
            footer = NULL,
            easyClose = TRUE
          )
        )
      }else{
        showModal(
          modalDialog(
            textInput("save_filename",
              label = "File Name"
            ),
            selectInput("save_data_filetype",
              label = "File Type",
              choices = c("csv", "rds")
            ),
            actionButton("save_data_execute", "Save"),
            modalButton("Cancel"),
            title = "Save As",
            footer = NULL,
            easyClose = FALSE
          )
        )
      }
    })

    observeEvent(input$save_data_execute, {
      if(nchar(input$save_filename) == 0){
        filename <- "revtools_abstract_screening"
      }else{
        if(grepl("\\.[[:lower:]]{3}$", input$save_filename)){
          filename <- substr(
            input$save_filename, 1,
            nchar(input$save_filename) - 4
          )
        }else{
          filename <- input$save_filename
        }
      }
      filename <- paste(filename, input$save_data_filetype, sep = ".")
      switch(input$save_data_filetype,
        "csv" = {write.csv(data$raw, file = filename, row.names = FALSE)},
        "rds" = {saveRDS(data$raw, file = filename)}
      )
      removeModal()
    })

    # add option to remove data
    observeEvent(input$clear_data, {
      shiny::showModal(
        shiny::modalDialog(
          HTML("If you proceed, all data will be removed from this window,
          including any progress you have made screening your data.
          If you have not saved your data,
          you might want to consider doing that first.<br><br>
          Are you sure you want to continue?<br><br>"
          ),
          shiny::actionButton(
            inputId = "clear_data_confirmed",
            label = "Confirm"),
          shiny::modalButton("Cancel"),
          title = "Clear all data",
          footer = NULL,
          easyClose = FALSE
        )
      )
    })

    observeEvent(input$clear_data_confirmed, {
      data$raw <- NULL
      progress$current <- 1
      progress$row <- NULL
      display$notes <- FALSE
      removeModal()
    })

    observeEvent(input$exit_app, {
      exit_modal()
    })

    observeEvent(input$exit_app_confirmed, {
      stopApp(returnValue = invisible(data$raw))
    })

  } # end server

  print(shinyApp(ui, server))

}