screen_titles <- function(x){

  if(missing(x)){x <- NULL}
  if(!is.null(x)){

    # throw a warning if a known file type isn't given
    accepted_inputs <- c("bibliography", "data.frame")
    if(!any(accepted_inputs == class(x))){
      stop("only classes 'bibliography' or 'data.frame' accepted by screen_visual")}

      if(class(x) == "bibliography"){
        x <- as.data.frame(x)
      }
  }

  # create ui
  ui_data <- screen_titles_ui()
  ui <- shinydashboard::dashboardPage(
    title = "revtools",
  	ui_data$header,
  	ui_data$sidebar,
  	ui_data$body,
  	skin = "black"
  )

  # start server
  server <- function(input, output, session){

    # BUILD REACTIVE VALUES
    data <- reactiveValues(
      raw = x,
      current = NULL,
      n_current = NULL,
      n_previous = NULL
    )
    # view <- reactiveValues(current = NULL)
    click_values <- reactiveValues(
      yes = NULL,
      no = NULL,
      maybe = NULL,
      group = c(0)
    )
    selector <- reactiveValues(
      yes = c(0),
      no = c(0),
      maybe = c(0)
    )

    # CREATE HEADER IMAGE
    output$header <- renderPlot({
      revtools_logo(text = "screen_titles")
    })

    # DATA INPUT
    ## when specified, ensure input data is processed correctly
    observeEvent(input$data_in, {
      if(is.null(data$raw)){
        data_previous <- x
      }else{
        data_previous <- data$raw
      }
      import_result <- import_shiny(
        source = input$data_in,
        current_data = data_previous
      )

      # create citation
      import_result$citation <- format_citation(
        data = import_result,
        details = (input$hide_names == FALSE),
        add_html = TRUE
      )

      # add extra columns as needed
      if(any(colnames(import_result) == "selected") == FALSE){
        import_result$selected <- NA
      }
      if(any(colnames(import_result) == "notes") == FALSE){
        import_result$notes <- NA
      }
      import_result$color <- "#000000"

      # save progress
      data$raw <- import_result
      data$n_current <- min(c(
        input$n_citations,
        length(which(is.na(import_result$selected)))
      ))
      data$current <- seq_len(data$n_current)
      selector$yes <- rep(0, data$n_current)
      selector$no <- rep(0, data$n_current)
      selector$maybe <- rep(0, data$n_current)
    })


    # add selector button to generate next set of texts
    output$next_group <- renderUI({
      if(!is.null(data$raw)){
        actionButton(
          inputId = "next_group_activated",
          label = "Next Group",
          width = "246px" # boxes are 80px each + 3px gap
        )
      }
    })

    # add progress indicator
    output$progress_text <- renderPrint({
      if(is.null(data$raw)){
        cat("")
      }else{
        n_progress <- length(which(is.na(data$raw$selected) == FALSE))
        n_total <- nrow(data$raw)
        cat(paste0(
          "<b>Progress:</b> ",
          n_progress,
          " of ",
          n_total,
          " categorized"
        ))
      }
    })

    # update text info to hide/show authors
    observeEvent(input$hide_names, {
      if(!is.null(data$raw)){
        data$raw$citation <- format_citation(
          data = data$raw,
          details = (input$hide_names == FALSE),
          add_html = TRUE
        )
      }
    })

    # fix bug whereby input$next_group_activated gets reset to zero,
    # resetting the text
    observeEvent(input$next_group_activated, {
      if(input$next_group_activated > 0){
        click_values$group <- click_values$group + 1
      }
    })

    # update list of currently selected texts
    observeEvent({
      click_values$group
      input$n_citations
      input$order
      }, {
      data$n_previous <- data$n_current
      remaining_values <- which(is.na(data$raw$selected))
      data$n_current <- min(c(
        input$n_citations,
        length(remaining_values)
      ))
      data$current <- switch(input$order,
        "order_initial" = {remaining_values},
        "order_alphabetical" = {remaining_values[order(data$raw$title[remaining_values])]},
        "order_random" = {remaining_values[order(rnorm(length(remaining_values)))]}
      )[seq_len(data$n_current)]

    })


    # ADD/SUBTRACT & RENDER TEXT BOXES
    observeEvent({
      data$current
      data$raw
    }, {
      if(!is.null(data$raw)){

        # add or subtract the requisite number of entries
        if(is.null(data$n_previous)){
          add_logical <- TRUE
          remove_logical <- FALSE
          add_values <- seq_len(data$n_current)
          remove_values <- NULL
        }else{
          if(data$n_previous > data$n_current){
            add_logical <- FALSE
            remove_logical <- TRUE
            add_values <- NULL
            remove_values <- seq_len(data$n_previous)[-seq_len(data$n_current)]
          }
          if(data$n_previous == data$n_current){
            add_logical <- FALSE
            remove_logical <- FALSE
            add_values <- NULL
            remove_values <- NULL
          }
          if(data$n_previous < data$n_current){
            add_logical <- TRUE
            remove_logical <- FALSE
            add_values <- seq_len(data$n_current)[-seq_len(data$n_previous)]
            remove_values <- NULL
          }
        }

        if(remove_logical){
          lapply(remove_values, function(a){
            removeUI(
              selector = paste0("#citation_", a)
            )
          })
          remove_logical <- FALSE
          data$n_previous <- data$n_current
        }

        if(add_logical){
          lapply(add_values, function(a, data){
            add_reference_ui(
              entry_number = a,
              ui_selector = "placeholder"
            )
          },
          data = data$raw
          )
          add_logical <- FALSE
          data$n_previous <- data$n_current
        }

        # render text for each reference
        lapply(seq_len(data$n_current), function(a, data){
          output[[paste0(
            "citation_",
            a,
            "_render"
          )]] <- renderPrint({
            cat(paste0(
              "<font color = ",
              data$color[a],
              ">",
              data$citation[a],
              "</font><br><br>"
            ))
          })
        },
        data = data$raw[data$current, ]
        )

      }
    })


    # TRACK ARTICLE SELECTIONS
    observe({

      # track whether 'yes' buttons are hit
      click_values$yes <- input_tracker(
        input = input,
        string = "citation_[[:digit:]]+_yes"
      )
      if(nrow(click_values$yes) == length(selector$yes)){
        update_check <- (click_values$yes$value > selector$yes)
        if(any(update_check)){
          data$raw$selected[data$current[which(update_check)]] <- "selected"
          data$raw$color[data$current[which(update_check)]] <- "#405d99"
          selector$yes <- click_values$yes$value
        }
      }

      # duplicate for 'no'
      click_values$no <- input_tracker(
        input = input,
        string = "citation_[[:digit:]]+_no"
      )
      if(nrow(click_values$no) == length(selector$no)){
        update_check <- (click_values$no$value > selector$no)
        if(any(update_check)){
          data$raw$selected[data$current[which(update_check)]] <- "excluded"
          data$raw$color[data$current[which(update_check)]] <- "#993f3f"
          selector$no <- click_values$no$value
        }
      }

      # and finally for 'maybe'
      click_values$maybe <- input_tracker(
        input = input,
        string = "citation_[[:digit:]]+_maybe"
      )
      if(nrow(click_values$maybe) == length(selector$maybe)){
        update_check <- (click_values$maybe$value > selector$maybe)
        if(any(update_check)){
          data$raw$selected[data$current[which(update_check)]] <- "unknown"
          data$raw$color[data$current[which(update_check)]] <- "#6d6d6d"
          selector$maybe <- click_values$maybe$value
        }
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
        filename <- "revtools_title_screening"
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
      lapply(seq_len(data$n_current), function(a){
        removeUI(
          selector = paste0("#citation_", a)
        )
      })
      data$raw <- NULL
      data$current <- NULL
      data$n_current <- NULL
      data$n_previous <- NULL
      removeModal()
    })


  } # end server

  shinyApp(ui, server)

}