screen_titles <- function(
  x = NULL,
  max_file_size
){

  # set file size if requested, ensuring to reset on exit
  if(!missing(max_file_size)){
    initial_file_size <- options("shiny.maxRequestSize")
    options(shiny.maxRequestSize = max_file_size * 1024^2)
    on.exit(options(initial_file_size))
  }

  data_in <- load_title_data(data = x)

  # create ui
  ui_data <- screen_titles_ui()
  ui <- shinydashboard::dashboardPage(
    title = "revtools | screen_titles",
  	ui_data$header,
  	ui_data$sidebar,
  	ui_data$body,
  	skin = "black"
  )

  # start server
  server <- function(input, output, session){

    # APP SETUP
    # build reactive values
    data <- reactiveValues(
      raw = data_in$data$raw,
      current = data_in$data$current,
      n_current = data_in$data$n_current,
      n_previous = data_in$data$n_previous
    )
    click_values <- reactiveValues(
      yes = NULL,
      no = NULL,
      maybe = NULL,
      page = data_in$click_values$page,
      column = "label"
    )
    selector <- reactiveValues(
      yes = data_in$selector$yes,
      no = data_in$selector$no,
      maybe = data_in$selector$maybe
    )
    update <- reactiveValues(
      add_logical = FALSE,
      add_values = NULL,
      remove_logical = FALSE,
      remove_values = NULL
    )

    # create header image
    output$header <- renderPlot({
      revtools_logo(text = "screen_titles")
    })


    # DATA MANIPULATION
    ## ensure something is drawn if data are supplied from the workspace
    observe({
      if(!is.null(x)){
        lapply(
          seq_len(8),
          function(a, data){
            add_reference_ui(
              entry_number = a,
              ui_selector = "placeholder"
            )
          },
          data = data$raw
        )
      }
    })

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
      if(!any(colnames(import_result) == "selected")){
        import_result$selected <- NA
      }
      if(!any(colnames(import_result) == "notes")){
        import_result$notes <- NA
      }
      if(!any(colnames(import_result) == "color")){
        import_result$color <- "#000000"
      }

      # save progress
      data$raw <- import_result
      data$n_current <- min(
        c(
          input$n_citations,
          length(which(is.na(import_result$selected)))
        )
      )
      data$current <- seq_len(data$n_current)
      data$raw$screening_page <- calc_pages(
        n = nrow(data$raw),
        each = input$n_citations
      )
      data$raw$order <- seq_len(nrow(data$raw))
      click_values$page <- 1
      selector$yes <- rep(0, data$n_current)
      selector$no <- rep(0, data$n_current)
      selector$maybe <- rep(0, data$n_current)
    })


    # PLACEHOLDERS
    # determine whether to add or subtract placeholders
    observeEvent({
      data$current
      data$raw
    }, {
      if(!is.null(data$raw)){
        if(is.null(data$n_previous)){
          update$add_logical <- TRUE
          update$add_values <- seq_len(data$n_current)
          update$remove_logical <- FALSE
          update$remove_values <- NULL
        }else{
          if(data$n_previous > data$n_current){
            update$add_logical <- FALSE
            update$add_values <- NULL
            update$remove_logical <- TRUE
            update$remove_values <- seq_len(data$n_previous)[-seq_len(data$n_current)]
          }
          if(data$n_previous == data$n_current){
            update$add_logical <- FALSE
            update$add_values <- NULL
            update$remove_logical <- FALSE
            update$remove_values <- NULL
          }
          if(data$n_previous < data$n_current){
            update$add_logical <- TRUE
            update$add_values <- seq_len(data$n_current)[-seq_len(data$n_previous)]
            update$remove_logical <- FALSE
            update$remove_values <- NULL
          }
        }
      }
    })

    observeEvent(update$add_values, {
      if(!is.null(data$raw) & update$add_logical){
        lapply(update$add_values, function(a){
          add_reference_ui(
            entry_number = a,
            ui_selector = "placeholder"
          )
        })
        update$add_logical <- FALSE
        data$n_previous <- data$n_current
        selector$yes <- rep(0, data$n_previous)
        selector$no <- rep(0, data$n_previous)
        selector$maybe <- rep(0, data$n_previous)
      }
    })

    observeEvent(update$remove_values, {
      if(!is.null(data$raw) & update$remove_logical){
        lapply(update$remove_values, function(a){
          removeUI(
            selector = paste0("#citation_", a)
          )
        })
        update$remove_logical <- FALSE
        data$n_previous <- data$n_current
      }
    })


    # PAGE NAVIGATION
    # render navigation buttons
    # output$navigation_buttons <- renderUI({
    #   if(!is.null(data$raw)){
    #     navigation_buttons()
    #   }
    # })

    # set page navigation functionality
    observeEvent(input$page_first, {
      if(input$page_first > 0){
        click_values$page <- 1
      }
    })
    observeEvent(input$page_back, {
      if(input$page_back > 0 & (click_values$page > 1)){
        click_values$page <- click_values$page - 1
      }
    })
    observeEvent(input$page_next, {
      if(input$page_next > 0 & (click_values$page < max(data$raw$screening_page))){
        click_values$page <- click_values$page + 1
      }
    })
    observeEvent(input$page_last, {
      if(input$page_last > 0){
        click_values$page <- max(data$raw$screening_page)
      }
    })

    # render 'select all' buttons
    output$select_all_buttons <- renderUI({
      if(!is.null(data$raw)){
        select_all_buttons()
      }
    })


    # TEXT ORDERING AND RENDERING
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


    # ensure decisions about selected columns are retained
    observeEvent(input$order_result, {
      click_values$column <- input$order_result
    })



    # allow user to select order
    output$column_selector <- renderUI({
      if(input$order == "order_selected"){
        available_colnames <- colnames(data$raw)
        available_colnames <- available_colnames[
          !available_colnames %in% c(
            "notes", "selected", "color", "order", "screening_page"
          )]
        selectInput(
          inputId = "order_result",
          label = "Select variable to order by:",
          choices = available_colnames,
          selected = click_values$column
        )
      }
    })

    observeEvent({
      input$n_citations
      # input$order
      input$order_result_go
      }, {
      if(!is.null(data$raw)){
        page_values <- calc_pages(
          n = nrow(data$raw),
          each = input$n_citations
        )
        data$raw$order <- switch(input$order,
          "order_initial" = {seq_len(nrow(data$raw))},
          "order_alphabetical" = {rank(
            data$raw$title,
            ties.method = "random"
          )},
          "order_random" = {order(rnorm(length(page_values)))},
          "order_selected" = {rank(
            data$raw[[input$order_result]],
            ties.method = "random"
          )}
        )
        data$raw$screening_page <- switch(input$order,
          "order_initial" = {page_values},
          "order_alphabetical" = {page_values[data$raw$order]},
          "order_random" = {page_values[order(rnorm(length(page_values)))]},
          "order_selected" = {page_values[data$raw$order]},
        )
        data$current <- which(data$raw$screening_page == click_values$page)
        data$n_current <- min(
          c(
            length(data$current),
            input$n_citations
          )
        )
      }
    })

    observe({
      if(!is.null(data$raw)){
        # id selected text
        selected_rows <- which(data$raw$screening_page == click_values$page)
        data$current <- selected_rows[order(data$raw$order[selected_rows])]
        data$n_current <- length(data$current)

        # render text for each reference
        lapply(seq_len(data$n_current), function(a, df){
          output[[paste0(
            "citation_",
            a,
            "_render"
          )]] <- renderPrint({
            cat(paste0(
              "<font color = ",
              df$color[a],
              ">",
              df$citation[a],
              "</font><br><br>"
            ))
          })
        },
        df = data$raw[data$current, ]
        )

      }
    })


    # SCREENING
    # track article selections
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

      if(!is.null(data$raw)){
        completeness_check(data$raw)
      }

    })

    # track 'select all' buttons
    observeEvent(input$all_yes, {
      data$raw$selected[data$current] <- "selected"
      data$raw$color[data$current] <- "#405d99"
      completeness_check(data$raw)
    })
    observeEvent(input$all_no, {
      data$raw$selected[data$current] <- "excluded"
      data$raw$color[data$current] <- "#993f3f"
      completeness_check(data$raw)
    })
    observeEvent(input$all_maybe, {
      data$raw$selected[data$current] <- "unknown"
      data$raw$color[data$current] <- "#6d6d6d"
      completeness_check(data$raw)
    })

    # add progress indicator
    output$progress_text <- renderUI({
      if(!is.null(data$raw)){
        n_progress <- length(which(is.na(data$raw$selected) == FALSE))
        n_total <- nrow(data$raw)
        div(
          list(
            div(
              style = "
                display: inline-block;
                vertical-align: top;
                text-align: right;
                width: 580px",
              renderText({
                HTML(
                  paste0(
                    n_progress,
                    " of ",
                    n_total,
                    " entries screened | Showing page ",
                    click_values$page,
                    " of ",
                    max(data$raw$screening_page)
                  )
                )
              })
            ),
            navigation_buttons()
          )
        )
      }
    })


    # SAVE OR CLEAR DATA
    observeEvent(input$save_data, {
      save_modal(
        x = data$raw,
        title = "Save As"
      )
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
      filename <- paste(filename, input$save_type, sep = ".")
      switch(input$save_type,
        "csv" = {write.csv(data$raw, file = filename, row.names = FALSE)},
        "rds" = {saveRDS(data$raw, file = filename)}
      )
      removeModal()
    })

    # add option to remove data
    observeEvent(input$clear_data, {
      clear_data_modal()
    })

    observeEvent(input$clear_data_confirmed, {
      lapply(seq_len(data$n_current), function(a){
        removeUI(
          selector = paste0("#citation_", a)
        )
      })
      # reset all default reactiveValues
      data$raw <- NULL
      data$current <- NULL
      data$n_current <- NULL
      data$n_previous <- NULL
      click_values$yes <- NULL
      click_values$no <- NULL
      click_values$maybe <- NULL
      selector$yes <- c(0)
      selector$no <- c(0)
      selector$maybe <- c(0)
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- FALSE
      update$remove_values <- NULL
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