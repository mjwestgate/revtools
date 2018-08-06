# function to launch a shiny app for topic model visualisation & article/word (de)selection
start_review_window <- function(x, remove_words){
  screen_topics(x, remove_words)
}

screen_topics <- function(x, remove_words){

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

  if(missing(remove_words)){
    remove_words <- stopwords()
  }else{
    remove_words <- as.character(remove_words)
  }


# create ui
ui_data <- screen_topics_ui()
ui <- shinydashboard::dashboardPage(
  title = "revtools | screen_topics",
	ui_data$header,
	ui_data$sidebar,
	ui_data$body,
	skin = "black"
)

# start server
server<-function(input, output, session){

  options(warn = -1) # hide incompatibility between shiny and plotly
  # https://github.com/hrbrmstr/metricsgraphics/issues/49

  # establish reactiveValues objects
  data <- reactiveValues(
    raw = NULL,
    columns = NULL,
    grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )
  plot_features <- reactiveValues(
    palette = NULL,
    appearance = NULL
  )
  # plots <- reactiveValues(
  #   scatter = NULL,
  #   bar = NULL
  # )
  click_data <- reactiveValues(
    main = c(),
    topic = c()
  )

  # CREATE HEADER IMAGE
  output$header <- renderPlot({
    revtools_logo(text = "screen_topics")
  })

  # DATA INPUT
  ## when specified, ensure input data is processed correctly
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
    if(any(colnames(import_result) == "selected") == FALSE){
      import_result$selected <- NA
    }
    if(any(colnames(import_result) == "display") == FALSE){
      import_result$display <- TRUE
    }
    if(any(colnames(import_result) == "topic") == FALSE){
      import_result$topic <- NA
    }
    if(any(colnames(import_result) == "notes") == FALSE){
      import_result$notes <- NA
    }
    data$raw <- import_result
    data$columns <- colnames(import_result)[
      which(
        (colnames(import_result) %in%
        c("selected", "topic", "display", "notes")) == FALSE
      )
    ]
  })

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
        HTML("  "),
        shiny::modalButton("Cancel"),
        title = "Clear all data",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })

  observeEvent(input$clear_data_confirmed, {
    data$raw <- NULL
    data$columns <- NULL
    data$grouped <- NULL
    data$dtm <- NULL
    data$model <- NULL
    data$plot_ready <- NULL
    removeModal()
  })

  # select a grouping variable
  output$response_selector <- renderUI({
    if(!is.null(data$columns)){
      if(any(data$columns == "label")){
        selected <- "label"
      }else{
        selected <- data$columns[1]
      }
      selectInput(
        "response_variable",
        label = "Show one point per:",
        choices = data$columns,
        selected = selected
      )
    }
  })

  # select text to be included in the DTM/topic model
  output$variable_selector <- renderUI({
    if(!is.null(data$columns)){
      checkboxGroupInput("variable_selector",
        "Select included variables:",
        choices = data$columns
      )
    }
  })


  # TOPIC MODELS
  observeEvent(input$calc_model, {

    # if no variables are selected, do not run a topic model
    if(length(input$variable_selector) < 1){
      showModal(
    		modalDialog(
          HTML("Please select 1 or more variables to include in the topic model<br><br>
          <em>Click anywhere to exit</em>"),
      		title = "Error: insufficient data",
      		footer = NULL,
      		easyClose = TRUE
    		)
    	)

    }else{
    	showModal(
    		modalDialog(
          HTML("Depending on the size of your dataset, this may take some time"),
      		title = "Calculating Topic Model",
      		footer = NULL,
      		easyClose = FALSE
    		)
    	)

      # wipe earlier models/data
      data$grouped <- NULL
      data$dtm <- NULL
      data$model <- NULL
      data$plot_ready <- NULL
      plot_features$appearance <- NULL

      # choose which rows to use for later calculation
      if(all(is.na(data$raw$selected)) == FALSE){
        data$raw$display[which(is.na(data$raw$selected) == FALSE)] <- FALSE
      }

      # create data.frame with only relevant information for topic modelling
      data$grouped  <- create_grouped_dataframe(
        data = data$raw[which(data$raw$display), ],
        response_variable = input$response_variable,
        text_variables = input$variable_selector
      )
      data$dtm <- make_DTM(data$grouped$text)

      # check for rows with no words; update to ensure all entries in 'data' match one another
      dtm_rowsums <- apply(data$dtm, 1, sum)
      if(any(dtm_rowsums == 0)){
        data$raw$display[which(data$raw$display)[which(dtm_rowsums == 0)]] <- FALSE
        keep_rows <- which(dtm_rowsums > 0)
        data$grouped$x <- data$grouped$x[keep_rows, ]
        data$dtm <- data$dtm[keep_rows, ]
      }

      # calculate topic model
      data$model <- run_LDA(
        dtm = data$dtm,
        topic_model = tolower(input$model_type),
        n_topics = input$n_topics,
        iterations = input$n_iterations
      )

      # create plottable information
      data$plot_ready <- build_plot_data(
        info = data$grouped,
        dtm = data$dtm,
        model = data$model,
        hide_names = input$hide_names
      )

      # create color palette
      plot_features$palette <- viridis(
        n = data$model@k,
        alpha = 0.9,
        begin = 0.1,
        end = 0.9,
        option = "A"
      )

      # add topic to data$raw,
      # noting that this data have been split in create_grouped_dataframe(),
      # which affects the order
      topic_dframe <- data.frame(
        variable = sort(unique(
          data$raw[which(data$raw$display), input$response_variable]
        )),
        topic = topicmodels::topics(data$model),
        stringsAsFactors = FALSE
      )
      result <- base::merge(
        x = data.frame(
          data$raw[, which(colnames(data$raw) != "topic")],
          order = seq_len(nrow(data$raw)),
          stringsAsFactors = FALSE
        ),
        y = topic_dframe,
        by.x = input$response_variable,
        by.y = "variable",
        all.x = TRUE,
        all.y = FALSE,
        sort = FALSE
      )
      data$raw <- result[, which(colnames(result) != "order")]
      # issue with this approach is that it causes columns to be reordered;
      # input$response variable is now first.
      # might be worth using lapply() or similar instead of merge()

      # add appearance info
      plot_features$appearance <- build_appearance(
        plot_data = data$plot_ready,
        palette = plot_features$palette
      )

      # exit modal
      removeModal()
    } # end if

  }) # end topic model calculation


  # update color palette when inputs change
  observeEvent({
    input$palette
    input$color_alpha
    input$color_hue
    input$point_size
    }, {
    if(is.null(data$model)){
      k <- 5
    }else{
      k <- data$model@k
    }
    plot_features$palette <- viridis(
      n = k,
      alpha = input$color_alpha,
      begin = input$color_hue[1],
      end = input$color_hue[2],
      option = input$palette
    )
    plot_features$appearance <- update_appearance(
      plot_data = plot_features$appearance,
      palette = plot_features$palette
    )
  })

  # update caption if required
  observeEvent(input$hide_names, {
    if(!is.null(data$plot_ready)){
      data$plot_ready$x$caption <- add_line_breaks(
        format_citation(
          data$plot_ready$x,
          details = (input$hide_names == FALSE)
        )
      )
    }
  })


  # PLOTS
  output$plot_main <- renderPlotly({
    validate(
      need(data$plot_ready, "Choose data & model parameters to continue")
    )
    do.call(
      paste0("plot_", input$plot_dims),
      list(
        input_info = data$plot_ready[[input$plot_type]],
        color = isolate(plot_features$appearance[[input$plot_type]]$color),
        pointsize = 12
      )
    )
  })

  # update (not redraw) when colours change
  observe({
  	plotlyProxy("plot_main", session) %>%
  		plotlyProxyInvoke("restyle", list(
  			marker = list(
  				size = input$point_size,
  				color = plot_features$appearance[[input$plot_type]]$color
  			)
  		))
  })

  # topic barplot
  output$plot_topic <- renderPlotly({
    validate(
      need(data$plot_ready, "")
    )
    plot_article_bar(
      x = data$plot_ready$topic,
      n = data$plot_ready$topic[[paste0("count_", input$plot_type)]],
      color = plot_features$appearance$topic$color
    )
  })


  # CAPTURE CLICK DATA
  observe({
  	click_main <- event_data(
      event = "plotly_click",
      source = "main_plot"
    )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
    current_data <- plot_features$appearance[[input$plot_type]]
    click_data$main <- which(data$plot_ready[[input$plot_type]][, 1] ==
      plot_features$appearance[[input$plot_type]]$id[click_main])
  	click_data$topic <- c()
  })

  observe({
  	click_topic <- event_data(
      event = "plotly_click",
      source = "topic_plot"
    )$pointNumber + 1
    click_data$topic <- which(data$plot_ready$topic$topic ==
      plot_features$appearance$topic$topic[click_topic])
  	click_data$main <- c()
  })

  # render 'selection' text
  output$selector_text <- renderPrint({
    if(length(click_data$main) > 0){
      if(any(c("label", "title") == input$response_variable)){
        cat(paste0(
          "<br><b>Entry:</b> ",
          format_citation(
            data$plot_ready[[input$plot_type]][click_data$main, ],
            abstract = FALSE,
            details = (input$hide_names == FALSE)
          ),
          "<br><br>"
        ))
      }else{
        cat(
          paste0(
            "<br><b>",
            gsub(
              "^[[:lower:]]",
              toupper(substr(input$response_variable, 1, 1)),
              input$response_variable
            ),
            ":</b> ",
            data$plot_ready[[input$plot_type]][[input$response_variable]][click_data$main],
            "<br><br>"
          )
        )
      }
    }else{
      if(length(click_data$topic) > 0){
        cat(
          paste0(
            "<br><b>Topic: ", click_data$topic,
            "</b><br><em>Most likely terms:</em> ",
    				data$plot_ready$topic$terms_default[click_data$topic],
            "<br><em>Heighest weighted terms:</em> ",
    				data$plot_ready$topic$terms_weighted[click_data$topic],
            "<br><br>"
          )
  			)
      }
    }
  })

  # render abstracts
  output$abstract_text <- renderPrint({
  	if(length(click_data$main) == 0){
  		cat("")
  	}else{
  	  if(any(colnames(data$plot_ready[[input$plot_type]]) == "abstract")){
        abstract_info <- paste0(
          "<br><b>Abstract:</b> ",
          data$plot_ready[[input$plot_type]]$abstract[click_data$main]
        )
  	    if(is.na(abstract_info)){
          cat("No abstract available")
        }else{
      	  cat(abstract_info)
  	  	}
  	  }else{
        cat("No abstracts available")
      }
  	}
  })

  # render selector buttons
  output$select_choice <- renderUI({
    if((length(click_data$main) > 0 | length(click_data$topic) > 0) & input$plot_type == "x"){
      radioButtons("select_point",
        label = "Selection:",
        choices = c("Select", "Exclude"),
        inline = TRUE
      )
    }
  })

  output$select_notes <- renderUI({
    if(length(click_data$main) > 0 | length(click_data$topic) > 0 ){
      if(length(click_data$main) > 0){
        selected_response <- data$plot_ready[[input$plot_type]][click_data$main, 1]
        row <- which(data$raw[which(data$raw$display), input$response_variable] == selected_response)
        start_text <- data$raw$notes[row]
      }else{
        topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
        rows <- which(data$raw$topic[which(data$raw$display)] == topic_selected)
        unique_text <- unique(data$raw$notes[rows])
        start_text <- unique_text[which(is.na(unique_text) == FALSE)]
        if(length(start_text) == 0){
          start_text <- ""
        }else{
          start_text <- paste(start_text, collapse = "; ")
        }
      }
      if(is.na(start_text)){
        initial_text <- ""
      }else{
        initial_text <- start_text
      }
      textAreaInput("select_notes",
        label = "Notes:",
        value = initial_text,
        resize = "vertical",
        width = "120%"
      )
    }
  })

  output$select_save <- renderUI({
    if(length(click_data$main) > 0 | length(click_data$topic) > 0){
      actionButton("select_saved",
        label = "Save Selection & Notes",
        width = "80%"
      )
    }
  })

  # when button is clicked, update plot and data as requested
  # Note: no note saving yet
  observeEvent(input$select_saved, {
    # set colors and answers
    if(input$select_point == "Select"){
      color_tr <- "#000000"
      result_tr <- TRUE
    }else{
      color_tr <- "#CCCCCC"
      result_tr <- FALSE
    }
    if(length(click_data$main) > 0){ # i.e. point selected on main plot
      plot_features$appearance[[input$plot_type]]$color[click_data$main] <- color_tr
      selected_response <- data$plot_ready[[input$plot_type]][click_data$main, 1]
      rows <- which(data$raw[which(data$raw$display), input$response_variable] == selected_response)
      data$raw$selected[rows] <- result_tr
      data$raw$notes[rows] <- input$select_notes
    }else{ # i.e. topic selected on barplot
      # color topic plot
      topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
      plot_features$appearance$topic$color[click_data$topic] <- color_tr
      # color main plot
      rows <- which(data$plot_ready[[input$plot_type]]$topic == topic_selected)
      plot_features$appearance[[input$plot_type]]$color[rows] <- color_tr
      # map to data$raw
      rows <- which(data$raw$topic[which(data$raw$display)] == topic_selected)
      data$raw$selected[rows] <- result_tr
      data$raw$notes[rows] <- input$select_notes
    }
  })

  # save data
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
      filename <- "revtools_data"
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

  # SAVE PLOTS WHEN REQUESTED
  # this fails at present because requires external dependencies.
  # this is also true of the recommended function 'plotly::orca'
  # observeEvent(input$save_bar, {
  #   if(is.null(plots$bar)){
  #     shiny::showModal(
  #       shiny::modalDialog(
  #         HTML(
  #           "Create a plot to begin<br><br>
  #           <em>Click anywhere to exit</em>"
  #         ),
  #         title = "Error: no plot to save",
  #         footer = NULL,
  #         easyClose = TRUE
  #       )
  #     )
  #   }else{
  #     shiny::showModal(
  #       shiny::modalDialog(
  #         shiny::textInput("save_bar_filename",
  #           label = "File Name"
  #         ),
  #         shiny::selectInput("save_bar_filetype",
  #           label = "File Type",
  #           choices = c("jpeg", "png", "pdf")
  #         ),
  #         shiny::actionButton("save_bar_execute", "Save"),
  #         shiny::modalButton("Cancel"),
  #         title = "Save Barplot As:",
  #         footer = NULL,
  #         easyClose = FALSE
  #       )
  #     )
  #   }
  # })
  #
  # observeEvent(input$save_bar_execute, {
  #   if(is.null(input$save_bar_filename)){
  #     file <- "revtools_barplot.jpeg"
  #   }else{
  #     file <- paste(
  #       input$save_bar_filename,
  #       input$save_bar_filetype,
  #       sep = "."
  #     )
  #   }
  #   plotly::export(
  #     p = plots$bar,
  #     file = file
  #   )
  # })


} # end server

shinyApp(ui, server) # run

}