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

    if(class(x) == "bibliography"){
      x <- as.data.frame(x)
    }

    x <- add_required_columns(data = x)
  }

  # add colnames
  if(is.null(x)){
    input_colnames <- NULL
  }else{
    input_colnames <- colnames(x)[
      which(
        (colnames(x) %in%
        c("selected", "topic", "display", "notes")
        ) == FALSE
      )
    ]
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
    raw = x,
    stopwords = remove_words,
    columns = input_colnames,
    grouped = NULL,
    dtm = NULL,
    model = NULL,
    plot_ready = NULL
  )
  plot_features <- reactiveValues(
    palette = NULL,
    appearance = NULL
  )
  click_data <- reactiveValues(
    main = c(),
    topic = c(),
    topic_2 = c(1),
    word = c()
  )
  words <- reactiveValues(
    stop = tm::stopwords(),
    current = NULL,
    rows = NULL,
    search_results = NULL,
    search_clicks = NULL,
    selected = NULL
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
    data$raw <- add_required_columns(
      data = import_result
    )
    data$columns <- colnames(data$raw)[
      which(
        (colnames(data$raw) %in%
        c("selected", "topic", "display", "notes")) == FALSE
      )
    ]
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
    data$columns <- NULL
    data$grouped <- NULL
    data$dtm <- NULL
    data$model <- NULL
    data$plot_ready <- NULL
    removeModal()
  })


  # data selection
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


  # calculate topic models
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
      data$dtm <- make_DTM(
        x = data$grouped$text,
        stop_words = data$stopwords
      )

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
          data = data$plot_ready$x,
          details = (input$hide_names == FALSE)
        )
      )
    }
  })


  # ENTRIES TAB
  # PLOTS
  output$plot_main <- renderPlotly({
    validate(
      need(data$plot_ready, "Choose data & model parameters to continue")
    )
    do.call(
      paste0("plot_", input$plot_dims),
      list(
        input_info = data$plot_ready$x,
        color = isolate(plot_features$appearance$x$color),
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
  				color = plot_features$appearance$x$color
  			)
  		))
  })

  # topic barplot (article tab)
  output$plot_topics <- renderPlotly({
    validate(
      need(data$plot_ready, "")
    )
    plot_topics(
      x = data$plot_ready$topic,
      n = data$plot_ready$topic$n,
      color = plot_features$appearance$topic$color,
      source = "topic_plot"
    )
  })

  # CLICK DATA
  observe({
    click_main <- event_data(
      event = "plotly_click",
      source = "main_plot"
    )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
    current_data <- plot_features$appearance$x
    click_data$main <- which(data$plot_ready$x[, 1] ==
      plot_features$appearance$x$id[click_main])
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

  # SHOW INFO ON CLICKED POINTS
  # show selected entry
  output$selector_text <- renderPrint({
    if(length(click_data$main) > 0){
      if(any(c("label", "title") == input$response_variable)){
        cat(paste0(
          "<br><b>Entry:</b> ",
          format_citation(
            data$plot_ready$x[click_data$main, ],
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
            data$plot_ready$x[[input$response_variable]][click_data$main],
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
  	  if(any(colnames(data$plot_ready$x) == "abstract")){
        abstract_info <- paste0(
          "<br><b>Abstract:</b> ",
          data$plot_ready$x$abstract[click_data$main]
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

  # SELECTION/DESELECTION
  # render selector buttons
  output$select_choice <- renderUI({
    if((length(click_data$main) > 0 | length(click_data$topic) > 0)){
      radioButtons("select_point",
        label = "Selection:",
        choices = c("Select", "Exclude"),
        inline = TRUE
      )
    }
  })

  # add notes
  output$select_notes <- renderUI({
    if(length(click_data$main) > 0 | length(click_data$topic) > 0 ){
      if(length(click_data$main) > 0){
        selected_response <- data$plot_ready$x[click_data$main, 1]
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

  # save selection choices & notes
  output$select_save <- renderUI({
    if(length(click_data$main) > 0 | length(click_data$topic) > 0){
      actionButton("select_saved",
        label = "Save Selection & Notes",
        width = "80%"
      )
    }
  })

  # UPDATE PLOT COLOURS
  # when button is clicked, update plot and data as requested
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
      plot_features$appearance$x$color[click_data$main] <- color_tr
      selected_response <- data$plot_ready$x[click_data$main, 1]
      rows <- which(data$raw[which(data$raw$display), input$response_variable] == selected_response)
      data$raw$selected[rows] <- result_tr
      data$raw$notes[rows] <- input$select_notes
    }else{ # i.e. topic selected on barplot
      # color topic plot
      topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
      plot_features$appearance$topic$color[click_data$topic] <- color_tr
      # color main plot
      rows <- which(data$plot_ready$x$topic == topic_selected)
      plot_features$appearance$x$color[rows] <- color_tr
      # map to data$raw
      rows <- which(data$raw$topic[which(data$raw$display)] == topic_selected)
      data$raw$selected[rows] <- result_tr
      data$raw$notes[rows] <- input$select_notes
    }
  })



    # WORD TAB

    # PLOTS
    # topic barplot (word tab)
    output$plot_topics_2 <- renderPlotly({
      validate(
        need(data$plot_ready, "")
      )
      plot_topics(
        x = data$plot_ready$topic,
        n = data$plot_ready$topic$n,
        color = plot_features$appearance$topic$color,
        source = "topic_plot_2"
      )
    })

    # word barplot
    output$plot_words <- renderPlotly({
      validate(
        need(data$plot_ready, "Choose data & model parameters to continue"),
        need(words$current, "Select a topic to continue")
      )
      plot_words(
        input_info = words$current,
        color = plot_features$appearance$y$color[words$rows]
      )

    })

  # get clicks from topic plot
  observe({
  	click_topic_2 <- event_data(
      event = "plotly_click",
      source = "topic_plot_2"
    )$pointNumber + 1
    click_data$topic_2 <- which(data$plot_ready$topic$topic ==
      plot_features$appearance$topic$topic[click_topic_2])
    click_data$word <- c() # should clear currently selected word, but doesn't
    if(length(click_topic_2) > 0){
      word_rows <- which(data$plot_ready$y$topic == click_data$topic_2)[1:30]
      word_data <- data$plot_ready$y[word_rows, ]
      word_data$term <- factor(
        x = rev(seq_len(30)),
        levels = seq_len(30),
        labels = rev(word_data$term)
      )
      words$current <- word_data
      words$rows <- word_rows
    }else{
      words$current <- NULL
      words$rows <- NULL
    }
  })

  # click data from word plot
  observe({
  	click_word <- event_data(
      event = "plotly_click",
      source = "word_plot"
    )$pointNumber + 1
    click_data$word <- which(
      (data$plot_ready$y$topic == click_data$topic_2) &
      (data$plot_ready$y$term == words$current$term[click_word])
    )
    if(length(click_data$word) > 0){
      words$selected <- data$plot_ready$y$term[click_data$word]
    }else{
      words$selected <- NULL
    }

  })

  # WORD SEARCH
  observeEvent(input$search_text, {
    removeUI(selector = "#search_buttons")
    if(nchar(input$search_text) < 1){
      words$search_results <- NULL
      words$search_clicks <- NULL
    }else{
      result <- grep(
        pattern = input$search_text,
        x = colnames(data$dtm)
      )
      if(length(result) > 0){
        result_rows <- result[seq_len(min(c(9, length(result))))]
        words$search_results <- colnames(data$dtm)[result_rows]
        words$search_clicks <- rep(0, length(result_rows))
        insertUI(
          selector = "#search_placeholder",
          ui = shiny::div(
            lapply(
              seq_len(length(words$search_results)),
              function(a, term){
                actionButton(
                  inputId = paste0("search_", a),
                  label = term[a]
                )
              },
              term = words$search_results
            ),
            id = "search_buttons"
          )
        )
      }else{
        words$search_results <- FALSE
        words$search_clicks <- NULL
      }
    }
  })

  # write some text that explains current search results
  output$search_results <- renderPrint({
    if(is.null(words$search_results)){
      cat("<em>Enter a word to begin searching</em>")
    }else{
      if(is.logical(words$search_results)){
        cat("<em>No results found</em>")
      }else{
        cat("Results:<br>")
      }
    }
  })

  # search for hits to search result buttons, and select any that are hit
  observe({
    if(!is.null(words$search_results)){
      if(!is.logical(words$search_results)){
        lookup_check <- grepl(
          "^search_[0-9]",
          names(input),
          perl = TRUE
        )
        lookup_names <- names(input)[which(lookup_check)]
        lookup_values <- unlist(lapply(
          lookup_names,
          function(a){input[[a]]}
        ))
        if(any(words$search_clicks != lookup_values)){
          words$selected <- words$search_results[
            which(words$search_clicks != lookup_values)
          ]
          words$search_clicks <- lookup_values
        }
      }
    }
  })

  # SELECTION/DESELECTION
  # give option to exclude selected word
  output$word_selector <- renderUI({
    if(!is.null(words$selected)){
      actionButton("remove_word",
        label = paste0(
          "Remove '",
          words$selected,
          "' from dataset"
        ),
        style = "width: 300px"
      )
    }
  })

  observeEvent(input$remove_word, {
    if(any(data$stopwords == words$selected) == FALSE){
      data$stopwords <- c(data$stopwords, words$selected)
    }
    if(any(words$current$term == words$selected)){
      row_color <- which(words$current$term == words$selected)
      plot_features$appearance$y$color[words$rows[row_color]] <- "#CCCCCC"
    }
    # Note: this doesn't grey out terms in the plots of other topics,
    # but will do for now
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


} # end server

shinyApp(ui, server) # run

}