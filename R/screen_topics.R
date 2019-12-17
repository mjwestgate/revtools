# function to launch a shiny app for topic model visualisation & article/word (de)selection
screen_topics <- function(
  x = NULL,
  remove_words = NULL,
  max_file_size
){

# set file size if requested, ensuring to reset on exit
if(!missing(max_file_size)){
  initial_file_size <- options("shiny.maxRequestSize")
  options(shiny.maxRequestSize = max_file_size * 1024^2)
  on.exit(options(initial_file_size))
}

data_in <- load_topic_data(
  data = x,
  stopwords = remove_words
)

# create ui
ui_data <- screen_topics_ui()
ui <- shinydashboard::dashboardPage(
  title = "revtools | screen_topics",
  header = ui_data$header,
  sidebar = ui_data$sidebar,
  body = ui_data$body,
  skin = "black"
)

# start server
server <- function(input, output, session){

  options(warn = -1) # hide incompatibility between shiny and plotly
  # https://github.com/hrbrmstr/metricsgraphics/issues/49

  # establish a reactiveValue object to store data
  data <- reactiveValues(
    raw = data_in$raw,
    stopwords = data_in$stopwords,
    columns = data_in$columns,
    grouped = data_in$grouped,
    dtm = data_in$dtm,
    model = data_in$model,
    plot_ready = data_in$plot_ready
  )

  # need to run some extra code here if class screen_topics_progress is used
  if(!is.null(data_in$model)){
    palette_initial <- viridis(
      n = data_in$model@k,
      alpha = 0.9,
      begin = 0.1,
      end = 0.9,
      option = "A"
    )
    appearance_initial <- build_appearance(
      plot_data = data_in$plot_ready,
      palette = palette_initial
    )
  }else{
    palette_initial <- NULL
    appearance_initial <- NULL
  }

  # add remaining reactiveValue objects
  plot_features <- reactiveValues(
    palette = palette_initial,
    appearance = appearance_initial,
    notes = FALSE,
    common_words = FALSE
  )
  click_data <- reactiveValues(
    main = c(),
    topic = c(),
    topic_2 = c(1),
    word = c()
  )
  words <- reactiveValues(
    current = NULL,
    rows = NULL,
    search_results = NULL,
    search_clicks = NULL,
    selected = NULL
  )

  # create header image
  output$header <- renderPlot({
    revtools_logo(text = "screen_topics")
  })

  # DATA INPUT
  ## when specified, ensure input data is processed correctly
  observeEvent(input$data_in, {
    data_loaded <- import_shiny_topic_data(
      source = input$data_in,
      current_data = data
    )
    data$raw <- data_loaded$raw
    data$columns <- data_loaded$columns
    data$grouped <- data_loaded$grouped
    data$dtm <- data_loaded$dtm
    data$model <- data_loaded$model
    data$plot_ready <- data_loaded$plot_ready

    # need to run some extra code here if class screen_topics_progress is used
    if(!is.null(data$model)){

      # create color palette
      plot_features$palette <- viridis(
        n = data$model@k,
        alpha = 0.9,
        begin = 0.1,
        end = 0.9,
        option = "A"
      )

      # add appearance info
      plot_features$appearance <- build_appearance(
        plot_data = data$plot_ready,
        palette = plot_features$palette
      )

    }
  })

  # show number of articles in current dataset
  output$progress_text <- renderText({
    if(!is.null(data$raw)){
      HTML(
        paste0(
          length(which(!is.na(data$raw$screened_topics))),
          " of ",
          nrow(data$raw),
          " entries screened"
        )
      )
    }
  })

  # duplicate of above for 'words' screen
  output$selector_n2 <- renderPrint({
    if(!is.null(data$raw)){
      cat(paste0(
        "Dataset containing ",
        nrow(data$raw),
        " entries | ",
        length(which(!is.na(data$raw$screened_topics))),
        " screened<br><br>"
      ))
    }
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
        response_column <- "label"
      }else{
        response_column <- data$columns[1]
      }
      selectInput(
        "response_variable",
        label = "Show one point per:",
        choices = data$columns,
        selected = response_column
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
      if(!all(is.na(data$raw$screened_topics))){
        data$raw$display[which(is.na(data$raw$screened_topics) == FALSE)] <- FALSE
      }

      # create data.frame with only relevant information for topic modelling
      data$grouped  <- create_grouped_dataframe(
        data = data$raw[which(data$raw$display), ],
        response_variable = input$response_variable,
        text_variables = input$variable_selector
      )

      data$dtm <- make_dtm(
        x = data$grouped$text,
        stop_words = data$stopwords,
        min_freq = input$min_freq * 0.01,
        max_freq = input$max_freq * 0.01,
        bigram_quantile = input$bigram_quantile * 0.01
      )

      if(input$response_variable != data$columns[1]){
        plot_features$common_words <- TRUE
      }else{
        plot_features$common_words <- FALSE
      }

      # restrict to only entries that are present in data$dtm
      if(data$dtm$nrow < nrow(data$grouped)){
        data$grouped <- data$grouped[as.numeric(data$dtm$dimnames$Docs), ]
      }

      # calculate topic model
      data$model <- run_topic_model(
        dtm = data$dtm,
        type = tolower(input$model_type),
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

      # add topic to data$raw
      data$grouped$topic <- topicmodels::topics(data$model)
      data$raw$topic <- unlist(lapply(
        data$raw[, input$response_variable],
        function(a, lookup){
          if(is.na(a)){
            NA
          }else{
            if(any(lookup[, 1] == a)){
              lookup$topic[which(lookup[, 1] == a)]
            }else{
              NA
            }
          }
        },
      lookup = data$grouped
      ))

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
      data$plot_ready$x$caption <- paste0(
        format_citation(
          data = data$plot_ready$x,
          details = !as.logical(input$hide_names),
          add_html = TRUE,
          line_breaks = TRUE
        ),
        "<br>[Topic #",
        data$plot_ready$x$topic,
        "]"
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

  # if a topic is selected, highlight points with that topic in the main plot
  observeEvent(click_data$topic, {
    if(length(click_data$topic) > 0){
      current_topic_colors <- rep("#d1d1d180", nrow(data$plot_ready$x))
      current_topic_colors[
        which(data$plot_ready$x$topic == click_data$topic)
        ] <- plot_features$palette[click_data$topic]
      current_topic_colors[plot_features$appearance$x$color == "#000000"] <- "#000000"
      current_topic_colors[plot_features$appearance$x$color == "#CCCCCC"] <- "#CCCCCC"
      plot_features$appearance$x$color <- current_topic_colors
    }else{
      plot_features$appearance <- update_appearance(
        plot_data = plot_features$appearance,
        palette = plot_features$palette
      )
    }
  })

  # SHOW INFO ON CLICKED POINTS
  # show selected entry
  output$selector_text <- renderPrint({
    if(length(click_data$main) > 0){ # i.e. display data for one entry
      citation_tr <- format_citation(
        data$plot_ready$x[click_data$main, ],
        abstract = FALSE,
        details = (input$hide_names == FALSE),
        add_html = TRUE
      )
      if(plot_features$common_words){
        display_text <- paste0(
          "<b>",
          citation_tr,
          "</b><br><em>Most common words:</em> ",
          data$plot_ready$x$common_words[click_data$main]
        )
      }else{
        display_text <- citation_tr
      }
      cat(paste0(
        "<br><font color =",
        plot_features$appearance$x$text_color[click_data$main],
        ">",
        display_text,
        "</font><br><br>"
      ))
    }else{ # display data for one topic
      if(length(click_data$topic) > 0){
        cat(
          paste0(
            "<br><font color =",
            plot_features$appearance$x$text_color[click_data$main],
            "><b>Topic: ",
            click_data$topic,
            "</b><br>",
    				data$plot_ready$topic$caption_full[click_data$topic],
            "</font><br><br>"
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
  # show selection options for selected point
  output$select_choice <- renderUI({
    if((length(click_data$main) > 0 | length(click_data$topic) > 0)){
      div(
        list(
          div(
            style = "
              display: inline-block;
              vertical-align: top;
              width: 80px",
            actionButton(
              inputId = "select_yes",
              label = "Select",
              style = "
                background-color: #7c93c1;
                color: #fff;
                width: 80px"
            )
          ),
          div(
            style = "
              display: inline-block;
              vertical-align: top;
              width: 80px",
            actionButton(
              inputId = "select_no",
              label = "Exclude",
              style = "
                background-color: #c17c7c;
                color: #fff;
                width: 80px"
            )
          ),
          div(
            style = "
              display: inline-block;
              vertical-align: top;
              width: 150px",
            actionButton(
              inputId = "notes_toggle",
              label = "Show/Hide Notes",
              style = "
                background-color: #adadad;
                color: #fff;
                width: 150px"
            )
          )
        )
      )
    }
  })

  # when toggle is triggered, invert display status of notes
  observeEvent(input$notes_toggle, {
    plot_features$notes <- !plot_features$notes
  })

  # when requested, show notes
  output$render_notes <- renderUI({
    if(
      plot_features$notes &
      (sum(c(click_data$main, click_data$topic)) > 0)
    ){
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
      # add content to screen
      div(
        list(
          br(),
          div(
            textAreaInput("notes_text",
              label = NULL,
              value = initial_text,
              resize = "vertical",
              width = "100%"
            )
          ),
          div(
            actionButton(
              inputId = "notes_save",
              label = "Save Notes",
              width = "80%"
            )
          )
        )
      )
    }
  })


  # SAVE STATUS OF EACH ARTICLE/TOPIC
  # when button is clicked, update plot and data as requested
  observeEvent(input$select_yes, {
    if(length(click_data$main) > 0){ # i.e. point selected on main plot
      plot_features$appearance$x$color[click_data$main] <- "#000000"
      plot_features$appearance$x$text_color[click_data$main] <- "#405d99" # NEW
      selected_response <- data$plot_ready$x[click_data$main, 1]
      display_rows <- which(data$raw$display)
      selected_rows <- display_rows[
        which(data$raw[display_rows, input$response_variable] == selected_response)
      ]
      data$raw$screened_topics[selected_rows] <- "selected"
    }else{ # i.e. topic selected on barplot
      # color topic plot
      topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
      plot_features$appearance$topic$color[click_data$topic] <- "#000000"
      plot_features$appearance$topic$text_color[click_data$main] <- "#405d99"
      # color main plot
      rows <- which(data$plot_ready$x$topic == topic_selected)
      plot_features$appearance$x$color[rows] <- "#000000"
      plot_features$appearance$x$text_color[rows] <- "#405d99"
      # map to data$raw
      display_rows <- which(data$raw$display)
      rows <- display_rows[which(data$raw$topic[display_rows] == topic_selected)]
      data$raw$screened_topics[rows] <- "selected"
    }
  })

  observeEvent(input$select_no, {
    if(length(click_data$main) > 0){ # i.e. point selected on main plot
      plot_features$appearance$x$color[click_data$main] <- "#CCCCCC"
      plot_features$appearance$x$text_color[click_data$main] <- "#993f3f"
      selected_response <- data$plot_ready$x[click_data$main, 1]
      display_rows <- which(data$raw$display)
      selected_rows <- display_rows[
        which(data$raw[display_rows, input$response_variable] == selected_response)
      ]
      data$raw$screened_topics[selected_rows] <- "excluded"
    }else{ # i.e. topic selected on barplot
      # color topic plot
      topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
      plot_features$appearance$topic$color[click_data$topic] <- "#CCCCCC"
      plot_features$appearance$topic$text_color[click_data$main] <- "#993f3f"
      # color main plot
      rows <- which(data$plot_ready$x$topic == topic_selected)
      plot_features$appearance$x$color[rows] <- "#CCCCCC"
      plot_features$appearance$x$text_color[rows] <- "#993f3f"
      # map to data$raw
      display_rows <- which(data$raw$display)
      rows <- display_rows[which(data$raw$topic[display_rows] == topic_selected)]
      data$raw$screened_topics[rows] <- "excluded"
    }
  })

  observeEvent(input$notes_save, {
    if(length(click_data$main) > 0){
      selected_response <- data$plot_ready$x[click_data$main, 1]
      display_rows <- which(data$raw$display)
      selected_rows <- display_rows[
        which(data$raw[display_rows, input$response_variable] == selected_response)
      ]
      data$raw$notes[selected_rows] <- input$notes_text
    }else{ # i.e. topic selected on barplot
      topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
      display_rows <- which(data$raw$display)
      rows <- display_rows[which(data$raw$topic[display_rows] == topic_selected)]
      data$raw$notes[rows] <- input$notes_text
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
    if(any(plot_features$appearance$y$id == words$selected)){
      row_color <- which(plot_features$appearance$y$id == words$selected)
      plot_features$appearance$y$color[row_color] <- "#CCCCCC"
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
          selectInput(
            inputId = "save_what",
            label = "Choose an item to save",
            choices = list(
              "Save Progress (.rds)" = "progress",
              "Save Choices (.csv)" = "choices"
            ),
            multiple = FALSE
          ),
          textInput(
            inputId = "save_filename",
            label = "File Name"
          ),
          actionButton(
            inputId = "save_data_execute",
            label = "Save"
          ),
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
    if(input$save_what == "progress"){
      file_extension <- "rds"
    }else{
      file_extension <- "csv"
    }
    filename <- paste(filename, file_extension, sep = ".")
    switch(input$save_what,
      "choices" = {
        write.csv(data$raw,
          file = filename,
          row.names = FALSE
        )
      },
      "progress" = {
        output <- list(
          raw = data$raw,
          stopwords = data$stopwords,
          columns = data$columns,
          grouped = data$grouped,
          dtm = data$dtm,
          model = data$model,
          plot_ready = data$plot_ready
        )
        class(output) <- "screen_topics_progress"
        saveRDS(
          output,
          file = filename
        )
      }
    )
    removeModal()
  })

  observeEvent(input$exit_app, {
    exit_modal()
  })

  observeEvent(input$exit_app_confirmed, {
    output <- list(
      raw = data$raw,
      stopwords = data$stopwords,
      columns = data$columns,
      grouped = data$grouped,
      dtm = data$dtm,
      model = data$model,
      plot_ready = data$plot_ready
    )
    class(output) <- "screen_topics_progress"
    stopApp(returnValue = invisible(output))
  })

} # end server

print(shinyApp(ui, server))

}