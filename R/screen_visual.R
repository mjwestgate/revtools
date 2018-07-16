# function to launch a shiny app for topic model visualisation & article/word (de)selection
start_review_window <- function(x, remove_words){
  screen_visual(x, remove_words)
}

screen_visual<-function(x, remove_words){

  if(missing(x)){x <- NULL}
  if(!is.null(x)){ # x<-as.data.frame(x)}

    # throw a warning if a known file type isn't given
    if(any(c("bibliography", "review_info", "data.frame")==class(x))==FALSE){
      stop("only classes 'bibliography', 'review_info' or 'data.frame' accepted by start_review_window")}

    switch(class(x),
      "bibliography" = {x <- as.data.frame(x)},
      "data.frame" = {x <- x},
      "review_info" = {x <- x$info})
  }

  if(missing(remove_words)){
    remove_words <- tm::stopwords()
  }else{
    remove_words <- as.character(remove_words)
  }


# create ui
ui_data <- screen_visual_ui()
ui <- shinydashboard::dashboardPage(
	ui_data$header,
	ui_data$sidebar,
	ui_data$body,
	skin = "blue"
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
  click_data <- reactiveValues(
    main = c(),
    topic = c()
  )

  # DATA INPUT
  ## when specified, ensure input data is processed correctly
  observeEvent(input$data_in, {
  	source <- input$data_in
  	if(is.null(x)){
  	  if(is.null(source)){
  	  	x <- NULL
  	  }else{
  	  	x <- as.data.frame(read_bibliography(source$datapath))
  	  }
  	}else{
  	  if(is.null(source)){
  	  	x <- x
  	  }else{
  	  	x <- merge_columns(x, as.data.frame(read_bibliography(source$datapath)))
  	  }
  	}
    if(any(colnames(x) == "selected") == FALSE){
      x$selected <- NA
    }
    data$raw <- x
    data$columns <- colnames(x)[which(colnames(x) != "selected")]
  })

  # observe({
  #   data$columns <- colnames(data$raw)[which(colnames(data$raw) != "selected")]
  # })

  # select a grouping variable
  output$response_selector <-renderUI({
    if(!is.null(data$columns)){
    	choices <- data$columns
      if(any(choices == "label")){
        selected <- "label"
      }else{
        selected <- choices[1]
      }
      selectInput(
        "response_variable",
        label = "Show one point per:",
        choices = choices,
        selected = selected
      )
    }
  })

  # add a sidebar menu listing columns available in data$raw
  output$variable_menu <- shinydashboard::renderMenu({
    if(!is.null(data$columns)){
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Variables",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = TRUE,
          shiny::checkboxGroupInput("variable_selector",
            "Select included variables:",
            choices = data$columns
          )
        )
      )
    }
  })


  # TOPIC MODELS
  observeEvent(input$calc_model, {

    # if no variables are selected, do not run a topic model
    if(length(input$variable_selector) < 1){
      shiny::showModal(
    		shiny::modalDialog(
        HTML("Please select 1 or more variables to include in the topic model<br><br>
        <em>Click anywhere to exit</em>"),
    		title = "Error: insufficient data",
    		footer = NULL,
    		easyClose = TRUE
    		)
    	)

    }else{
    	shiny::showModal(
    		shiny::modalDialog(
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

      # create data.frame with only relevant information for topic modelling
      keep_rows <- sort(c(
        which(data$raw$selected),
        which(is.na(data$raw$selected))
      ))
      data$grouped  <- create_grouped_dataframe(
        data = data$raw[keep_rows, ],
        response_variable = input$response_variable,
        text_variables = input$variable_selector
      )
      if(any(colnames(data$grouped) == "text")){
        data$dtm <- make_DTM(data$grouped$text)
      }

      # calculate topic model
    	x_keep <- which(apply(data$dtm, 1, sum) > 0)
      data$model <- run_LDA(
        x = data$dtm[x_keep, ],
        topic_model = tolower(input$model_type),
        n_topics = input$n_topics,
        iterations = input$n_iterations
      )

      # create plottable information
      data$plot_ready <- build_plot_data(
        info = data$grouped[x_keep, ],
        dtm = data$dtm[x_keep, ],
        model = data$model,
        hide_names = input$hide_names
      )

      # create color palette
      plot_features$palette <- viridisLite::viridis(
        n = data$model@k,
        alpha = 0.9,
        begin = 0,
        end = 0.9,
        option = "D"
      )

      # add appearance info
      plot_features$appearance <- build_appearance(
        data$plot_ready,
        plot_features$palette
      )

      # exit modal
      shiny::removeModal()
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
    plot_features$palette <- viridisLite::viridis(
      n = k,
      alpha = input$color_alpha,
      begin = input$color_hue[1],
      end = input$color_hue[2],
      option = input$palette
    )
    plot_features$appearance <- update_appearance(
      plot_features$appearance,
      plot_features$palette
    )
  })

  # update caption if required
  observeEvent(input$hide_names, {
    if(!is.null(data$plot_ready)){
      data$plot_ready$x$caption <- apply(data$plot_ready$x, 1,
        function(a, hide){format_citation_dataframe(a, hide_details = hide)
        }, hide = input$hide_names
      )
    }
  })


  # PLOTS
  output$plot_main <- plotly::renderPlotly({
    validate(
      need(data$model, "Choose data & model parameters to continue")
    )
    do.call(
      paste0("plot_", input$plot_dims),
      list(
        input_info = data$plot_ready[[input$plot_type]],
        color = isolate(plot_features$appearance[[input$plot_type]]$color),
        pointsize = 12
        # height = input$screen_size
      )
    )
  })

  # update (not redraw) when colours change
  observe({
  	plotly::plotlyProxy("plot_main", session) %>%
  		plotly::plotlyProxyInvoke("restyle", list(
  			marker = list(
  				size = input$point_size,
  				color = plot_features$appearance[[input$plot_type]]$color[
            plot_features$appearance[[input$plot_type]]$display
          ]
  			)
  		))
  })

  # topic barplot
  # note: may require an observe({}) to ensure updates
  output$plot_topic <- plotly::renderPlotly({
    validate(
      need(data$model, "Choose model parameters")
    )
    plot_article_bar(
      x = data$plot_ready$topic,
      n = data$plot_ready$topic[[paste0("count_", input$plot_type)]],
      color = plot_features$appearance$topic$color
    )
  })


  # CAPTURE CLICK DATA
  observe({
  	click_main <- plotly::event_data(
      "plotly_click",
      source = "main_plot"
    )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
    current_data <- plot_features$appearance[[input$plot_type]]
    click_data$main <- which(data$plot_ready[[input$plot_type]][, 1] ==
      plot_features$appearance[[input$plot_type]]$id[click_main])
  	click_data$topic <- c()
  })

  observe({
  	click_topic <- plotly::event_data(
      "plotly_click",
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
        cat(format_citation(
          data$plot_ready[[input$plot_type]][click_data$main, ],
          abstract = FALSE,
          details = (input$hide_names == FALSE)
        ))
      }else{
        cat(paste(
          data$plot_ready[[input$plot_type]][[input$response_variable]][click_data$main],
          collapse = "<br>"
        ))
      }
    }else{
      if(length(click_data$topic) > 0){
        cat(paste0(
          "<b>Topic #", click_data$topic,
          "</b> | Key terms<br><em>Most likely:</em> ",
  				data$plot_ready$topic$terms_default[click_data$topic],
          "<br><em>Heighest weighted:</em> ",
  				data$plot_ready$topic$terms_weighted[click_data$topic]
  			))
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
  output$select_yes <- renderPrint({
  	if(length(click_data$main) > 0 & input$plot_type == "x"){
  		actionButton("return_yes", "Select", style="color: #fff; background-color: #428bca;")
  	}
  })

  output$select_no <- renderPrint({
  	if(length(click_data$main) > 0){
  		actionButton("return_no", "Exclude", style="color: #fff; background-color: #428bca;")
  	}
  })

  output$topic_yes <- renderPrint({
  	if(length(click_data$topic) > 0 & input$plot_type == "x"){
  		actionButton("topic_yes", "Select", style="color: #fff; background-color: #428bca;")
  	}
  })

  output$topic_no <- renderPrint({
  	if(length(click_data$topic) > 0){
  		actionButton("topic_no", "Exclude", style="color: #fff; background-color: #428bca;")
  	}
  })


  # ARTICLE/WORD/TOPIC SELECTION
  observeEvent(input$return_yes, {
    plot_features$appearance[[input$plot_type]]$color[click_data$main] <- "#000000"
    selected_response <- data$plot_ready[[input$plot_type]][click_data$main, 1]
    rows <- which(data$raw[, input$response_variable] == selected_response)
    data$raw$selected[rows] <- TRUE
  })

  observeEvent(input$return_no, {
    # colour points
    plot_features$appearance[[input$plot_type]]$color[click_data$main] <- "#CCCCCC"
    # map to data$raw
    selected_response <- data$plot_ready[[input$plot_type]][click_data$main, 1]
    rows <- which(data$raw[, input$response_variable] == selected_response)
    data$raw$selected[rows] <- FALSE
  })

  observeEvent(input$topic_yes, {
    # color topic plot
    topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
    plot_features$appearance$topic$color[click_data$topic] <- "#000000"
    # color main plot
    rows <- which(data$plot_ready[[input$plot_type]]$topic == topic_selected)
    plot_features$appearance[[input$plot_type]]$color[rows] <- "#000000"
    # map to data$raw
    selected_responses <- data$plot_ready[[input$plot_type]][rows, 1]
    rows_raw <- which(data$raw[, input$response_variable] %in% selected_responses)
    data$raw$selected[rows_raw] <- TRUE
  })

  observeEvent(input$topic_no, {
    # color topic plot
    topic_selected <- plot_features$appearance$topic$topic[click_data$topic]
    plot_features$appearance$topic$color[click_data$topic] <- "#CCCCCC"
    # color main plot
    rows <- which(data$plot_ready[[input$plot_type]]$topic == topic_selected)
    plot_features$appearance[[input$plot_type]]$color[rows] <- "#CCCCCC"
    # map to data$raw
    selected_responses <- data$plot_ready[[input$plot_type]][rows, 1]
    rows_raw <- which(data$raw[, input$response_variable] %in% selected_responses)
    data$raw$selected[rows_raw] <- FALSE
  })


} # end server

shiny::shinyApp(ui, server) # run

}
