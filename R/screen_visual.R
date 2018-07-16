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
  observe({
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
    data$raw <- x
  })

  # select a grouping variable
  output$response_selector <-renderUI({
    if(!is.null(data$raw)){
    	choices <- colnames(data$raw)
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
    if(!is.null(data$raw)){
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Variables",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = TRUE,
          shiny::checkboxGroupInput("variable_selector",
            "Select included variables:",
            choices = colnames(data$raw)
          )
        )
      )
    }
  })


  # TOPIC MODELS
  observeEvent(input$calc_model, {

    # if no variables are selected, do not run a topic model
    if(length(input$variable_selector)<1){
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
      data$dtm <- NULL
      data$model <- NULL
      plot_features$appearance <- NULL

      # create data.frame with only relevant information for topic modelling
      data$grouped  <- create_grouped_dataframe(
        data = data$raw,
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
        pointsize = 12,
        height = input$screen_size
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


  # CLICK DATA
  observe({
  	click_result <- plotly::event_data(
      "plotly_click",
      source = "main_plot"
    )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
  	click_data$main <- which(
  		data$plot_ready[[input$plot_type]][, 1] == plot_features$appearance[[input$plot_type]]$id[click_result]
  	)
  	click_result$topic <- c()
  })

  # test output
  output$example_text <- renderPrint({
    if(length(click_data$main)>0){
      cat(format_citation(
        data$plot_ready[[input$plot_type]][click_data$main, ],
        abstract = FALSE,
        details = (input$hide_names == FALSE)
      ))
    }
  })


} # end server

shiny::shinyApp(ui, server) # run

}
