# user call to run a shiny interface to bibliographic data
start_review_window<-function(info){ # input can be an object of class("bibliography" | "review_info")

# throw a warning if a known file type isn't given
# Note that this precludes the user from giving a file name - possibly a usfeul feature
if(any(c("bibliography", "review_info", "data.frame")==class(info))==FALSE){
	stop("only classes 'bibliography', 'review_info' or 'data.frame' accepted by start_review")}
if(class(info)=="bibliography"){info <-make_reference_dataframe(info)}
	
if(class(info)!="review_info"){
	cat("building Document Term Matrix\n")
	dtm<-make_DTM(info)
	rownames(dtm)<-info$label
	dtm<-dtm[order(rownames(dtm)), order(colnames(dtm))]
	x_keep<-apply(dtm , 1, sum)>0
	default_topics<-round(length(which(x_keep))*0.05, 0)
		if(default_topics > 30){default_topics <-30}
		if(default_topics < 3){default_topics <-5}
	cat("running Topic Model\n")
	model<-run_LDA(dtm[x_keep, ], n.topics=default_topics)
	palette<-viridis(n=model@k, alpha=0.9, begin=0, end=0.9)

	# create a row (article) lookup
	x_matrix<-posterior(model)$topics # article x topic
	x<-data.frame(
		label= info$label[which(x_keep)],
		dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(x_matrix, 1, which.max),
		weight= apply(x_matrix, 1, max),
		stringsAsFactors=FALSE)
	citation_data<-data.frame(
		label= info$label,
		caption=apply(info, 1, format_citation_dataframe),
		tested=FALSE,
		selected=FALSE,
		display=x_keep,
		topic_counter=0,
		decision_time="",
		stringsAsFactors=FALSE)
	x<-merge(x, citation_data, by="label", all=TRUE)

	# create a column (word) lookup
	y_matrix<-t(posterior(model)$terms)
	y<-data.frame(
		caption=rownames(y_matrix),
		dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(y_matrix, 1, which.max),
		weight= apply(y_matrix, 1, max),
		stringsAsFactors=FALSE)	
	word_frequency<-data.frame(
		caption=colnames(dtm),
		frequency=apply(dtm, 2, sum),
		tested=FALSE,
		selected=FALSE,
		display=TRUE,
		topic_counter=0,
		decision_time="",
		stringsAsFactors=FALSE)
	y<-merge(y, word_frequency, by="caption", all=TRUE)

	# build simplified ordinations to allow selecting articles/words by topic
	x_topic<-build_topic_df(x, y_matrix)
	y_topic<-build_topic_df(y, x_matrix, type="y", xdata=x)

}

# create topic ordinations (articles and words) by averaging over topics
# article_topics # summarized version of article_ordination
# word_topics	# summarized version of word_ordination


# build user interface
ui<-fluidPage(

	# build sidebar to contain useful information
	column(width=2,
		h4("Controls"),
		selectInput("plot_type", "Plot Type", choice=c("articles", "words"), selected="articles"),
		checkboxInput("show_display", strong("Display options"), value=FALSE),
		conditionalPanel(condition="input.show_display==true",
			# dimension options
			selectInput("plot1_dim", "main plot", choice=c("2D", "3D"), selected="3D"),
			selectInput("plot2_dim", "topic plot", choice=c("2D", "3D"), selected="2D"),
			# color options
			checkboxInput("show_colors", strong("colors"), value=FALSE),
			conditionalPanel(condition="input.show_colors==true",
				selectInput("color", "color scheme", choices=c("viridis", "magma", "inferno", "plasma")),
				sliderInput("color_alpha", "opacity", min=0.2, max=1, step=0.1, value= 0.9)	,
				sliderInput("color_hue", "hue range", min=0, max=1, step=0.05, value= c(0, 0.9))	
				),
			# point size options
			checkboxInput("show_points", strong("point size"), value=FALSE),
			conditionalPanel(condition="input.show_points==true",	
				sliderInput("plot1_pointsize", "main plot", min=2, max=20, step=2, value= 10),
				sliderInput("plot2_pointsize", "topic plot", min=2, max=20, step=2, value= 10)
				)
			), # end conditionalPanel
		# LDA options
		checkboxInput("show_LDA", strong("Topic Model options"), value=FALSE),
		conditionalPanel(condition="input.show_LDA==true",
			selectInput("modeltype", "model type", choices=c("LDA", "CTM")),
			sliderInput("niter", "number of iterations", 
				min=1000, max=20000, step=1000, value= 2000),
			uiOutput("topic_slider"),
			actionButton("go_LDA", strong("recalculate model"), width="100%", 
				style="color: #fff; background-color: #428bca;")
			), # end conditionalPanel
		# Save options
		checkboxInput("show_save", strong("Save options"), value=FALSE),
		conditionalPanel(condition="input.show_save==true",
			textInput("saveas", "save as:", "revtools_results.rds"),
			actionButton("save", "save")
			)
		),

	# main window
	column(width = 7, 
		plotlyOutput("plot_x1", height = 500),  # show ordination in main window
		tableOutput("plot_x1_click"), # text from plot 1
		tableOutput("plot_x2_click") # text from plot 2
		), # end main panel
	
	#  right-hand column, to select show topics and allow selection
	column(width=3,
		# add barchart here
		plotlyOutput("plot_x2"),
		# then text to select relevant articles
		tableOutput("select_text_x1"), br(), # early
		uiOutput("select_yes_x1"), 
		uiOutput("select_no_x1"), br(),
		tableOutput("select_text_x2"), br(), # early
		uiOutput("select_yes_x2"), 
		uiOutput("select_no_x2"), br()
		)

) # end ui builder


# code for what to draw
server <- function(input, output, session) {

options(warn=-1) # hide incompatibility between shiny and plotly
# https://github.com/hrbrmstr/metricsgraphics/issues/49

# create reactiveValues object to store all data during shiny operations
if(class(info)=="review_info"){
	infostore<-reactiveValues(
		x= info$x,
		x_topic= info$x_topic,
		y= info$y,
		y_topic= info$y_topic,
		dtm= info$dtm,
		model= info$model,
		palette= info$palette)
	topic_counter<-max(info$x$topic_counter)+1
}else{
	infostore<-reactiveValues(
		x= x, # row info
		x_topic = x_topic,
		y=y, # col info
		y_topic = y_topic,
		dtm=dtm,
		model=model,
		palette= palette)
	topic_counter<-1
	}

# set LDA options
# set up slider to select the number of topics
output$topic_slider<-renderUI({
	sliderInput("n.topics", "number of topics", min=3, max=30, value= infostore$model@k)
	})

# if asked, re-run LDA
observeEvent(input$go_LDA, {
	rows<-which(infostore$x$display)
	cols<-which(infostore$y$display)
	infostore$model<-LDAfun(infostore$dtm[rows, cols], 
		topic.model=input$modeltype, 
		n.topics=input$n.topics,
		iter=input$niter)
	infostore$palette<-do.call(input$color, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	
	# update x (article) lookup
	x_matrix<-posterior(infostore$model)$topics
	x_new<-data.frame(
		label= infostore$x$label[which(infostore$x$display)],
		dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(x_matrix, 1, which.max),
		weight= apply(x_matrix, 1, max),
		stringsAsFactors=FALSE)
	x_new<-merge(infostore$x[, c(1, 7:ncol(infostore$x))], x_new, by="label", all=TRUE)
	x_new$color[which(x_new$display)]<-palette[x_new$topic]
	infostore$x<-x_new

	# update y (word) lookup
	y_matrix<-t(posterior(infostore$model)$terms)
	y_new<-data.frame(
		label= infostore$y$label[which(infostore$y$display)],
		dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(y_matrix, 1, which.max),
		weight= apply(y_matrix, 1, max),
		stringsAsFactors=FALSE)
	y_new<-merge(infostore$y[, c(1, 7:ncol(infostore$x))], y_new, by="caption", all=TRUE)
	y_new$color[which(y_new$display)]<-palette[y_new$topic]
	infostore$y<-y_new

	# topic ordinations
	infostore$x_topic<-build_topic_df(infostore$x, y_matrix)
	infostore$y_topic<-build_topic_df(infostore$y, x_matrix, type="y", xdata=infostore$x)

	topic_counter<-topic_counter+1
	})


# set colors
observeEvent(input$color, {
	infostore$palette<-do.call(input$color, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})
observeEvent(input$color_alpha, {
	infostore$palette<-do.call(input$color, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})
observeEvent(input$color_hue, {
	infostore$palette<-do.call(input$color, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})



# plot 1: article ordination (x)
observe({ # note: this is intended to update plotly when infostore$x changes
	output$plot_x1<-renderPlotly({
		if(input$plot_type=="articles"){
			if(input$plot1_dim=="3D"){plot1_3D(infostore$x, infostore$palette, input$plot1_pointsize)
			}else{plot1_2D(infostore$x, infostore$palette, input$plot1_pointsize)}
		}else{
			if(input$plot1_dim=="3D"){plot1_3D(infostore$y, infostore$palette, input$plot1_pointsize)
			}else{plot1_2D(infostore$y, infostore$palette, input$plot1_pointsize)}
		}
	})})

# set reactive values to observe when a point is clicked on plot x1
click_x1<-reactiveValues(d=NULL)
observe({click_x1$d<-(event_data("plotly_click", source="x1")$pointNumber + 1)})
output$plot_x1_click<-renderPrint({
	if(is.null(click_x1$d)){cat("")
	}else{
		click_x2$d<-NULL
		if(input$plot_type=="articles"){
			cat(infostore$x$topic[which(infostore$x$display)[click_x1$d]])
		}else{
			cat(infostore$y$topic[which(infostore$y$display)[click_x1$d]])
		}
		}
	})
output$select_text_x1<-renderPrint({
	if(is.null(click_x1$d)==FALSE){cat("<strong>Select Article?</strong>")}})
output$select_yes_x1<-renderPrint({
	if(is.null(click_x1$d)==FALSE){
		actionButton("return_yes_x1", "Yes", style="color: #fff; background-color: #428bca;")}})
output$select_no_x1<-renderPrint({
	if(is.null(click_x1$d)==FALSE){
		actionButton("return_no_x1", "No", style="color: #fff; background-color: #428bca;")}})

# link action buttons to select/deselect articles to article status
observeEvent(input$return_yes_x1, {
	if(input$plot_type=="articles"){
		row<-which(infostore$x$display)[click_x1$d]
		infostore$x$tested[row]<-TRUE
		infostore$x$selected[row]<-TRUE
		infostore$x$topic_counter[row]<-topic_counter
		infostore$x$decision_time[row]<-as.character(Sys.time())
		infostore$x$display<-(infostore$x$tested & infostore$x$selected==FALSE)==FALSE
	}else{
		row<-which(infostore$y$display)[click_x1$d]
		infostore$y$tested[row]<-TRUE
		infostore$y$selected[row]<-TRUE
		infostore$y$topic_counter[row]<-topic_counter
		infostore$y$decision_time[row]<-as.character(Sys.time())
		infostore$y$display<-(infostore$y$tested & infostore$y$selected==FALSE)==FALSE
	}
	click_x1$d<-NULL
	click_x2$d<-NULL
	})
observeEvent(input$return_no_x1, {
	if(input$plot_type=="articles"){
		row<-which(infostore$x$display)[click_x1$d]
		infostore$x$tested[row]<-TRUE
		infostore$x$selected[row]<-FALSE
		infostore$x$topic_counter[row]<-topic_counter
		infostore$x$decision_time[row]<-as.character(Sys.time())
		infostore$x$display<-(infostore$x$tested & infostore$x$selected==FALSE)==FALSE
	}else{
		row<-which(infostore$y$display)[click_x1$d]
		infostore$y$tested[row]<-TRUE
		infostore$y$selected[row]<-FALSE
		infostore$y$topic_counter[row]<-topic_counter
		infostore$y$decision_time[row]<-as.character(Sys.time())
		infostore$y$display<-(infostore$y$tested & infostore$y$selected==FALSE)==FALSE
	}
	click_x1$d<-NULL
	click_x2$d<-NULL
	# if(length(unique(infostore[[entry1]]$topic))<max(infostore[[entry1]]$topic)){
		# infostore$x_topic<-build_topic_df(
			# input_info=infostore$x[which(infostore$x$display), ], 
			# comparison_matrix=t(posterior(infostore$model)$terms))
		# # note: infostore$y may retain this topic, because we don't delete words by topic on this plot
		# } 
	})



# plot 2: topic ordination
observe({ # note: this is intended to update plotly when infostore$x changes
	output$plot_x2<-renderPlotly({
		if(input$plot_type=="articles"){
			if(input$plot2_dim=="3D"){plot2_3D(infostore$x_topic, infostore$palette, input$plot2_pointsize)
			}else{plot2_2D(infostore$x_topic, infostore$palette, input$plot2_pointsize)}
		}else{
			if(input$plot2_dim=="3D"){plot2_3D(infostore$y_topic, infostore$palette, input$plot2_pointsize)
			}else{plot2_2D(infostore$y_topic, infostore$palette, input$plot2_pointsize)}
		}
	})})

# set reactive values to observe when a point is clicked on plot x2
click_x2<-reactiveValues(d=NULL)
observe({click_x2$d<-(event_data("plotly_click", source="x2")$pointNumber + 1)})
output$plot_x2_click<-renderPrint({
	if(is.null(click_x2$d)){cat("")
	}else{
		click_x1$d<-NULL
		if(input$plot_type=="articles"){
			cat(infostore$x_topic$caption[click_x2$d])
		}else{
			cat(infostore$y_topic$caption[click_x2$d])
		}}
	})
output$select_text_x2<-renderPrint({
	if(is.null(click_x2$d)==FALSE){cat("<strong>Select Topic?</strong>")}})
output$select_yes_x2<-renderPrint({
	if(is.null(click_x2$d)==FALSE){
		actionButton("return_yes_x2", "Yes", style="color: #fff; background-color: #428bca;")}})
output$select_no_x2<-renderPrint({
	if(is.null(click_x2$d)==FALSE){
		actionButton("return_no_x2", "No", style="color: #fff; background-color: #428bca;")}})

# link action buttons to select/deselect articles to article status
observeEvent(input$return_yes_x2, {
	if(input$plot_type=="articles"){
		row<-which(infostore$x$display & infostore$x$topic==infostore$x_topic$topic[click_x2$d])
		infostore$x$tested[row]<-TRUE
		infostore$x$selected[row]<-TRUE
		infostore$x$topic_counter[row]<-topic_counter
		infostore$x$decision_time[row]<-as.character(Sys.time())
		infostore$x$display<-(infostore$x$tested & infostore$x$selected==FALSE)==FALSE
	}else{
		row<-which(infostore$y$display & infostore$y$topic==infostore$y_topic$topic[click_x2$d])
		infostore$y$tested[row]<-TRUE
		infostore$y$selected[row]<-TRUE
		infostore$y$topic_counter[row]<-topic_counter
		infostore$y$decision_time[row]<-as.character(Sys.time())
		infostore$y$display<-(infostore$y$tested & infostore$y$selected==FALSE)==FALSE
	}
	click_x1$d<-NULL
	click_x2$d<-NULL
	})
observeEvent(input$return_no_x2, {
	if(input$plot_type=="articles"){
		row<-which(infostore$x$display & infostore$x$topic==infostore$x_topic$topic[click_x2$d])
		infostore$x$tested[row]<-TRUE
		infostore$x$selected[row]<-FALSE
		infostore$x$topic_counter[row]<-topic_counter
		infostore$x$decision_time[row]<-as.character(Sys.time())
		infostore$x$display<-(infostore$x$tested & infostore$x$selected==FALSE)==FALSE
	}else{
		row<-which(infostore$y$display & infostore$y$topic==infostore$y_topic$topic[click_x2$d])
		infostore$y$tested[row]<-TRUE
		infostore$y$selected[row]<-FALSE
		infostore$y$topic_counter[row]<-topic_counter
		infostore$y$decision_time[row]<-as.character(Sys.time())
		infostore$y$display<-(infostore$y$tested & infostore$y$selected==FALSE)==FALSE
	}
	click_x1$d<-NULL
	click_x2$d<-NULL
	})
	# infostore$x_topic<-build_topic_df(
		# input_info=infostore$x[which(infostore$x$display), ], 
		# comparison_matrix=t(posterior(infostore$model)$terms))
	# })


# export all data when requested by the 'save' button
observeEvent(input$save, {
	output<-list(
		x= infostore$x,
		x_topic= infostore$x_topic, 
		y= infostore$y, 
		y_topic= infostore$y_topic,
		dtm=infostore$dtm,
		model=infostore$model,
		palette =infostore$palette
		)
	class(output)<-"review_info"
	saveRDS(output, input$saveas)
	})

} # end server

shinyApp(ui, server) # run
} # end function


# start_review_window(test_df)