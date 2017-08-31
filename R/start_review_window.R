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


# build user interface
# icon options: http://fontawesome.io/icons/
header<- dashboardHeader(title="revtools")
sidebar<-dashboardSidebar(
	sidebarMenu(
		id="tabs",

		# need an input option here 
		# shiny::fileInput
		# https://blog.rstudio.com/2017/08/15/shiny-1-0-4/?utm_content=buffer31546&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
		# https://github.com/rstudio/shiny/blob/master/NEWS.md

		menuItem("Plot", icon=icon("bar-chart-o"),	
			menuItem("Content",
				menuSubItem("Articles", tabName="articles", selected=TRUE),
				menuSubItem("Words", tabName="words")
			),
			menuItem("Level",
				menuSubItem("Observations", tabName="observations", selected=TRUE),
				menuSubItem("Topics", tabName="topics")
			)
		),
		menuItem("Display", icon=icon("paint-brush"),

			# add dynamic screen sizing

			menuItem("Dimensions", 
				menuSubItem("2D", tabName="2d"),
				menuSubItem("3D", tabName="3d", selected=TRUE)
			),
			menuItem("Color Scheme",
				menuSubItem("Viridis", tabName="viridis"),
				menuSubItem("Magma", tabName="magma", selected=TRUE),
				menuSubItem("Inferno", tabName="inferno"),
				menuSubItem("Plasma", tabName="plasma")
			),
			sliderInput("color_alpha", "Opacity", min=0.2, max=1, step=0.1, value= 0.9),
			sliderInput("color_hue", "Hue", min=0, max=1, step=0.05, value= c(0, 0.9)),
			sliderInput("point_size", "Point Size", min=2, max=20, step=2, value= 10)
		),

		menuItem("Topic Model", icon=icon("calculator"),
			menuItem("Type",
				menuSubItem("LDA", tabName="lda", selected=TRUE),
				menuSubItem("CTM", tabName="ctm")
			),
			sliderInput("interations", "Iterations", min=1000, max=20000, step=1000, value= 2000),
			uiOutput("topic_slider"),
			actionButton("go_LDA", strong("Recalculate"))
		),

		menuItem("Save", icon=icon("save"),
			textInput("saveas", "Save as:", "revtools_results.rds"),
			actionButton("save", "Save")
		)
	)
)

body<-dashboardBody(
	fluidRow(
		box(plotlyOutput("plot_main", height=800), width=8), 
		box(width=4, title="Selected Text", solidHeader=TRUE, status="primary",
			tableOutput("plot_click"), br(),
			tableOutput("select_text"), p("  "),
			uiOutput("select_yes"), p("  "), #br(),
			uiOutput("select_no")
		)
	)
)

ui<-dashboardPage(header, sidebar, body) #, skin="black")


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

# create a list that stores all information from sidebar input
sidebar_tracker<-reactiveValues(
	content="articles",	
	level="observations",
	dimensions="3d",
	color_scheme="magma",
	model_type="lda")

# update the above as needed
observeEvent(input$tabs, {
	if(any(c("articles", "words")==input$tabs)){sidebar_tracker$content<-input$tabs}
	if(any(c("observations", "topics")==input$tabs)){sidebar_tracker$level<-input$tabs}
	if(any(c("2d", "3d")==input$tabs)){sidebar_tracker$dimensions<-input$tabs}
	if(any(c("magma", "viridis", "inferno", "plasma")==input$tabs)){sidebar_tracker$color_scheme<-input$tabs}
	if(any(c("lda", "ctm")==input$tabs)){sidebar_tracker$model_type<-input$tabs}
})

# lookup for which data and functions to use in plotly
plot_lookup<-expand.grid(
	content=c("articles", "words"),
	level=c("observations", "topics"),
	dimensions=c("2d", "3d"),
	stringsAsFactors=FALSE)
plot_lookup<-plot_lookup[order(plot_lookup$content, plot_lookup$level), ]
plot_lookup$data<-paste(rep(c("x", "y"), each=4), rep(rep(c("", "_topic"), each=2), 2), sep="")
plot_lookup$fun<-paste("plot", rep(c(1, 1, 2, 2), 2), "_", rep(c("2D", "3D"), 4), sep="")

# set up slider to select the number of topics
output$topic_slider<-renderUI({
	sliderInput("n_topics", "number of topics", min=3, max=30, value= infostore$model@k)
	})

# if asked, re-run LDA
observeEvent(input$go_LDA, {
	rows<-which(infostore$x$display)
	cols<-which(infostore$y$display)
	infostore$model<-LDAfun(infostore$dtm[rows, cols], 
		topic.model=sidebar_tracker$model_type, 
		n.topics=input$n_topics,
		iter=input$iterations)
	infostore$palette<-do.call(sidebar_tracker$color_scheme, list(
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
observeEvent(sidebar_tracker$color_scheme, {
	infostore$palette<-do.call(sidebar_tracker$color_scheme, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})
observeEvent(input$color_alpha, {
	infostore$palette<-do.call(sidebar_tracker$color_scheme, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})
observeEvent(input$color_hue, {
	infostore$palette<-do.call(sidebar_tracker$color_scheme, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	})

# plot window
observe({ # note: this is intended to update plotly when infostore$x changes
	output$plot_main<-renderPlotly({
		row_tr<-which(
			plot_lookup$content==sidebar_tracker$content &
			plot_lookup$level==sidebar_tracker$level &
			plot_lookup$dimensions==sidebar_tracker$dimensions)
		do.call(plot_lookup$fun[row_tr], list(
			input_info= infostore[[plot_lookup$data[row_tr]]],
			palette=infostore$palette,
			pointsize=input$point_size)
		)
	})
})

# click behavior: set reactive values to observe when a point is clicked on the plot
click_vals<-reactiveValues(d=NULL)
observe({click_vals$d<-(event_data("plotly_click", source="main_plot")$pointNumber + 1)})
output$plot_click<-renderPrint({
	if(is.null(click_vals$d)){cat("")
	}else{
		row_tr<-which(
			plot_lookup$content==sidebar_tracker$content &
			plot_lookup$level==sidebar_tracker$level &
			plot_lookup$dimensions==sidebar_tracker$dimensions)
		data_tr<-infostore[[plot_lookup$data[row_tr]]]
		if(sidebar_tracker$level=="observations"){
			cat(data_tr$caption[which(data_tr$display)[click_vals$d]])
		}else{
			cat(data_tr$caption[click_vals$d])
		}
	}
})

# controls for selector keys
output$select_text<-renderPrint({
	if(is.null(click_vals$d)==FALSE){
		if(sidebar_tracker$level=="topics"){
			cat("<strong>Select Topic?</strong>")
		}else{ # i.e. observations
			if(sidebar_tracker$content=="articles"){
				cat("<strong>Select Article?</strong>")
			}else{
				cat("<strong>Select Word?</strong>")
			}
		}
	}
})
output$select_yes<-renderPrint({
	if(is.null(click_vals$d)==FALSE){
		actionButton("return_yes", "Select", style="color: #fff; background-color: #428bca;")}})
output$select_no<-renderPrint({
	if(is.null(click_vals$d)==FALSE){
		actionButton("return_no", "Exclude", style="color: #fff; background-color: #428bca;")}})

# link action buttons to select/deselect articles to article status
observeEvent(input$return_yes, {
	row_tr<-which(
		plot_lookup$content==sidebar_tracker$content &
		plot_lookup$level==sidebar_tracker$level &
		plot_lookup$dimensions==sidebar_tracker$dimensions)
	if(sidebar_tracker$level=="observations"){
		lookup_val<-plot_lookup$data[row_tr]
		data_tr<-infostore[[lookup_val]]
		row<-which(data_tr$display)[click_vals$d]
	}else{
		lookup_val<-substr(plot_lookup$data[row_tr], 1, 1)
		data_tr<-infostore[[lookup_val]]
		row<-which(data_tr$display & data_tr$topic==infostore[[plot_lookup$data[row_tr]]]$topic[click_vals$d])
	}
	data_tr$tested[row]<-TRUE
	data_tr$selected[row]<-TRUE
	data_tr$topic_counter[row]<-topic_counter
	data_tr$decision_time[row]<-as.character(Sys.time())
	data_tr$display<-(data_tr$tested & data_tr$selected==FALSE)==FALSE
	infostore[[lookup_val]]<-data_tr
	click_vals$d<-NULL
})

observeEvent(input$return_no, {
	row_tr<-which(
		plot_lookup$content==sidebar_tracker$content &
		plot_lookup$level==sidebar_tracker$level &
		plot_lookup$dimensions==sidebar_tracker$dimensions)
	if(sidebar_tracker$level=="observations"){
		lookup_val<-plot_lookup$data[row_tr]
		data_tr<-infostore[[lookup_val]]
		row<-which(data_tr$display)[click_vals$d]
	}else{
		lookup_val<-substr(plot_lookup$data[row_tr], 1, 1)
		data_tr<-infostore[[lookup_val]]
		row<-which(data_tr$display & data_tr$topic==infostore[[plot_lookup$data[row_tr]]]$topic[click_vals$d])
	}
	data_tr$tested[row]<-TRUE
	data_tr$selected[row]<-FALSE
	data_tr$topic_counter[row]<-topic_counter
	data_tr$decision_time[row]<-as.character(Sys.time())
	data_tr$display<-(data_tr$tested & data_tr$selected==FALSE)==FALSE
	infostore[[lookup_val]]<-data_tr
	# add section to recalculate topic data.frames
	if(sidebar_tracker$content=="articles"){
		infostore$x_topic<-build_topic_df(infostore$x, y_matrix)
	}else{
		infostore$y_topic<-build_topic_df(infostore$y, x_matrix, type="y", xdata=infostore$x)
	}
	click_vals$d<-NULL
})

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