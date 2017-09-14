# user call to run a shiny interface to bibliographic data
start_review_window<-function(info){ # input can be an object of class("bibliography" | "review_info")

# throw a warning if a known file type isn't given
if(any(c("bibliography", "review_info", "data.frame")==class(info))==FALSE){
	stop("only classes 'bibliography', 'review_info' or 'data.frame' accepted by start_review")}
if(class(info)=="bibliography"){info <-make_reference_dataframe(info)}
	
if(class(info)!="review_info"){
	cat("building Document Term Matrix\n")
	dtm<-make_DTM(info)
	rownames(dtm)<-paste0("x", c(1:nrow(dtm)))
	dtm<-dtm[apply(dtm , 1, sum)>0, order(colnames(dtm))]
	x_keep<-apply(dtm , 1, sum)>0
	default_topics<-round(length(which(x_keep))*0.05, 0)
		if(default_topics > 30){default_topics <-30}
		if(default_topics < 3){default_topics <-5}
	cat("running Topic Model\n")
	model<-run_LDA(dtm, n.topics=default_topics)
	palette_initial <-magma(n=model@k, alpha=0.9, begin=0, end=0.9)

	# data to send to plotinfo
	x_matrix<-posterior(model)$topics # article x topic
	y_matrix<-t(posterior(model)$terms)
	plot_list<-list(
		x=data.frame(
			id=rownames(dtm),
			label= info$label[which(x_keep)],
			dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
			topic= apply(x_matrix, 1, which.max),
			weight= apply(x_matrix, 1, max),
			caption=apply(info, 1, format_citation_dataframe),
			stringsAsFactors=FALSE),
		y=data.frame(
			id=paste0("y", c(1:nrow(y_matrix))),
			label=rownames(y_matrix),
			dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
			topic= apply(y_matrix, 1, which.max),
			weight= apply(y_matrix, 1, max),
			caption=rownames(y_matrix),
			stringsAsFactors=FALSE)
		)
	if(any(colnames(info)=="abstract")){plot_list$x$abstract<-info$abstract[x_keep]}
	plot_list$x_topic<-build_topic_df(plot_list$x, y_matrix)
	plot_list$y_topic<-build_topic_df(plot_list$y, x_matrix, type="y", xdata= plot_list$x)

	# generate info to pass to infostore: it updates the display, but not the whole plot
	dynamic_list<-list(
		x=data.frame(
			id=rownames(dtm),
			label= plot_list$x$label,
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			color=palette_initial[plot_list$x$topic],
			stringsAsFactors=FALSE),
		y=data.frame(
			id=plot_list$y$id,
			label=colnames(dtm),
			frequency=apply(dtm, 2, sum),
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			color=palette_initial[plot_list$y$topic],
			stringsAsFactors=FALSE),
		x_topic=data.frame(
			id= plot_list$x_topic$id,
			topic= plot_list$x_topic$topic,
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			color=palette_initial[plot_list$x_topic$topic],
			stringsAsFactors=FALSE),
		y_topic=data.frame(
			id= plot_list$y_topic$id,
			topic= plot_list$y_topic$topic,
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			color=palette_initial[plot_list$y_topic$topic],
			stringsAsFactors=FALSE)
		)
}


# build user interface
# icon options: http://fontawesome.io/icons/
header<- dashboardHeader(title="revtools")
sidebar<-dashboardSidebar(
	sidebarMenu(
		id="tabs",

		menuItem("Plot", icon=icon("bar-chart-o"),	
			menuItem("Content",
				menuSubItem("Articles", tabName="articles", selected=TRUE),
				menuSubItem("Words", tabName="words")
			),
			menuItem("Level",
				menuSubItem("Observations", tabName="observations", selected=TRUE),
				menuSubItem("Topics", tabName="topics")
			),
			menuItem("Window",
				sliderInput("screen_size", "Height (px)", min=400, max=1400, step=100, value= 600)
			),
			menuItem("Dimensions", 
				menuSubItem("2D", tabName="2d", selected=TRUE),
				menuSubItem("3D", tabName="3d")
			)
		),
		menuItem("Colors", icon=icon("paint-brush"),

			menuItem("Palette",
				menuSubItem("Viridis", tabName="viridis"),
				menuSubItem("Magma", tabName="magma", selected=TRUE),
				menuSubItem("Inferno", tabName="inferno"),
				menuSubItem("Plasma", tabName="plasma")
			),

			menuItem("Options",			
				sliderInput("color_alpha", "Opacity", min=0.2, max=1, step=0.1, value= 0.9),
				sliderInput("color_hue", "Hue", min=0, max=1, step=0.05, value= c(0, 0.9)),
				sliderInput("point_size", "Point Size", min=0, max=20, step=2, value= 12)
			)
		),

		menuItem("Topic Model", icon=icon("calculator"),
			menuItem("Model Type",
				menuSubItem("LDA", tabName="lda", selected=TRUE),
				menuSubItem("CTM", tabName="ctm")
			),
			menuItem("Model Specs",
				sliderInput("iterations", "# Iterations", min=1000, max=20000, step=1000, value= 2000),
				sliderInput("n_topics", "# Topics", min=2, max=30, step=1, value=5)
				# uiOutput("topic_slider")
			),
			actionButton("go_LDA", strong("Recalculate"))
		),

		menuItem("Save", icon=icon("save"),
			selectInput("save_type", "File Type", 
				choices=c("All Data (.rds)", "Article Selections (.csv)")),
			textInput("saveas", "Save As:", "revtools_results.rds"),
			actionButton("save", "Save")
		)
	)
)

body<-dashboardBody(
	fluidRow(
		box(width=8, # title="Plot Window", solidHeader=TRUE, status="primary",
			withSpinner(plotlyOutput("plot_main"))
		), 
		box(width=4, title="Selected Text", solidHeader=TRUE, status="primary",
			tableOutput("plot_status"),
			tableOutput("plot_click"), br(),
			tableOutput("select_text"), p("  "),
			uiOutput("select_yes"), p("  "), # br(), 
			uiOutput("select_no"), br(),
			tableOutput("abstract_info")
		)
	)
)

ui<-dashboardPage(header, sidebar, body)


server <- function(input, output, session) {

options(warn=-1) # hide incompatibility between shiny and plotly
# https://github.com/hrbrmstr/metricsgraphics/issues/49

# create a list that stores all information from sidebar input
sidebar_tracker<-reactiveValues(
	content="articles",	
	level="observations",
	dimensions="2d",
	color_scheme="magma",
	model_type="lda")

# update the above as needed
observeEvent(input$tabs, {
	if(any(c("articles", "words")==input$tabs)){
		sidebar_tracker$content<-input$tabs}
	if(any(c("observations", "topics")==input$tabs)){
		sidebar_tracker$level<-input$tabs}
	if(any(c("2d", "3d")==input$tabs)){
		sidebar_tracker$dimensions<-input$tabs}
	if(any(c("magma", "viridis", "inferno", "plasma")==input$tabs)){
		sidebar_tracker$color_scheme<-input$tabs}
	if(any(c("lda", "ctm")==input$tabs)){
		sidebar_tracker$model_type<-input$tabs}
})

# create reactiveValues objects to store all data during shiny operations
if(class(info)=="review_info"){
	infostore<-reactiveValues(
		x = info$x,
		x_topic= info$x_topic,
		y = info$y,
		y_topic= info$y_topic,
		dtm= info$dtm,
		model= info$model
	)
	topic_counter<-max(info$x$topic_counter)+1
}else{
	infostore<-reactiveValues(
		x = dynamic_list$x,
		y = dynamic_list$y,
		x_topic = dynamic_list$x_topic,
		y_topic = dynamic_list$y_topic,
		dtm=dtm,
		model=model
	)
	topic_counter<-1
	}

plotinfo<-reactiveValues(
	x = plot_list$x,
	y = plot_list$y,
	x_topic = plot_list$x_topic,
	y_topic = plot_list$y_topic
)

rm(dynamic_list, plot_list, dtm, model)


## PLOT
# lookup for which data and functions to use in plotly
plot_lookup<-expand.grid(
	content=c("articles", "words"),
	level=c("observations", "topics"),
	dimensions=c("2d", "3d"),
	stringsAsFactors=FALSE)
plot_lookup<-plot_lookup[order(plot_lookup$content, plot_lookup$level), ]
plot_lookup$tag<-paste0(
	rep(c("x", "y"), each=4), 
	rep(rep(c("", "_topic"), each=2), 2)
	)
plot_lookup$fun<-paste("plot", rep(c(1, 1, 2, 2), 2), "_", rep(c("2D", "3D"), 4), sep="")

# keep track of which plot type and dataset to use
plot_data<-reactiveValues(dataset="x", fun="plot1_2D")
observeEvent({
	sidebar_tracker$content
	sidebar_tracker$level 
	sidebar_tracker$dimensions
	}, {
	row_tr<-which(
		plot_lookup$content==sidebar_tracker$content &
		plot_lookup$level==sidebar_tracker$level &
		plot_lookup$dimensions==sidebar_tracker$dimensions)
	plot_data$dataset<-plot_lookup$tag[row_tr]
	plot_data$fun<-plot_lookup$fun[row_tr]
})

# update colors as needed
observeEvent({
	sidebar_tracker$content
	sidebar_tracker$level
	sidebar_tracker$color_scheme
	input$color_alpha
	input$color_hue
	input$point_size
	}, {
	palette_tr<-do.call(sidebar_tracker$color_scheme, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	infostore$x$color[infostore$x$present]<-palette_tr[plotinfo$x$topic]
		infostore$x$color[which(infostore$x$selected)]<-"#000000"
		infostore$x$color[which(infostore$x$display==FALSE)]<-"#CCCCCC"
	infostore$x_topic$color[infostore$x_topic$present]<-palette_tr[plotinfo$x_topic$topic]
		infostore$x_topic$color[which(infostore$x_topic$selected)]<-"#000000"
		infostore$x_topic$color[which(infostore$x_topic$display==FALSE)]<-"#CCCCCC"
	infostore$y$color[infostore$y$present]<-palette_tr[plotinfo$y$topic]
		infostore$y$color[which(infostore$y$selected)]<-"#000000"
		infostore$y$color[which(infostore$y$display==FALSE)]<-"#CCCCCC"
	infostore$y_topic$color[infostore$y_topic$present]<-palette_tr[plotinfo$y_topic$topic]
		infostore$y_topic$color[which(infostore$y_topic$selected)]<-"#000000"
		infostore$y_topic$color[which(infostore$y_topic$display==FALSE)]<-"#CCCCCC"
})

# plot window
output$plot_main<-renderPlotly({
	do.call(plot_data$fun, list(
		input_info= plotinfo[[plot_data$dataset]],
		color=isolate(infostore[[plot_data$dataset]]$color[infostore[[plot_data$dataset]]$present]),
		pointsize=12,
		height=input$screen_size
		)
	)
})

# update (not redraw) when colors change
observe({
	plotlyProxy("plot_main", session) %>%
		plotlyProxyInvoke("restyle", list(
			marker=list(
				size = input$point_size, 
				color=infostore[[plot_data$dataset]]$color[infostore[[plot_data$dataset]]$present]
			)
		))
})

## POINT SELECTION
# set reactive values to observe when a point is clicked on the plot
click_vals<-reactiveValues(d=c())
observe({
	click_data<-event_data("plotly_click", source="main_plot")$pointNumber + 1
	click_vals$d<-which(
		infostore[[plot_data$dataset]]$id == plotinfo[[plot_data$dataset]]$id[click_data]
	)
})

output$plot_status<-renderPrint({
	if(length(click_vals$d)==0){
		cat("")
	}else{
		if(infostore[[plot_data$dataset]]$tested[click_vals$d]){
			if(infostore[[plot_data$dataset]]$selected[click_vals$d]){
				cat("<b>Status:</b> Included")
			}else{
				cat("<b>Status:</b> Excluded")
			}
		}else{
			cat("")
		}
	}
})

output$plot_click<-renderPrint({
	if(length(click_vals$d)==0){
		cat("")
	}else{
		cat(plotinfo[[plot_data$dataset]]$caption[click_vals$d])
	}
})


output$abstract_info<-renderPrint({
	if(length(click_vals$d)==0){
		cat("")
	}else{
		if(any(colnames(plotinfo[[plot_data$dataset]])=="abstract")){
			cat(paste0(
				"<b>Abstract</b><br>",
				plotinfo[[plot_data$dataset]]$abstract[click_vals$d]
			))
		}else{
			cat("")
		}
	}
})


# controls for selector keys
output$select_text<-renderPrint({
	if(length(click_vals$d)>0){
		if(sidebar_tracker$level=="topics"){
			if(sidebar_tracker$content=="articles"){
				cat("<strong>Select Topic?</strong>")
			}else{
				cat("<strong>Exclude Words?</strong>")
			}
		}else{ # i.e. observations
			if(sidebar_tracker$content=="articles"){
				cat("<strong>Select Article?</strong>")
			}else{
				cat("<strong>Exclude Word?</strong>")
			}
		}
	}
})

output$select_yes<-renderPrint({
	if(length(click_vals$d)>0 & sidebar_tracker$content=="articles"){
		actionButton("return_yes", "Select") #, style="color: #fff; background-color: #428bca;")}
	}else{
		cat("")
	}
})

output$select_no<-renderPrint({
	if(length(click_vals$d)>0){
		actionButton("return_no", "Exclude") #, style="color: #fff; background-color: #428bca;")}
	}else{
		cat("")
	}
})

observeEvent(input$return_yes, {
	infostore[[plot_data$dataset]]$tested[click_vals$d]<-TRUE
	infostore[[plot_data$dataset]]$selected[click_vals$d]<-TRUE 
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[click_vals$d]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[click_vals$d]<-"#000000" 
	if(sidebar_tracker$level=="topics"){
		dataset_full<-substr(plot_data$dataset,  1, 1)
		rows<-which(plotinfo[[dataset_full]]$topic==infostore[[plot_data$dataset]]$topic[click_vals$d])
		infostore[[dataset_full]]$tested[rows]<-TRUE
		infostore[[dataset_full]]$selected[rows]<-TRUE
		infostore[[dataset_full]]$topic_counter[rows]<-topic_counter
		infostore[[dataset_full]]$decision_time[rows]<-as.character(Sys.time())
		infostore[[dataset_full]]$color[rows]<-"#000000"
	}	
	click_vals$d<-c()
})

observeEvent(input$return_no, {
	infostore[[plot_data$dataset]]$tested[click_vals$d]<-TRUE
	infostore[[plot_data$dataset]]$selected[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$display[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[click_vals$d]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[click_vals$d]<-"#CCCCCC" 
	# if you have ben altering topics, ensure articles are updated to match
	if(sidebar_tracker$level=="topics"){
		dataset_full<-substr(plot_data$dataset,  1, 1)
		rows<-which(plotinfo[[dataset_full]]$topic==infostore[[plot_data$dataset]]$topic[click_vals$d])
		infostore[[dataset_full]]$tested[rows]<-TRUE
		infostore[[dataset_full]]$selected[rows]<-FALSE
		infostore[[dataset_full]]$display[rows]<-FALSE
		infostore[[dataset_full]]$topic_counter[rows]<-topic_counter
		infostore[[dataset_full]]$decision_time[rows]<-as.character(Sys.time())
		infostore[[dataset_full]]$color[rows]<-"#CCCCCC"
	}else{ # and vice versa
		remaining_points<-xtabs(infostore[[plot_data$dataset]]$display ~ 
			infostore[[plot_data$dataset]]$topic, drop.unused.levels=FALSE)
		if(any(remaining_points<1)){
			lost_topic<-as.numeric(names(remaining_points)[which(remaining_points<1)])
			dataset_tr<-paste0(plot_data$dataset, "_topic")
			row_tr<-which(infostore[[dataset_tr]]$topic==lost_topic)
			infostore[[dataset_tr]]$tested[row_tr]<-TRUE
			infostore[[dataset_tr]]$selected[row_tr]<-FALSE
			infostore[[dataset_tr]]$display[row_tr]<-FALSE
			infostore[[dataset_tr]]$topic_counter[row_tr]<-topic_counter
			infostore[[dataset_tr]]$decision_time[row_tr]<-as.character(Sys.time())
			infostore[[dataset_tr]]$color[row_tr]<-"#CCCCCC" 
		}
	}
	click_vals$d<-c()
})

## TOPIC MODELS
# if asked, re-run LDA
observeEvent(input$go_LDA, {
	infostore$x$present<-infostore$x$display
	infostore$y$present<-infostore$y$display
	# kept_rows<-rows[which(apply(infostore$dtm[rows, cols], 1, sum)>0 )]
	infostore$model<-run_LDA(
		x=infostore$dtm[infostore$x$present, infostore$y$present],
		topic.model=sidebar_tracker$model_type,
		n.topics=input$n_topics,
		iter=input$iterations)

	# static data
	x_matrix<-posterior(infostore$model)$topics # article x topic
	y_matrix<-t(posterior(infostore$model)$terms)
	keep_cols<-c("id", "caption", "abstract")
	initial_captions<-plotinfo$x[, keep_cols[which(keep_cols %in% colnames(plotinfo$x))]]
	plotinfo$x<-data.frame(
		id=rownames(infostore$dtm)[infostore$x$present],
		label= infostore$x$label[infostore$x$present],
		dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(x_matrix, 1, which.max),
		weight= apply(x_matrix, 1, max),
		stringsAsFactors=FALSE
	)
	plotinfo$x<-merge(plotinfo$x, initial_captions, by="id", all.x=TRUE, all.y=FALSE)
	plotinfo$y<-data.frame(
		id=infostore$y$id[infostore$y$present],
		label=rownames(y_matrix),
		dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(y_matrix, 1, which.max),
		weight= apply(y_matrix, 1, max),
		caption=rownames(y_matrix),
		stringsAsFactors=FALSE
	)
	plotinfo$x_topic<-build_topic_df(plotinfo$x, y_matrix)
	plotinfo$y_topic<-build_topic_df(plotinfo$y, x_matrix, type="y", xdata= plotinfo$x)

	topic_counter<-topic_counter+1
})

## EXPORT
# export all data when requested by the 'save' button
observeEvent(input$save, {
	if(input$save_type=="Article Selections (.csv)"){
		ouput<-as.data.frame(cbind(static_list$x, infostore$x[, 2:6]))
		write.csv(ouput, input$saveas)
	}else{
		output<-list(
			x= infostore$x,
			x_topic= infostore$x_topic, 
			y= infostore$y, 
			y_topic= infostore$y_topic,
			dtm=infostore$dtm,
			model=infostore$model
			)
		class(output)<-"review_info"
		saveRDS(output, input$saveas)
	}
})

} # end server

shinyApp(ui, server) # run

} # end function