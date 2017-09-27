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
	cat("running Topic Model\n")
	model<-run_LDA(dtm, n.topics=5)
	palette_initial <-viridisLite::magma(n=model@k, alpha=0.9, begin=0, end=0.9)

	# data to send to plotinfo
	x_matrix<-topicmodels::posterior(model)$topics # article x topic
	y_matrix<-t(topicmodels::posterior(model)$terms)
	plot_list<-list(
		x=data.frame(
			id=rownames(dtm),
			label= info$label[which(x_keep)],
			ade4::dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
			topic= apply(x_matrix, 1, which.max),
			weight= apply(x_matrix, 1, max),
			caption=apply(info, 1, format_citation_dataframe),
			stringsAsFactors=FALSE),
		y=data.frame(
			id=paste0("y", c(1:nrow(y_matrix))),
			label=rownames(y_matrix),
			ade4::dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
			topic= apply(y_matrix, 1, which.max),
			weight= apply(y_matrix, 1, max),
			caption=rownames(y_matrix),
			stringsAsFactors=FALSE)
		)
	if(any(colnames(info)=="abstract")){plot_list$x$abstract<-info$abstract[x_keep]}
	plot_list$topic<-build_topic_df_simple(plot_list$x, y_matrix)

	# generate info to pass to infostore: it updates the display, but not the whole plot
	# further, this information remains of constant length as go_LDA is run
	dynamic_list<-list(
		x=data.frame(
			id=rownames(dtm),
			label= plot_list$x$label,
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			topic=plot_list$x$topic,
			color=palette_initial[plot_list$x$topic],
			stringsAsFactors=FALSE),
		y=data.frame(
			id=plot_list$y$id,
			label=colnames(dtm),
			frequency=apply(dtm, 2, sum),
			tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
			topic_counter=0,
			decision_time="",
			topic=plot_list$y$topic,
			color=palette_initial[plot_list$y$topic],
			stringsAsFactors=FALSE),
		topic=data.frame(
			id= plot_list$topic$id,
			topic= plot_list$topic$topic,
			tested=FALSE, selected=FALSE, display=TRUE,
			x_count=plot_list$topic$x_count,
			y_count=plot_list$topic$y_count,
			color=palette_initial[plot_list$topic$topic],
			stringsAsFactors=FALSE)
		)
}

# build user interface
header<- shinydashboard::dashboardHeader(title="revtools")
sidebar<-shinydashboard::dashboardSidebar(
	sidebarMenu(
		id="tabs",
		menuItem("Plot", icon=icon("bar-chart-o"),	
			menuSubItem("Articles", tabName="articles", selected=TRUE),
			menuSubItem("Words", tabName="words"),
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
			),
			actionButton("go_LDA", strong("Recalculate"))
		),
		menuItem("Save", icon=icon("save"),
			selectInput("save_type", "File Type", 
				choices=c(".csv (selection data)", ".rds (all data)")),
			textInput("saveas", "Save As:", "revtools_results.csv"),
			actionButton("save", "Save")
		)
	)
)

body<-shinydashboard::dashboardBody(
	fluidRow(
		column(width=8,
			shinydashboard::box(width=NULL,
				shinycssloaders::withSpinner(plotly::plotlyOutput("plot_main"))
			)
		),
		column(width=4, 
			shinydashboard::box(title="Topics", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=FALSE,
				plotly::plotlyOutput("plot_topic")
			),
			shinydashboard::box(
				title="Selected Text", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=FALSE,
				tableOutput("plot_click"),
				shiny::splitLayout(
					uiOutput("select_yes"),
					uiOutput("select_no"),
					cellWidths=c("25%", "25%")
				),
				shiny::splitLayout(
					uiOutput("topic_yes"),
					uiOutput("topic_no"),
					cellWidths=c("25%", "25%")
				)
			),
			shinydashboard::box(title="Abstract", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=TRUE,
				tableOutput("abstract_info")
			)
		)
	)
)

ui<-shinydashboard::dashboardPage(header, sidebar, body)

server <- function(input, output, session) {

options(warn=-1) # hide incompatibility between shiny and plotly
# https://github.com/hrbrmstr/metricsgraphics/issues/49

# create a list that stores all information from sidebar input
sidebar_tracker<-reactiveValues(
	content="articles",	
	dimensions="2d",
	color_scheme="magma",
	model_type="lda")

# update the above as needed
observeEvent(input$tabs, {
	if(any(c("articles", "words")==input$tabs)){
		sidebar_tracker$content<-input$tabs}
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
		y = info$y,
		topic= info$topic,
		dtm= info$dtm,
		model= info$model
	)
	topic_counter<-max(info$x$topic_counter)+1
}else{
	infostore<-reactiveValues(
		x = dynamic_list$x,
		y = dynamic_list$y,
		topic = dynamic_list$topic,
		dtm=dtm,
		model=model
	)
	topic_counter<-1
	}

plotinfo<-reactiveValues(
	x = plot_list$x,
	y = plot_list$y,
	topic = plot_list$topic
)

rm(dynamic_list, plot_list, dtm, model)


## PLOT
# lookup for which data and functions to use in plotly
plot_lookup<-expand.grid(
	content=c("articles", "words"),
	dimensions=c("2d", "3d"),
	stringsAsFactors=FALSE)
plot_lookup<-plot_lookup[order(plot_lookup$content), ]
plot_lookup$tag<-rep(c("x", "y"), each=2)
plot_lookup$fun<-paste("plot1_", rep(c("2D", "3D"), 2), sep="")

# keep track of which plot type and dataset to use
plot_data<-reactiveValues(dataset="x", fun="plot1_2D")
observeEvent({
	sidebar_tracker$content
	sidebar_tracker$dimensions
	}, {
	row_tr<-which(
		plot_lookup$content==sidebar_tracker$content &
		plot_lookup$dimensions==sidebar_tracker$dimensions)
	plot_data$dataset<-plot_lookup$tag[row_tr]
	plot_data$fun<-plot_lookup$fun[row_tr]
})

# update colors as needed
observeEvent({
	sidebar_tracker$content
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
	infostore$y$color[infostore$y$present]<-palette_tr[plotinfo$y$topic]
		infostore$y$color[which(infostore$y$selected)]<-"#000000"
		infostore$y$color[which(infostore$y$display==FALSE)]<-"#CCCCCC"
	infostore$topic$color[infostore$topic$present]<-palette_tr[plotinfo$topic$topic]
		infostore$topic$color[which(infostore$topic$selected)]<-"#000000"
		infostore$topic$color[which(infostore$topic$display==FALSE)]<-"#CCCCCC"
})


# PLOTS
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
	plotly::plotlyProxy("plot_main", session) %>%
		plotly::plotlyProxyInvoke("restyle", list(
			marker=list(
				size = input$point_size, 
				color=infostore[[plot_data$dataset]]$color[infostore[[plot_data$dataset]]$present]
			)
		))
})

# topic barplot
observe({
	output$plot_topic<-renderPlotly({
		plot_article_bar(
			x=plotinfo$topic,
			n=infostore$topic[, paste0(plot_data$dataset, "_count")], 
			color=infostore$topic$color
		)
	})
})


# set reactive values to observe when a point is clicked
click_vals<-reactiveValues(d=c())
observe({
	click_data<-event_data("plotly_click", source="main_plot")$pointNumber + 1
	click_vals$d<-which(
		infostore[[plot_data$dataset]]$id == plotinfo[[plot_data$dataset]]$id[click_data]
	)
	topic_click$d<-c()
})

topic_click<-reactiveValues(d=c())
observe({
	topic_click$d<-event_data("plotly_click", source="topic_plot")$pointNumber + 1
	click_vals$d<-c()
})

output$plot_click<-renderPrint({
	if(length(click_vals$d)==0 & length(topic_click$d)==0){
		cat("")
	}else{
		if(length(click_vals$d)>0){
			# report whether article has already been selected/excluded
			if(infostore[[plot_data$dataset]]$tested[click_vals$d]){
				if(infostore[[plot_data$dataset]]$selected[click_vals$d]){
					status_tr<-"<b>Status:</b> Included</br>"
				}else{
					status_tr<-"<b>Status:</b> Excluded</br>"
				}
			}else{
				status_tr<-""
			}
			row_tr<-which(plotinfo[[plot_data$dataset]]$id == infostore[[plot_data$dataset]]$id[click_vals$d])
			topic_text<-paste0(" [Topic #", plotinfo[[plot_data$dataset]]$topic[row_tr], "]")
			cat(paste0(
				status_tr,
				plotinfo[[plot_data$dataset]]$caption[click_vals$d],
				topic_text, "<br><br>"
			))
		}else{ # topics
			cat(paste0(
				"<b>Topic #", topic_click$d, "</b> ",
				plotinfo$topic$caption[topic_click$d]
			))
		}
	}
})

# selector buttons
output$select_yes<-renderPrint({
	if(length(click_vals$d)>0 & sidebar_tracker$content=="articles"){
		actionButton("return_yes", "Select", style="color: #fff; background-color: #428bca;")
	}
})

output$select_no<-renderPrint({
	if(length(click_vals$d)>0){
		actionButton("return_no", "Exclude", style="color: #fff; background-color: #428bca;")
	}
})

output$topic_yes<-renderPrint({
	if(length(topic_click$d)>0 & sidebar_tracker$content=="articles"){
		actionButton("topic_yes", "Select", style="color: #fff; background-color: #428bca;")
	}
})

output$topic_no<-renderPrint({
	if(length(topic_click$d)>0){
		actionButton("topic_no", "Exclude", style="color: #fff; background-color: #428bca;")
	}
})

# abstracts
output$abstract_info<-renderPrint({
	if(length(click_vals$d)==0){
		cat("")
	}else{
		if(any(colnames(plotinfo[[plot_data$dataset]])=="abstract")){
			abstract_info<-plotinfo[[plot_data$dataset]]$abstract[click_vals$d]
			if(is.na(abstract_info)){cat("No abstract available")
			}else{
				cat(abstract_info)
			}
		}else{cat("No abstracts available")}
	}
})

# click info
observeEvent(input$return_yes, {
	infostore[[plot_data$dataset]]$tested[click_vals$d]<-TRUE
	infostore[[plot_data$dataset]]$selected[click_vals$d]<-TRUE 
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[click_vals$d]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[click_vals$d]<-"#000000"
	# note: section needed here to update infostore$topic if changed from excluded to included
	click_vals$d<-c()
})

observeEvent(input$return_no, {
	infostore[[plot_data$dataset]]$tested[click_vals$d]<-TRUE
	infostore[[plot_data$dataset]]$selected[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$display[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[click_vals$d]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[click_vals$d]<-"#CCCCCC" 
	# update topic plot
	row_tr<-which(plotinfo[[plot_data$dataset]]$id == infostore[[plot_data$dataset]]$id[click_vals$d])
	topic_tr<-plotinfo[[plot_data$dataset]]$topic[row_tr]
	row_tr<-which(infostore$topic$topic==topic_tr)
	col_tr<-paste0(plot_data$dataset, "_count")
	infostore$topic[row_tr, col_tr]<-(infostore$topic[row_tr, col_tr]-1)
	click_vals$d<-c()
})

observeEvent(input$topic_yes, {
	infostore$topic$tested[topic_click$d]<-TRUE
	infostore$topic$selected[topic_click$d]<-TRUE 
	infostore$topic$color[topic_click$d]<-"#000000" 
	# select all points in this topic
	topic_rows<-which(plotinfo[[plot_data$dataset]]$topic == topic_click$d)
	ids_tr<-plotinfo[[plot_data$dataset]]$id[topic_rows]
	rows_tr<-which(infostore[[plot_data$dataset]]$id %in% ids_tr)
	infostore[[plot_data$dataset]]$tested[rows_tr]<-TRUE
	infostore[[plot_data$dataset]]$selected[rows_tr]<-TRUE
	infostore[[plot_data$dataset]]$topic_counter[rows_tr]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[rows_tr]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[rows_tr]<-"#000000" 
	topic_click$d<-c()
})

observeEvent(input$topic_no, {
	# deselect topics
	infostore$topic$tested[topic_click$d]<-TRUE
	infostore$topic$selected[topic_click$d]<-FALSE 
	infostore$topic$color[topic_click$d]<-"#CCCCCC" 
	# deselect all points in this topic
	topic_rows<-which(plotinfo[[plot_data$dataset]]$topic == topic_click$d)
	ids_tr<-plotinfo[[plot_data$dataset]]$id[topic_rows]
	rows_tr<-which(infostore[[plot_data$dataset]]$id %in% ids_tr)
	infostore[[plot_data$dataset]]$tested[rows_tr]<-TRUE
	infostore[[plot_data$dataset]]$selected[rows_tr]<-FALSE
	infostore[[plot_data$dataset]]$display[rows_tr]<-FALSE
	infostore[[plot_data$dataset]]$topic_counter[rows_tr]<-topic_counter
	infostore[[plot_data$dataset]]$decision_time[rows_tr]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[rows_tr]<-"#CCCCCC" 
	topic_click$d<-c()
})

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
	x_matrix<-posterior(infostore$model)$topics # article * topic
	y_matrix<-t(posterior(infostore$model)$terms)
	keep_cols<-c("id", "caption", "abstract")
	initial_captions<-plotinfo$x[, keep_cols[which(keep_cols %in% colnames(plotinfo$x))]]
	# update plotinfo
	x_update<-data.frame(
		id=infostore$x$id[infostore$x$present],
		label= infostore$x$label[infostore$x$present],
		ade4::dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(x_matrix, 1, which.max),
		weight= apply(x_matrix, 1, max),
		stringsAsFactors=FALSE
	)
	x_update<-merge(x_update, initial_captions, by="id", all.x=TRUE, all.y=FALSE)
	plotinfo$x<-x_update[order(x_update$id), ]
	plotinfo$y<-data.frame(
		id=infostore$y$id[infostore$y$present],
		label=rownames(y_matrix),
		ade4::dudi.coa(y_matrix, scannf=FALSE, nf=3)$li,
		topic= apply(y_matrix, 1, which.max),
		weight= apply(y_matrix, 1, max),
		caption=rownames(y_matrix),
		stringsAsFactors=FALSE
	)
	plotinfo$topic<-build_topic_df_simple(plotinfo$x, y_matrix)
	# update infostore
	palette_tr<-do.call(sidebar_tracker$color_scheme, list(
		n=infostore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	col_order<-c("id", "label", "tested", "selected", "display", "present",
		"topic_counter", "decision_time", "topic", "color")
	update_x<-infostore$x[infostore$x$present, which(colnames(infostore$x)!="topic")]	
	update_x<-merge(update_x, plotinfo$x[, c("id", "topic")], all=TRUE)[, col_order]
	update_x$color<-palette_tr[update_x$topic]
	update_x <-as.data.frame(rbind(update_x,
		infostore$x[which(infostore$x$present==FALSE), ]))
	infostore$x<-update_x
	# ditto for y
	col_order<-c(col_order[c(1, 2)], "frequency", col_order[c(3:10)])
	update_y<-infostore$y[infostore$y$present, which(colnames(infostore$y)!="topic")]	
	update_y <-merge(update_y, plotinfo$y[, c("id", "topic")], all=TRUE)[, col_order]
	update_y$color<-palette_tr[update_y$topic]
	update_y <-as.data.frame(rbind(update_y,
		infostore$y[which(infostore$y$present==FALSE), ]))
	infostore$y<-update_y
	# and topics
	infostore$topic<-data.frame(
		id= plotinfo$topic$id,
		topic= plotinfo$topic$topic,
		tested=FALSE, selected=FALSE, display=TRUE,
		x_count= plotinfo$topic$x_count,
		y_count= plotinfo$topic$y_count,
		color= palette_tr[plotinfo$topic$topic],
		stringsAsFactors=FALSE)
	topic_counter<-topic_counter+1
})

## EXPORT
# export all data when requested by the 'save' button
observeEvent(input$save, {
	if(input$save_type==".csv (selection data)"){
		ouput<-merge(info, 
			infostore$x[, c("label", "tested", "selected", "topic", "topic_counter", "decision_time")], 
			by="label")
		write.csv(ouput, input$saveas, row.names=FALSE)
	}else{
		output<-list(
			x= infostore$x,
			y= infostore$y, 
			topic= infostore$topic,
			dtm=infostore$dtm,
			model=infostore$model
			)
		class(output)<-"review_info"
		saveRDS(output, input$saveas)
	}
})

} # end server

shiny::shinyApp(ui, server) # run

} # end function