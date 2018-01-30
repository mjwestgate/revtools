# user call to run a shiny interface to bibliographic data
start_review_window<-function(x){

# throw a warning if a known file type isn't given
if(any(c("bibliography", "review_info", "data.frame")==class(x))==FALSE){
	stop("only classes 'bibliography', 'review_info' or 'data.frame' accepted by start_review_window")}

# get data in a standard format
switch(class(x),
	"bibliography"={info<-as.data.frame(x)},
	"data.frame"={info<-x},
	"review_info"={info<-x$info})

if(class(x)=="review_info"){
	dtm <- x$dtm
	model <- x$model
	plot_list<- x$plotinfo
	x_keep<-x$infostore$x$display
}else{
	cat("building Document Term Matrix\n")
	dtm<-make_DTM(info)
	rownames(dtm)<-paste0("x", c(1:nrow(dtm)))
	x_keep<-apply(dtm , 1, sum)>0
	dtm<-dtm[x_keep, order(colnames(dtm))]
	cat("running Topic Model\n")
	model<-run_LDA(dtm, n_topics=5)
	plot_list<-build_plot_data(info, model, dtm, x_keep)
	palette_initial <- viridisLite::magma(n=model@k, alpha=0.9, begin=0, end=0.9)
}


# create user interface
ui_data<-revtools_ui()
ui<-shinydashboard::dashboardPage(ui_data$header, ui_data$sidebar, ui_data$body)



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
		sidebar_tracker$color_scheme<-input$tabs} # paste0("viridisLite::", input$tabs)}
	if(any(c("lda", "ctm")==input$tabs)){
		sidebar_tracker$model_type<-input$tabs}
})

# create reactiveValues objects to store all data during shiny operations
# 'infostore' updates the display, but not whole plot
if(class(x)=="review_info"){
	infostore<-reactiveValues(
		x = x$infostore$x,
		y = x$infostore$y,
		topic = x$infostore$topic
	)
	modelstore <- reactiveValues(model=x$model)
}else{
	infostore <- build_infostore(plot_list, dtm, palette_initial)
	modelstore <- reactiveValues(model=model)
}
# 'plotinfo' stores the coordinates that are actually drawn
plotinfo<-reactiveValues(
	x = plot_list$x,
	y = plot_list$y,
	topic = plot_list$topic
)
topic_counter<-reactiveValues(x=1) # input$go_LDA # 0 #max(infostore$x$topic_counter)+1



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
		n=modelstore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	infostore$x$color[infostore$x$present]<-palette_tr[plotinfo$x$topic]
		infostore$x$color[which(infostore$x$selected)]<-"#000000"
		infostore$x$color[which(infostore$x$display==FALSE)]<-"#CCCCCC"
	infostore$y$color[infostore$y$present]<-palette_tr[plotinfo$y$topic]
		infostore$y$color[which(infostore$y$selected)]<-"#000000"
		infostore$y$color[which(infostore$y$display==FALSE)]<-"#CCCCCC"
	infostore$topic$color<-palette_tr[plotinfo$topic$topic]
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
				"<b>Topic #", topic_click$d, "</b> | Key terms<br><em>Most likely:</em> ",
				plotinfo$topic$caption[topic_click$d], "<br><em>Heighest weighted:</em> ",
				plotinfo$topic$caption_weighted[topic_click$d]
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
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter$x
	infostore[[plot_data$dataset]]$decision_time[click_vals$d]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[click_vals$d]<-"#000000"
	# note: section needed here to update infostore$topic if changed from excluded to included
	click_vals$d<-c()
})

observeEvent(input$return_no, {
	infostore[[plot_data$dataset]]$tested[click_vals$d]<-TRUE
	infostore[[plot_data$dataset]]$selected[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$display[click_vals$d]<-FALSE
	infostore[[plot_data$dataset]]$topic_counter[click_vals$d]<-topic_counter$x
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
	infostore[[plot_data$dataset]]$topic_counter[rows_tr]<-topic_counter$x
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
	infostore[[plot_data$dataset]]$topic_counter[rows_tr]<-topic_counter$x
	infostore[[plot_data$dataset]]$decision_time[rows_tr]<-as.character(Sys.time())
	infostore[[plot_data$dataset]]$color[rows_tr]<-"#CCCCCC" 
	topic_click$d<-c()
})

# if asked, re-run LDA
observeEvent(input$go_LDA, {
	infostore$x$present<-infostore$x$display
	infostore$y$present<-infostore$y$display
	modelstore$model<-run_LDA(
		x=dtm[infostore$x$present, infostore$y$present],
		topic_model=sidebar_tracker$model_type,
		n_topics=input$n_topics,
		iterations=input$iterations)	
	# update plotinfo
	updated_plot_list<-build_plot_data(info, modelstore$model, 
		dtm[infostore$x$present, infostore$y$present],
		which(infostore$x$present))
	plotinfo$x <- updated_plot_list$x
	plotinfo$y <- updated_plot_list$y
	plotinfo$topic <- updated_plot_list$topic
	# update infostore
	palette_tr<-do.call(sidebar_tracker$color_scheme, list(
		n=modelstore$model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	info_update <- update_infostore(infostore, palette_tr, plotinfo)
	infostore$x <- info_update$x
	infostore$y <- info_update$y
	infostore$topic <- info_update$topic
	topic_counter$x <- topic_counter$x+1
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
			info = info,
			dtm = dtm,
			model = modelstore$model,
			plotinfo = list(x=plotinfo$x, y=plotinfo$y, topic=plotinfo$topic),
			infostore = list(x= infostore$x, y= infostore$y, topic=infostore$topic)
			)
		class(output)<-"review_info"
		saveRDS(output, input$saveas)
	}
})

} # end server

shiny::shinyApp(ui, server) # run

} # end function