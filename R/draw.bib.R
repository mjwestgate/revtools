# user call to run a shiny interface to bib data
# this differs from previous version in that all data in stored in a single reactiveObject, which can then be exported.

draw.bib<-function(x){ # input can be an object of class("bibdata" | "bibselector")

# throw a warning if a known file type isn't given
# Note that this precludes the user from giving a file name - possibly a usfeul feature
if(any(c("bibdata", "bibselector")==class(x))==FALSE){
	stop("only class(bibdata) or class(bibselector) accepted by draw.bib()")}

# if new data given, calculate all of the relevant entries
if(class(x)=="bibdata"){
	selector<-data.frame(
		labels=names(x),
		x=0, y=0, topic=0, # to be filled by model results later
		tested=FALSE,
		selected=FALSE,
		display=FALSE,
		decisionTime=NA,
		color="black",
		order=c(1:length(x)),
		stringsAsFactors=FALSE)
	cat("building Document Term Matrix\n")
	dtm<-getDTM(x)
	selector$display<-apply(dtm , 1, sum)>0
	cat("running Topic Model\n")
	default.topics<-round(length(which(selector$display))*0.05, 0)
		if(default.topics > 30){default.topics<-30}
		if(default.topics < 3){default.topics<-5}
	model<-LDAfun(dtm[selector$display, ])
	cat("building plot data\n")
	# start with article-word matrix
	lda.weights<-posterior(model)$topics
	lda.coa<-dudi.coa(lda.weights, scannf=FALSE, nf=2) 
	selector$x[selector$display]<-lda.coa$li[, 1]
	selector$y[selector$display]<-lda.coa$li[, 2]
	selector$topic[selector$display]<-apply(lda.weights, 1, which.max)
	# then topics
	topic.coa<-as.data.frame(do.call(rbind, lapply(
		split(selector[, c("x", "y")], selector$topic), function(a){
		apply(a, 2, mean)
		})))
	topic.dframe<-data.frame(
		topic=sort(unique(selector$topic)),
		x=topic.coa$x,
		y=topic.coa$y,
		count=as.numeric(xtabs(~ topic, data= selector)),
		tested=0,
		selected=0,
		display=TRUE,
		color="black",
		stringsAsFactors=FALSE)
	lda.terms<-t(posterior(model)$terms)
	# lda.terms<-lda.terms/apply(lda.terms, 1, sum)
	}

# build simple user interface
ui<-fluidPage(

	# build sidebar to contain useful information
	column(width=2,
		h4("Controls"),
		checkboxInput("show_display", strong("Display options"), value=FALSE),
		conditionalPanel(condition="input.show_display==true",
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
				sliderInput("pointsize_plot1", "articles", min=0.5, max=4.5, step=0.5, value= 2),
				sliderInput("pointsize_plot2", "topics", min=1, max=9, step=1, value= 4)
				)
			), # end conditionalPanel
		# LDA options
		checkboxInput("show_LDA", strong("Topic Model options"), value=FALSE),
		conditionalPanel(condition="input.show_LDA==true",
			selectInput("modeltype", "model type", choices=c("LDA", "CTM")),
			uiOutput("topic_slider"),
			actionButton("go_LDA", strong("recalculate model"), width="100%", 
				style="color: #fff; background-color: #428bca;")
			), # end conditionalPanel
		# Save options
		checkboxInput("show_save", strong("Save options"), value=FALSE),
		conditionalPanel(condition="input.show_save==true",
			textInput("saveas", "save as:", "bibviewr_results.rds"),
			actionButton("save", "save")
			)
		),

	# main window
	column(width = 7, 
		plotOutput("plot1", height = 500, # show ordination in main window
			click = "plot_click", 
			hover=hoverOpts(id="plot_hover", delay=100, clip=TRUE)),
		tags$style(type="text/css","#info_hover { color:#404040;}"), 
		tableOutput("text")  # to display abstracts
		), # end main panel
	
	#  right-hand column, to select show topics and allow selection
	column(width=3,
		# add barchart here
		plotOutput("plot2", height = 250, # show ordination in main window
			click = "plot2_click", 
			hover=hoverOpts(id="plot2_hover", delay=100, clip=TRUE)),
		# then text to select relevant articles
		tableOutput("select_text"), br(),
		uiOutput("select_yes"), br(),
		uiOutput("select_no")
		)

) # end ui builder


# code for what to draw
server <- function(input, output) {

# create reactiveValues object to store all data during shiny operations
if(class(x)=="bibdata"){
	infostore<-reactiveValues(
		DATA= x,
		DTM=dtm,
		LDA=model,
		TERMS= list(
			absolute=lda.terms,
			relative=lda.terms/apply(lda.terms, 1, sum)),
		SEL=selector,
		TOPIC=topic.dframe)
}else{ # i.e. if(class(x)=="bibselector")
	infostore<-reactiveValues(
		DATA= x$DATA,
		DTM=x$DTM,
		LDA=x$LDA,
		TERMS=x$TERMS,
		SEL=x$SEL,
		TOPIC=x$TOPIC)
	}


# set LDA options
# set up slider to select the number of topics
output$topic_slider<-renderUI({
	sliderInput("n.topics", "number of topics", min=3, max=30, value= infostore$LDA@k)
	})

# if asked, re-run LDA
observeEvent(input$go_LDA, {
	rows<-which(infostore$SEL$display)
	model<-LDAfun(infostore$DTM[rows, ], 
		topic.model=input$modeltype, 
		n.topics=input$n.topics)
	lda.weights<-posterior(model)$topics
	palette<-do.call(input$color, list(
		n=model@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	
	# update selector information
	lda.coa<-dudi.coa(lda.weights, scannf=FALSE, nf=2) 
	selector.new<-data.frame(
		labels=infostore$SEL$labels[rows],
		x=lda.coa$li[, 1],
		y=lda.coa$li[, 2],	
		topic=apply(lda.weights, 1, which.max),
		stringsAsFactors=FALSE)
	selector.new<-merge(
		selector.new, 
		infostore$SEL[, -c(2:4)],
		by="labels",
		all=TRUE)
	selector.new$color<-palette[selector.new$topic]

	# update topic information
	# Note that the content below has no article-specific component,
	# and therefore does not require adjustment by row
	topic.coa<-as.data.frame(do.call(rbind, lapply(
		split(selector.new[, c("x", "y")], selector.new$topic), function(a){
		apply(a, 2, mean)
		})))
	topic.dframe<-data.frame(
		topic=sort(unique(selector.new$topic)),
		x=topic.coa$x,
		y=topic.coa$y,
		count=as.numeric(xtabs(~ topic, data= selector.new)),
		stringsAsFactors=FALSE)
	topic.dframe$tested= topic.dframe$count - xtabs(~ topic + tested, data= selector.new)[, 1]
	topic.dframe$selected= topic.dframe$count - xtabs(~ topic + selected, data= selector.new)[, 1]
	topic.dframe$display<-TRUE
	topic.dframe$color<-palette[topic.dframe$topic]

	lda.terms<-t(posterior(model)$terms)

	# update infostore
	infostore$LDA<-model
	infostore$TERMS<-list(
		absolute=lda.terms,
		relative=lda.terms/apply(lda.terms, 1, sum))
	infostore$SEL<-selector.new[order(selector.new$order), ]
	infostore$TOPIC<-topic.dframe
	})


# set colors
observeEvent(input$color, {
	palette<-do.call(input$color, list(
		n=infostore$LDA@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	infostore$SEL$color<-palette[infostore$SEL$topic]
	infostore$TOPIC$color<-palette
	})
observeEvent(input$color_alpha, {
	palette<-do.call(input$color, list(
		n=infostore$LDA@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	infostore$SEL$color<-palette[infostore$SEL$topic]
	infostore$TOPIC$color<-palette
	})
observeEvent(input$color_hue, {
	palette<-do.call(input$color, list(
		n=infostore$LDA@k, 
		alpha=input$color_alpha,
		begin=input$color_hue[1],
		end=input$color_hue[2]))
	infostore$SEL$color<-palette[infostore$SEL$topic]
	infostore$TOPIC$color<-palette
	})

# set reactive values to observe when a point is hovered or clicked
# NOTE: need to use SEL$tested and SEL$selected to 'hide' 
plot_interaction<-reactiveValues(hover=NULL, click=NULL)
click_recorder<-reactiveValues(row=NULL)
hover_recorder<-reactiveValues(row=NULL)


## PLOT 1 Interactivity
# set values if hovered
observeEvent(input$plot_hover, {
	res <- nearPoints(df=infostore$SEL[which(
		c(infostore$SEL$tested & infostore$SEL$selected==FALSE)==FALSE), ],
		coordinfo=input$plot_hover,
		xvar="x", yvar="y",
		threshold=10,
		maxpoints=1)
	if(nrow(res)==1){
		selector.hover<-which(names(infostore$DATA)==res$labels[1])
		plot_interaction$hover<-paste(
			pretty.citations(infostore$DATA[[res$labels[1]]], abstract=TRUE),
			paste("<br><em>Article number ", selector.hover, "</em>", sep=""))
		hover_recorder$row<-selector.hover
	}else{plot_interaction$hover<-NULL}
	})
# duplicate for clicking
observeEvent(input$plot_click, {
	res <- nearPoints(df=infostore$SEL[which(
		c(infostore$SEL$tested & infostore$SEL$selected==FALSE)==FALSE), ],
		coordinfo=input$plot_click,
		xvar="x", yvar="y",
		threshold=10,
		maxpoints=1)
	if(nrow(res)==1){
		selector.click<-which(names(infostore$DATA)==res$labels[1])
		plot_interaction$click<-paste(
			pretty.citations(infostore$DATA[[res$labels[1]]], abstract=TRUE),
			paste("<br><em>Article number ", selector.click, "</em>", sep=""))
		click_recorder$row<-selector.click
	}else{plot_interaction$click<-NULL}
	})

# draw an ordination of articles
output$plot1<-renderPlot({
	z<-infostore$SEL[which(infostore$SEL$display),]
	par(mar=rep(0.5, 4))
	plot(y ~ x, data=z, type="n", ann=FALSE, axes=FALSE)
	box(bty="o", col="grey50")
	if(any(z$selected)){
		points(y ~ x, data=z[which(z$selected), ], 
			pch=21, 
			cex=input$pointsize_plot1, 
			col=z$color[which(z$selected)])}
		points(y ~ x, data=z[which(z$selected==FALSE), ], 
			pch=16, 
			cex=input$pointsize_plot1, 
			col=z$color[which(z$selected==FALSE)])
	}) # end plot1


## PLOT 2 Interactivity
# set values if hovered
observeEvent(input$plot2_hover, {
	res <- nearPoints(df=infostore$TOPIC,
		coordinfo=input$plot2_hover,
		xvar="x", yvar="y",
		threshold=10,
		maxpoints=1)
	if(nrow(res)==1){
		selector.hover<-list(
			absolute=sort(infostore$TERMS$absolute[, res$topic[1]], decreasing=TRUE)[1:10],
			relative=sort(infostore$TERMS$relative[, res$topic[1]], decreasing=TRUE)[1:10])
		plot_interaction$hover<-paste(
			"<strong>Highest weighted terms<br>absolute:</strong><br>",
			paste(names(selector.hover$absolute), collapse=", "),
			"<br><strong>relative:</strong><br>",
			paste(names(selector.hover$relative), collapse=", "), sep="")
		hover_recorder$row<-which(infostore$SEL$topic==res$topic[1])
	}else{plot_interaction$hover<-NULL}
	})
# set values if clicked
observeEvent(input$plot2_click, {
	res <- nearPoints(df=infostore$TOPIC,
		coordinfo=input$plot2_click,
		xvar="x", yvar="y",
		threshold=10,
		maxpoints=1)
	if(nrow(res)==1){
		selector.click<-list(
			absolute=sort(infostore$TERMS$absolute[, res$topic[1]], decreasing=TRUE)[1:10],
			relative=sort(infostore$TERMS$relative[, res$topic[1]], decreasing=TRUE)[1:10])
		plot_interaction$click<-paste(
			"<strong>Highest weighted terms<br>absolute:</strong><br>",
			paste(names(selector.click$absolute), collapse=", "),
			"<br><strong>relative:</strong><br>",
			paste(names(selector.click$relative), collapse=", "), sep="")
		click_recorder$row<-which(infostore$SEL$topic==res$topic[1])
	}else{plot_interaction$click<-NULL}
	})

# draw a key to topics
output$plot2<-renderPlot({
	z<-infostore$TOPIC
	par(mar=rep(0.5, 4))
	plot(y ~ x, data=z[which(z$display), ], 
		xlim=range(infostore$SEL$x[infostore$SEL$display]), 
		ylim=range(infostore$SEL$y[infostore$SEL$display]),
		type="n", ann=FALSE, axes=FALSE)
	points(y ~ x, data=z[which(z$display), ], 
		pch=16, 
		cex=input$pointsize_plot2, 
		col=z$color[which(z$display)])
	box(bty="o", col="grey50")
	})
	# ?pie


# show abstracts
output$text<-renderPrint({
	if(is.null(plot_interaction$click) & is.null(plot_interaction$hover)){cat("")
	}else{
		if(is.null(plot_interaction$click)==FALSE){cat(plot_interaction$click)
		}else{cat(plot_interaction$hover)}
		}
	})	


# article selector
output$select_text<- renderPrint({
	 if(is.null(plot_interaction$click)==FALSE){cat("<strong>Select?</strong>")} })
output$select_yes<- renderUI({
	 if(is.null(plot_interaction$click)==FALSE){
		actionButton("return_yes", "Yes", style="color: #fff; background-color: #428bca;")} })
output$select_no<- renderUI({
	 if(is.null(plot_interaction$click)==FALSE){
		actionButton("return_no", "No", style="color: #fff; background-color: #428bca;")} })


# link action buttons to select/deselect articles to article status
observeEvent(input$return_yes, {
	infostore$SEL$tested[click_recorder$row]<-TRUE
	infostore$SEL$selected[click_recorder$row]<-TRUE
	infostore$SEL$display<-(infostore$SEL$tested & infostore$SEL$selected==FALSE)==FALSE
	infostore$SEL$decisionTime[click_recorder$row]<-as.character(Sys.time())
	# update topic counts		
	infostore$TOPIC$tested=infostore$TOPIC$count - xtabs(~ topic + tested, data= infostore$SEL)[, 1]
	infostore$TOPIC$selected=infostore$TOPIC$count - xtabs(~ topic + selected, data= infostore$SEL)[, 1]
	infostore$TOPIC$display<-(infostore$TOPIC$count==infostore$TOPIC$tested & 
		infostore$TOPIC$selected==0)==FALSE
	})
observeEvent(input$return_no, {
	infostore$SEL$tested[click_recorder$row]<-TRUE
	infostore$SEL$selected[click_recorder$row]<-FALSE
	infostore$SEL$display<-(infostore$SEL$tested & infostore$SEL$selected==FALSE)==FALSE
	infostore$SEL$decisionTime[click_recorder$row]<-as.character(Sys.time())
	# update topic counts		
	infostore$TOPIC$tested=infostore$TOPIC$count - xtabs(~ topic + tested, data= infostore$SEL)[, 1]
	infostore$TOPIC$selected=infostore$TOPIC$count - xtabs(~ topic + selected, data= infostore$SEL)[, 1]
	infostore$TOPIC$display<-(infostore$TOPIC$count==infostore$TOPIC$tested & 
		infostore$TOPIC$selected==0)==FALSE
	})
# ensure that when an article is classified, it is deselected
observeEvent(input$return_yes, {plot_interaction$click<-NULL})
observeEvent(input$return_no, {plot_interaction$click<-NULL})


# export all data when requested by the 'save' button
observeEvent(input$save, {
	output<-list(
		DATA=infostore$DATA,
		DTM= infostore$DTM, 
		LDA=infostore$LDA,
		TERMS=infostore$TERMS,
		SEL=infostore$SEL,
		TOPIC=infostore$TOPIC)
	class(output)<-"bibselector"
	saveRDS(output, input$saveas)
	})

} # end server

shinyApp(ui, server) # run
} # end function