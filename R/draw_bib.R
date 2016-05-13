
draw.bib<-function(x){

bibliography<-x$bibliography
dtm<-x$dtm
coauthors<-x$coauthors

ui <- fluidPage(theme=shinytheme("spacelab"),

	# add some custom css - altered from code in Shiny
	# primary goal is to alter slider color from blue to grey
	tags$style(type="text/css", "body {background-color:#fff;}

	.irs-bar {
	    height: 8px; top: 25px;
	    border-top: 1px solid #969696;
	    border-bottom: 1px solid #969696;
	    background: #969696;
	}
	    .irs-bar-edge {
	        height: 8px; top: 25px;
	        width: 14px;
	        border: 1px solid #969696;
	        border-right: 0;
	        background: #969696;
	        border-radius: 16px 0 0 16px;
	        -moz-border-radius: 16px 0 0 16px;
	    }
	
	.irs-from, .irs-to, .irs-single {
	    color: #fff;
	    font-size: 11px; line-height: 1.333;
	    text-shadow: none;
	    padding: 1px 3px;
	    background: #969696;
	    border-radius: 3px;
	    -moz-border-radius: 3px;
	}
	
	.irs-grid {
	    height: 27px;
	}
	.irs-grid-pol {
	    opacity: 0.5;
	    background: #969696;
	}
	.irs-grid-pol.small {
	    background: #999;
	}
	
	.irs-grid-text {
	    bottom: 5px;
	    color: #969696;
	}
	"), 

	# set first row
	fluidRow(

		# SIDE BAR
		column(width=2,
			h4("Controls"),
			actionButton("go_LDA", strong("(re)calculate LDA"), width="100%", 
				style="color: #fff; background-color: #428bca;"),
			# LDA control
			#h4("Show LDA options"), 
			checkboxInput("show_LDA", strong("Show LDA options"), value=FALSE),
			conditionalPanel(condition="input.show_LDA==true",
				uiOutput("topic_slider"),
				sliderInput("n.iter", "# iterations", 
					min=1000, max=5000, value=1000, step=1000),
				checkboxInput("retain_points", "Retain excluded points?", value=TRUE),
				selectInput("col_selector", "color scheme", choices=c(
					"Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
					selected="Dark2", multiple=FALSE, width="100%")
				), # end LDA conditionalPanel

			# Display options
			checkboxInput("show_display", strong("Show display options"),  value=TRUE),
			conditionalPanel(condition="input.show_display==true",
				checkboxInput("show_bib_details", "Show bibliographic details?",  value=FALSE),
				checkboxInput("show_coauthors", "Show coauthorship?",  value=FALSE),
				checkboxInput("show_citations", "Show citations?",  value=FALSE),
				conditionalPanel(condition="input.show_citations==true",
					sliderInput("expand_citations", "expansion factor", min=1, max=6, value=3, step=1)), 
				sliderInput("point_size", "point size", min=0.5, max=4, value=1.5, step=0.5)
				), # end Display conditionalPanel

			# Export options
			checkboxInput("show_export", strong("Show export options"),  value=FALSE),
			conditionalPanel(condition="input.show_export==true",
				actionButton("export_all", "export all",
					style="color: #fff; background-color: #337ab7;"),
				br(), br(),
				actionButton("export_selected", "export selected",
					style="color: #fff; background-color: #337ab7;")
				) # end export conditionalPanel

		),

		# MAIN WINDOW
		column(width = 7, 
			plotOutput("plot1", height = 500, 
				click = "plot_click", 
				dblclick="plot_dblclick",
				brush=brushOpts(id="plot_brush", resetOnNew=TRUE),
				hover=hoverOpts(id="plot_hover", delay=0)),
			tags$style(type="text/css","#info_hover { color:#404040;}"), 
			tableOutput("text")
			),

		# PROGRESS INFO.
		column(width=3,

			# Review info - make conditional?
			#  topic barplot 
			plotOutput("plot_bar", height = 250),
			# progress bar
			plotOutput("plot_progress", height=100),
			# information on highlighted words
			tableOutput("plot_words"),
			# select options
			br(), tableOutput("select_text"), br(),
			uiOutput("select_yes"), br(),
			uiOutput("select_maybe"), br(),
			uiOutput("select_no")

			# consider adding a taxize option here.

			)

	) # end fluidRow 1
) # end fluidPage



# draw interactive plots
server <- function(input, output) {

	# set up slider to select the number of topics
	# wellPanel(sliderInput("n.topics", "# topics", min=1, max=12, value=5))
	output$topic_slider<-renderUI({
		sliderInput("n.topics", "# topics", min=1, 
			max=brewer.pal.info[input$col_selector, ]$maxcolors, value=5)
		})

	# set up a variable to catch decisions re: article inclusion
	article.selector <- reactiveValues(status = rep(NA, nrow(dtm)))

	# (re)run LDA when requested
	LDA_data<-reactiveValues(model=NULL, coords=NULL, palette=NULL)
	current.display<-reactiveValues(rows=c(1:nrow(dtm)))
	observeEvent(input$go_LDA, {
		# select relevant data
		if(input$retain_points | any(article.selector$status =="no", na.rm=TRUE)==FALSE){
			data.tr<-dtm
		}else{
			present.rows<-which(is.na(article.selector$status)==FALSE)
			no.rows<-which(article.selector$status[present.rows]=="no")
			current.display$rows <-c(1:nrow(dtm))[-present.rows[no.rows]]
			data.tr<-dtm[current.display$rows, ]
			data.tr<-data.tr[, which(apply(data.tr, 2, sum)>0)]}
		model<-LDAfun(data.tr, n.topics=input$n.topics, iter=input$n.iter)
		lda.weights<-posterior(model)$topics
		lda.coa<-dudi.coa(lda.weights, scannf=FALSE, nf=2) 
		# lda.coa<-dudi.coa(dtm[, which(apply(dtm, 2, sum)>2)], scannf=FALSE, nf=2) # alternative option
		# note: using lda.weights emphasizes model-based differences more than ordination of raw data
		# whether or not this is desirable is difficult to know.
		coords<-lda.coa$li
		colnames(coords)<-c("x", "y")
		coords$labels<-names(bibliography)[current.display$rows]
		palette.tr<-brewer.pal(input$n.topics, input$col_selector)
		coords$colors<-palette.tr[topics(model)]
		LDA_data$coords<-coords
		LDA_data$model<-model
		LDA_data$palette<-palette.tr
		})

	# allow point sizes to be relative to citation rates if requested
	# note: requires observation of whether to plot, and how much to increase size by.	
	point.expansion<-reactiveValues(add=rep(1, length(bibliography)))
	observeEvent(input$show_citations, {
		if(input$show_citations){
			raw.data<-citations.n(bibliography)$n
			add.vals<-1+((raw.data/max(raw.data))*input$expand_citations)
			point.expansion$add<-add.vals
		}else{point.expansion$add<-rep(1, length(bibliography))}
		})
	observeEvent(input$expand_citations, {
		if(input$show_citations){
			raw.data<-citations.n(bibliography)$n
			add.vals<-1+((raw.data/max(raw.data))*input$expand_citations)
			point.expansion$add<-add.vals
		}else{point.expansion$add<-rep(1, length(bibliography))}
		})

	# set reactive values to observe when a point is hovered or clicked	
	plot_interaction<-reactiveValues(hover=NULL, click=NULL)
	click_recorder<-reactiveValues(row=NULL)
	hover_recorder<-reactiveValues(row=NULL)
	# set values if hovered
	observeEvent(input$plot_hover, {
		if(is.null(LDA_data$coords)==FALSE){
		res <- nearPoints(df=LDA_data$coords, coordinfo=input$plot_hover,
			xvar="x", yvar="y",
			threshold=10,
			maxpoints=1)
		if(nrow(res)==1){
			selector<-which(names(bibliography)==res$label)
			plot_interaction$hover<-paste(
				pretty.citations(bibliography[[selector]], abstract=TRUE, details=input$show_bib_details),
				paste("<br><em>Article number ", selector, "</em>", sep=""))
			hover_recorder$row<-selector
		}else{plot_interaction$hover<-NULL}} # necessary?
		})
	# and clicking
	observeEvent(input$plot_click, {
		if(is.null(LDA_data$coords)==FALSE){
		res <- nearPoints(df=LDA_data$coords, coordinfo=input$plot_click,
			xvar="x", yvar="y",
			threshold=10,
			maxpoints=1)
		if(nrow(res)==1){
			selector<-which(names(bibliography)==res$label)
			plot_interaction$click<-paste(
				pretty.citations(bibliography[[selector]], abstract=TRUE, details=input$show_bib_details),
				paste("<br><em>Article number ", selector, "</em>", sep=""))
			click_recorder$row<-selector
		}else{
			plot_interaction$click <-NULL}}
		})

	# ARTICLE SELECTOR
	output$select_text<- renderPrint({
		 if(is.null(plot_interaction$click)==FALSE){cat("<strong>Select?</strong>")} })
	output$select_yes<- renderUI({
		 if(is.null(plot_interaction$click)==FALSE){
			actionButton("return_yes", "Yes", style="color: #fff; background-color: #428bca;")} })
	output$select_maybe<- renderUI({
		 if(is.null(plot_interaction$click)==FALSE){
			actionButton("return_maybe", "Maybe", style="color: #fff; background-color: #428bca;")} })
	output$select_no<- renderUI({
		 if(is.null(plot_interaction$click)==FALSE){
			actionButton("return_no", "No", style="color: #fff; background-color: #428bca;")} })

	# need a separate system to track which points to keep
	# point.display <- reactiveValues(status = rep(NA, nrow(dtm)))

	# link action buttons to select/deselect articles to article status
	observeEvent(input$return_yes, {article.selector$status[click_recorder$row]<-"yes"})
	observeEvent(input$return_maybe, {article.selector$status[click_recorder$row]<-"maybe"})
	observeEvent(input$return_no, {article.selector$status[click_recorder$row]<-"no"})
	# ensure that when an article is classified, it is deselected
	observeEvent(input$return_yes, {plot_interaction$click<-NULL})
	observeEvent(input$return_maybe, {plot_interaction$click<-NULL})
	observeEvent(input$return_no, {plot_interaction$click<-NULL})

	# draw plots
	ordination.ranges<-reactiveValues(x=NULL, y=NULL) # for zooming

	# ORDINATION
	output$plot1 <- renderPlot({
		if(is.null(LDA_data$model))return()
		else{
		# draw 
		par(mar=rep(0.5, 4), cex=input$point_size)
		# if zoomed, correct ranges accordingly
		if(is.null(ordination.ranges$x)){
			plot(y ~ x, data= LDA_data$coords, type="n", ann=FALSE, axes=FALSE)
		}else{
			plot(y ~ x, data= LDA_data$coords, 
				xlim=ordination.ranges$x, ylim=c(ordination.ranges$y),
				type="n", ann=FALSE, axes=FALSE)
		}
		# add lines
		if(is.null(click_recorder$row)==FALSE){
			name.tr<-names(bibliography)[click_recorder$row]
			point.tr<-which(LDA_data$coords$label==name.tr)
			if(any(coauthors$a1== name.tr) & input$show_coauthors){
				row1<-which(coauthors$a1== name.tr)
				names2<-coauthors$a2[row1]
				row2<-which(sapply(names(bibliography)[current.display$rows], function(a, lookup){
					if(any(lookup==a)){TRUE}else{FALSE}}, lookup=names2))
				result.list<-lapply(as.list(row2), function(a, from, lookup){
					list(x= as.numeric(lookup$x[c(from, a)]), 
						y=as.numeric(lookup$y[c(from, a)]), 
						col="grey30", lwd=2)},
					from= point.tr,
					lookup=LDA_data$coords)
				invisible(lapply(result.list, function(a){do.call(lines, a)}))
				points(y~x, data=LDA_data$coords[row2, ], 
					 cex= point.expansion$add[current.display$rows[row2]]+1, pch=21, bg="white", col="grey30")
				points(y~x, data=LDA_data$coords[point.tr, ], 
					 cex= point.expansion$add[current.display$rows[point.tr]]+1, 
					pch=21, bg="white", col="black")
			}else{
				name.tr<-names(bibliography)[click_recorder$row]
				point.tr<-which(LDA_data$coords$label==name.tr)
				points(y~x, data=LDA_data$coords[point.tr, ], 
					 cex= point.expansion$add[current.display$rows[point.tr]]+1, bg="white", col="black")		
			}}
		# draw points as required given their selected status
		point.status<-article.selector$status[current.display$rows]
		if(any(is.na(point.status))){
			rows<-which(is.na(point.status))
			points(y ~ x, data= LDA_data$coords[rows, ], 
				pch=16, col=LDA_data$coords$colors[rows], 
				cex= point.expansion$add[current.display$rows[rows]])}
		if(any(point.status =="yes", na.rm=TRUE)){
			rows.valid<-which(is.na(point.status)==FALSE)
			rows<-rows.valid[which(point.status[rows.valid]=="yes")]
			points(y ~ x, data= LDA_data$coords[rows, ], pch=21, 
				bg=LDA_data$coords$colors[rows], col="black", 
				cex= point.expansion$add[current.display$rows[rows]])}
		if(any(point.status =="maybe", na.rm=TRUE)){
			rows.valid<-which(is.na(point.status)==FALSE)
			rows<-rows.valid[which(point.status[rows.valid]=="maybe")]
			points(y ~ x, data= LDA_data$coords[rows, ], pch=21, 
				col=LDA_data$coords$colors[rows], bg="grey", 
				cex= point.expansion$add[current.display$rows[rows]])}
		if(any(point.status =="no", na.rm=TRUE)){
			rows.valid<-which(is.na(point.status)==FALSE)
			rows<-rows.valid[which(point.status[rows.valid]=="no")]
			points(y ~ x, data= LDA_data$coords[rows, ], 
				pch=1, col=LDA_data$coords$colors[rows], 
				cex= point.expansion$add[current.display$rows[rows]])}
		# add count of number of points displayed
		mtext(paste(nrow(LDA_data$coords), "/", nrow(dtm), "points shown", sep=" "),
			side=1, col="grey40", adj=1, cex=0.8, line=-1)	
		}
	})

	# ZOOM observation
	observeEvent(input$plot_dblclick, {
		brush<-input$plot_brush
		if(!is.null(brush)){
			ordination.ranges$x<-c(brush$xmin, brush$xmax)
			ordination.ranges$y<-c(brush$ymin, brush$ymax)
		}else{
			ordination.ranges$x<-NULL
			ordination.ranges$y<-NULL
		}
	})

	# BARPLOT
	output$plot_bar <- renderPlot({
		if(is.null(LDA_data$model))return()
		else{
			topic.vec<-topics(LDA_data$model)
			n.topics<-max(topic.vec)
			selector.vector<-addNA(factor(article.selector$status[current.display$rows], 
				levels=c("no", "maybe", "yes"), exclude=FALSE))	
			# xtab to get frequencies in each topic/selection category
			result.mat<-xtabs( ~ topic.vec + selector.vector,
				na.action=na.pass,	exclude=NULL,
				drop.unused.levels=FALSE)
			result<-as.data.frame(result.mat)
			colnames(result)<-c("topic", "selected", "n")
			# if there are NA values, incorporate them with 'maybe's
			if(any(is.na(result$selected))){
				result$n[which(result$selected=="maybe")]<-apply(cbind(
					result$n[which(result$selected=="maybe")],
					result$n[which(is.na(result$selected))]),
					1, sum)
				result<-result[-which(is.na(result$selected)), ]
				}
			
			# set colors
			palette.pale<-adjustcolor(LDA_data$palette, alpha.f=0.5)
			result$fill<-c(rep("white", n.topics), palette.pale, LDA_data$palette)
			result$border<-rep(LDA_data$palette, 3)		
			# set x coordinates
			xvals<-seq(0, 1, length.out=(n.topics+1))
			xvals<-xvals[-length(xvals)]
			xvals<-xvals+(xvals[2]*0.5)
			# differences between bars
			xdiff<-xvals[1]*0.8
			xbars<-data.frame(initial=xvals-xdiff, final=xvals+xdiff)
			
			# draw
			par(mar=c(3, 0.5, 2, 0.5))
			plot(1~1, type="n", ann=FALSE, axes=FALSE, ylim=c(1, 0), xlim=c(max(result$n)+1, 0))
			xlabs<-axTicks(1)
			axis(1, at= xlabs, labels=rep("", length(xlabs)), col="grey30", tcl=0.5)
			axis(1, at=xlabs, line=-0.5, lwd=0)
			# axis(4, at=xvals, labels=c(1:n.topics), lwd=0, line=-1, las=1) 
			# add bars
			result.list<-split(result, result$topic)
			for(i in 1:n.topics){
				data.tr<-result.list[[i]][3:1, ]
				yvals<-c(0, cumsum(data.tr$n))
				for(k in 1:3){
					polygon(y= as.numeric(xbars[i, ])[c(1, 2, 2, 1)],
						x= yvals[c(k, k+1)][c(1, 1, 2, 2)],
						border= data.tr$border[k],
						col=data.tr$fill[k])
					}
				}	
			# topic labels
			terms.plot<-terms(LDA_data$model, 3)
			labels.tr<-apply(terms.plot, 2, function(a){paste(a, collapse=" ")})
			text(x=rep(0, length(labels.tr)), y=xvals, labels=labels.tr, pos=2, cex=1, col="grey40")				
			# axis labels
			mtext("Topic", side=3, adj=0.9, line=0)
			mtext("# Articles", side=1, adj=0.5, line=2)
		}
	})

	# temporal trends?

	# PROGRESS BAR
	output$plot_progress <- renderPlot({
		if(is.null(LDA_data$model))return()
		else{
			complete<-length(which(is.na(article.selector$status)==FALSE))
			total<-nrow(dtm)
			pc.complete<-(1/total)*complete	
			# draw
			par(mar=c(3, 4, 3, 2))
			plot(1~1, type="n", ann=FALSE, axes=FALSE, xlim=c(0, 1), ylim=c(0, 1))
			polygon(x=c(0, 1, 1, 0), y=c(0, 0, 1, 1), border="grey30")
			polygon(x=c(0, pc.complete, pc.complete, 0), 
				y=c(0, 0, 1, 1), col=adjustcolor("darkblue", alpha.f=0.4), 
				border="darkblue")
			axis(1, at=seq(0, 1, 0.5), labels=c(0, 50, 100), lwd=0, line=-1)
			abline(v=0.5, col="grey30")
			mtext(	
				paste(complete, " of ", total, " (", round((pc.complete*100), 0), "%) Classified", sep=""),
				side=3, line=0.5, adj=1)
		}
	})

	# KEYWORDS
	output$plot_words<-renderPrint({
		 if(is.null(plot_interaction$click) & is.null(plot_interaction$hover)){cat("")
		}else{
			if(is.null(plot_interaction$click)==FALSE){
				name.tr<-names(bibliography)[click_recorder$row]
				point.tr<-which(LDA_data$coords$label==name.tr)
				topic.tr<-topics(LDA_data$model)[point.tr]
				terms.topic<-terms(LDA_data$model, 5)[, topic.tr]
				terms.article<-names(sort(dtm[click_recorder$row, ], decreasing=TRUE))[1:5]
				result<-paste("<strong>Keywords</strong><br><strong>Topic:</strong> ",
					paste(terms.topic, collapse=", "),
					"<br><strong>Article:</strong> ", 
					paste(terms.article, collapse=", "), sep="")
			}else{
				name.tr<-names(bibliography)[hover_recorder$row]
				point.tr<-which(LDA_data$coords$label==name.tr)
				topic.tr<-topics(LDA_data$model)[point.tr]
				terms.topic<-terms(LDA_data$model, 8)[4:8, topic.tr]
				terms.article<-names(sort(dtm[hover_recorder$row, ], decreasing=TRUE))[1:5]
				result<-paste("<strong>Keywords</strong><br><strong>Topic:</strong> ",
					paste(terms.topic, collapse=", "),
					"<br><strong>Article:</strong> ", 
					paste(terms.article, collapse=", "), sep="")
			}
			cat(result)
			}
		})	

	# ABSTRACTS
	output$text<-renderPrint({
		 if(is.null(plot_interaction$click) & is.null(plot_interaction$hover)){cat("")
		}else{
			if(is.null(plot_interaction$click)==FALSE){cat(plot_interaction$click)
			}else{cat(plot_interaction$hover)}
			}
		})	
	
	# EXPORT
	observeEvent(input$export_selected, {
		selector1<-which(is.na(article.selector$status)==FALSE)
		selector2<-selector1[which(article.selector$status[selector1]==TRUE)]
		result<-get.citations(bibliography[selector1])
		write.csv(result, "article_results_selected.csv", row.names=FALSE)
		})
	observeEvent(input$export_all, {
		result<-get.citations(bibliography)
		result$selected<-article.selector$status
		write.csv(result, "article_results_all.csv", row.names=FALSE)
		})

} # end server


shinyApp(ui, server) # run
} # end function

