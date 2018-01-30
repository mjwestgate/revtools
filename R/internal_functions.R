# create a summary of article ordination data (x in start_review_window)
build_topic_df_simple<-function(
	input_info,
	comparison_matrix,
	dtm,
	term_matrix
	){
	# get coordinates
	topic_list<-split(input_info[, c("topic", "Axis1", "Axis2", "Axis3")], input_info$topic)
	topic_mean<-lapply(topic_list, function(a){apply(a, 2, mean)})
	topic_df <-as.data.frame(do.call(rbind, topic_mean))
	# add terms
	topic_df$id<-paste0("xt", colnames(comparison_matrix))
	topic_df$caption<-apply(term_matrix, 2, function(a){paste(a, collapse=", ")})
	comp_scaled <-(comparison_matrix / apply(comparison_matrix, 1, sum))
	terms_scaled<-apply(comp_scaled, 2, function(a){names(sort(a, decreasing=TRUE))[1:5]})
	topic_df$caption_weighted<-apply(terms_scaled, 2, function(a){paste(a, collapse=", ")})	
	# add sums
	topic_df$x_count<-unlist(lapply(topic_list, nrow))
	word_count<-as.data.frame(xtabs(~apply(comparison_matrix, 1, which.max)), stringsAsFactors=FALSE)
	topic_df$y_count<-word_count$Freq
	return(topic_df)
}	

# data to send to plotinfo
build_plot_data<-function(info, model, dtm, x_keep){
	x_matrix<-modeltools::posterior(model)$topics # article x topic
	y_matrix<-t(modeltools::posterior(model)$terms)
	plot_list<-list(
		x=data.frame(
			id=rownames(dtm),
			label= info$label[x_keep],
			ade4::dudi.coa(x_matrix, scannf=FALSE, nf=3)$li,
			topic= apply(x_matrix, 1, which.max),
			weight= apply(x_matrix, 1, max),
			caption=apply(info[x_keep, ], 1, format_citation_dataframe),
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
	plot_list$topic<-build_topic_df_simple(plot_list$x, y_matrix, dtm, topicmodels::get_terms(model, 5))	
	return(plot_list)
	}


# function to build infostore. Note that this can only be done once, whereas build_plot_data can be updated
# generate info to pass to infostore: it updates the display, but not the whole plot
# further, this information remains of constant length as go_LDA is run
build_infostore<-function(plot_list, dtm, palette){
result<-reactiveValues(
	x=data.frame(
		id=rownames(dtm),
		label= plot_list$x$label,
		tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
		topic_counter=0,
		decision_time="",
		topic=plot_list$x$topic,
		color= palette[plot_list$x$topic],
		stringsAsFactors=FALSE),
	y=data.frame(
		id=plot_list$y$id,
		label=colnames(dtm),
		frequency=apply(dtm, 2, sum),
		tested=FALSE, selected=FALSE, display=TRUE, present=TRUE,
		topic_counter=0,
		decision_time="",
		topic=plot_list$y$topic,
		color= palette[plot_list$y$topic],
		stringsAsFactors=FALSE),
	topic=data.frame(
		id= plot_list$topic$id,
		topic= plot_list$topic$topic,
		tested=FALSE, selected=FALSE, display=TRUE,
		x_count=plot_list$topic$x_count,
		y_count=plot_list$topic$y_count,
		color= palette[plot_list$topic$topic],
		stringsAsFactors=FALSE)
)
return(result)
}

# update infostore when go_LDA is run
update_infostore<-function(infostore, palette, plotinfo){
	col_order<-c("id", "label", "tested", "selected", "display", "present",
		"topic_counter", "decision_time", "topic", "color")
	update_x<-infostore$x[infostore$x$present, ]
	update_x$topic<-plotinfo$x$topic
	update_x<-update_x[, col_order]
	update_x$color<-palette[update_x$topic]
	update_x <-as.data.frame(rbind(update_x,
		infostore$x[which(infostore$x$present==FALSE), ]))
	# ditto for y
	col_order<-c(col_order[c(1, 2)], "frequency", col_order[c(3:10)])
	update_y<-infostore$y[infostore$y$present, ]	
	update_y$topic<-plotinfo$y$topic
	update_y<-update_y[, col_order]
	update_y$color<-palette[update_y$topic]
	update_y <-as.data.frame(rbind(update_y,
		infostore$y[which(infostore$y$present==FALSE), ]))
	# and topics
	update_topic<-data.frame(
		id= plotinfo$topic$id,
		topic= plotinfo$topic$topic,
		tested=FALSE, selected=FALSE, display=TRUE,
		x_count= plotinfo$topic$x_count,
		y_count= plotinfo$topic$y_count,
		color= palette[plotinfo$topic$topic],
		stringsAsFactors=FALSE)
	output<-list(x=update_x, y=update_y, topic=update_topic)
	return(output)
}