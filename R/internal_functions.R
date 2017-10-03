# create a summary of article ordination data (x in start_review_window)
build_topic_df_simple<-function(
	input_info,
	comparison_matrix,
	dtm
	){
	topic_list<-split(input_info[, c("topic", "Axis1", "Axis2", "Axis3")], input_info$topic)
	topic_mean<-lapply(topic_list, function(a){apply(a, 2, mean)})
	topic_df <-as.data.frame(do.call(rbind, topic_mean))
	# calculate relevance
	word_counts<-apply(dtm, 2, sum)
	word_prob<-log(word_counts) - log(sum(word_counts))
	lift<-log(comparison_matrix) - word_prob # rep(word_prob, each=ncol(comparison_matrix))
	lambda<-0.5 # 1 #0.6
	word_weight<- (lambda * log(comparison_matrix)) + ((1 - lambda) * lift)
	caption_df<-data.frame(
		id=paste0("xt", colnames(comparison_matrix)),
		topic=as.numeric(colnames(comparison_matrix)),
		caption="none", caption_weighted="none",
		stringsAsFactors=FALSE)
	for(i in 1:nrow(caption_df)){
		row_order<-order(comparison_matrix[ , i], decreasing=TRUE)	
		caption_df$caption[i]<-paste(rownames(comparison_matrix)[row_order][1:5], collapse=", ") 
		data_tr<-word_weight[row_order, i]
		caption_df$caption_weighted[i]<-paste(names(sort(data_tr[-c(1:5)], decreasing=TRUE)[1:5]), collapse=", ") 
		}
	topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
	topic_df$x_count<-unlist(lapply(topic_list, nrow))
	word_count<-as.data.frame(xtabs(~apply(comparison_matrix, 1, which.max)), stringsAsFactors=FALSE)
	colnames(word_count)<-c("topic", "y_count")
	word_count$topic<-as.numeric(word_count$topic)
	topic_df <-merge(topic_df, word_count, by="topic")
	return(topic_df)
}