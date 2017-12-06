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