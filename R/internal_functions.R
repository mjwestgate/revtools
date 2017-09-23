# create a summary of article ordination data (x in start_review_window)
build_topic_df<-function(
	input_info, # x or y - i.e. data.frame of articles or words
	comparison_matrix, # posterior matrix from LDA, giving words or articles. Should not = input_info
	type="x", # default is to look up words for each topic; alternative is articles 
	xdata # if type="x", then relevant info is in comparison_matrix; not so for y
	){
	topic_list<-split(input_info[, c("topic", "Axis1", "Axis2", "Axis3")], input_info$topic)
	topic_mean<-lapply(topic_list, function(a){apply(a, 2, mean)})
	topic_count<-lapply(topic_list, nrow)
	topic_df <-as.data.frame(do.call(rbind, topic_mean))
	topic_df$n<-unlist(topic_count)
	if(type=="x"){
		caption_df<-data.frame(
			id=paste0("xt", colnames(comparison_matrix)),
			topic=as.numeric(colnames(comparison_matrix)),
			caption=apply(comparison_matrix, 2, function(a){
				result<-sort(a, decreasing=TRUE)[1:5]
				paste(names(result), collapse=" ")
				}),
			stringsAsFactors=FALSE)
		topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
	}else{
		caption_df<-data.frame(
			id=paste0("yt", colnames(comparison_matrix)),
			topic=as.numeric(colnames(comparison_matrix)),
			caption=apply(comparison_matrix, 2, function(a, lookup){
				lookup$caption[which.max(a)[1]]},
				lookup= xdata),
			stringsAsFactors=FALSE)
		topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
		}
	return(topic_df)
	}


# create a summary of article ordination data (x in start_review_window)
build_topic_df_simple<-function(
	input_info,
	comparison_matrix
	){
	topic_list<-split(input_info[, c("topic", "Axis1", "Axis2", "Axis3")], input_info$topic)
	topic_mean<-lapply(topic_list, function(a){apply(a, 2, mean)})
	topic_df <-as.data.frame(do.call(rbind, topic_mean))
	caption_df<-data.frame(
		id=paste0("xt", colnames(comparison_matrix)),
		topic=as.numeric(colnames(comparison_matrix)),
		caption=apply(comparison_matrix, 2, function(a){
			result<-sort(a, decreasing=TRUE)[1:5]
			paste(names(result), collapse=" ")
			}),
		stringsAsFactors=FALSE)
	topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
	topic_df$x_count<-unlist(lapply(topic_list, nrow))
	word_count<-as.data.frame(xtabs(~apply(comparison_matrix, 1, which.max)), stringsAsFactors=FALSE)
	colnames(word_count)<-c("topic", "y_count")
	word_count$topic<-as.numeric(word_count$topic)
	topic_df <-merge(topic_df, word_count, by="topic")
	return(topic_df)
}