# create a summary of article ordination data (x in start_review_window)
build_topic_df<-function(
	input_info, # x or y - i.e. data.frame of articles or words
	comparison_matrix, # posterior matrix from LDA, giving words or articles. Should not = input_info
	type="x", # default is to look up words for each topic; alternative is articles 
	xdata # if type="x", then relevant info is in comparison_matrix; not so for y
	){
	topic_list<-lapply(split(input_info[, c("topic", "Axis1", "Axis2", "Axis3")], input_info$topic), 
		function(a){apply(a, 2, mean)})
	topic_df <-as.data.frame(do.call(rbind, topic_list))
	if(type=="x"){
		caption_df<-data.frame(
			topic=as.numeric(colnames(comparison_matrix)),
			caption=apply(comparison_matrix, 2, function(a){
				result<-sort(a, decreasing=TRUE)[1:5]
				paste(names(result), collapse=" ")
				}),
			stringsAsFactors=FALSE)
		topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
	}else{
		caption_df<-data.frame(
			topic=as.numeric(colnames(comparison_matrix)),
			caption=apply(comparison_matrix, 2, function(a, lookup){
				lookup$caption[which.max(a)[1]]},
				lookup= xdata),
			stringsAsFactors=FALSE)
		topic_df<-merge(topic_df, caption_df, by="topic", all=FALSE)
		}
	return(topic_df)
	}

# function to set plot color without showing a key
add_color<-function(variable, palette){palette[variable]}