find_duplicates<-function(x){

	if(class(x)=="bibliography"){x<-as.data.frame(x)}
	if(class(x)!="data.frame"){stop("find_duplicates only accepts objects of class 'bibliography' or 'data.frame'")}
	if(all(c("label", "title", "journal", "year") %in% colnames(x))==FALSE){
		stop("x must contain columns named 'label', 'title', 'journal' and 'year' for find_duplicates to function")}
	if(length(unique(x$label))<nrow(x)){x$label<-make.names(x$label, unique=TRUE)}

	# prep a checkable data.frame
	x.lower<-x
	x.lower$title<-gsub("[[:punct:]]", "", tolower(x.lower$title)) # remove punctuation
	x.lower$journal<-tolower(x.lower$journal)
	x.lower$year<-as.numeric(x.lower$year)

	# check for similar journals - allows fuzzy matching of journal titles
	journal_names<-sort(unique(x.lower$journal)) 
	journals_cleaned<-gsub("\\b\\w{1,3}\\s|[[:punct:]]", "", journal_names) # remove all words <=3 characters
	journals_cleaned <-gsub("  ", " ", journals_cleaned) # remove double spaces
	# split into words
	text_list<-strsplit(journals_cleaned, " ")
	word_count<-unlist(lapply(text_list, length))
	text_list<-lapply(text_list, function(a, n){
		b<-rep(NA, n)
		b[1:length(a)]<-a
		return(b)
		}, n=max(word_count))
	text_dframe<-as.data.frame(do.call(rbind, text_list), stringsAsFactors=FALSE)
	text_dframe$initial_title<-journal_names
	text_dframe$word_count<-word_count
	text_dframe<-text_dframe[order(text_dframe$word_count), ]
	text_dframe$group<-0
	text_dframe$checked<-FALSE
	
	# use while loop to investigate possible duplication
	while(all(text_dframe$checked)==FALSE){
		group_tr<-max(text_dframe$group)+1
		unchecked_rows<-which(text_dframe$checked==FALSE)
		if(length(unchecked_rows)==1){
				text_dframe$checked[unchecked_rows]<-TRUE
				text_dframe$group[unchecked_rows]<-group_tr
		}else{
			# select rows
			row_tr<-unchecked_rows[1]
			row_comparison<-unchecked_rows[-1]
			cols_tr<-c(1:min(c(5, text_dframe$word_count[row_tr])))	
			# group text for comparison
			source_text<-paste(text_dframe[row_tr, cols_tr], collapse=" ")
			if(length(cols_tr)==1){
				comparison_text<-text_dframe[row_comparison, cols_tr]
			}else{
				if(length(row_comparison)==1){
					comparison_text<-paste(text_dframe[row_comparison, cols_tr], collapse=" ")
				}else{ # >1
					comparison_text<-apply(text_dframe[row_comparison, cols_tr], 1, 
						function(a){paste(a, collapse=" ")})
					}
				}
			similarity_check<-c(comparison_text == source_text)
			# output results
			if(any(similarity_check)){
				rows_final<-c(row_tr, row_comparison[which(similarity_check)])
			}else{
				rows_final<-row_tr}
			text_dframe$checked[rows_final]<-TRUE
			text_dframe$group[rows_final]<-group_tr
		}
	} # end loop
	journal_groups<-text_dframe[, c("initial_title", "group")]
	colnames(journal_groups)<-c("journal", "journal_group")
	x.lower<-merge(x.lower, journal_groups, by="journal", all=TRUE)
	x.lower<-x.lower[order(x.lower$label), ]

	# prep for duplicate testing
	x.lower$checked<-is.na(x.lower$title) # i.e. those that are missing titles are not checked
	x.lower$duplicate_group<-NA
	group_increment<-1

	# check for similar titles within journals and years
	while(length(which(x.lower$checked==FALSE))>0){
		if(length(which(x.lower$checked==FALSE))==1){
			last_row<-which(x.lower$checked==FALSE)
			x.lower$duplicate_group[last_row]<-group_increment
			x.lower$checked[last_row]<-TRUE
		}else{
			# locate relevant information
			unchecked_test<-x.lower$checked==FALSE
			row_start<-which(unchecked_test)[1]
			# include only those rows with similar years
			if(is.na(x.lower$year[row_start])){
				year_test<-rep(TRUE, nrow(x.lower))
			}else{
				year_test<-(abs(x.lower$year - x.lower$year[row_start]) < 2 | is.na(x.lower$year))}
			# include only rows with similar journal titles
			if(is.na(x.lower$journal[row_start])){	
				journal_test<-rep(TRUE, nrow(x.lower))
			}else{
				journal_test<-((x.lower$journal_group == x.lower$journal_group[row_start] ) | is.na(x.lower$journal))}
			# combine
			comp_rows<-which(unchecked_test & year_test & journal_test)
			comp_rows <- comp_rows[which(comp_rows != row_start)]
			text_similarity <- stringdist::stringdist(x.lower$title[row_start], x.lower$title[comp_rows])
			if(any(text_similarity < 5)){
				checked_rows<-c(row_start, comp_rows[which(text_similarity < 5)])
				x.lower$duplicate_group[checked_rows]<-group_increment
				x.lower$checked[checked_rows]<-TRUE
			}else{
				x.lower$duplicate_group[row_start]<-group_increment
				x.lower$checked[row_start]<-TRUE
				}
			group_increment <- group_increment + 1
			}
		}
	x<-merge(x, x.lower[, c("label", "duplicate_group")], by="label", all=TRUE)

	# now ensure that entries that are missing titles are given a unique number
	if(any(is.na(x$title))){
		rows_tr<-which(is.na(x$title))
		x$duplicate_group[rows_tr]<-c(1:length(rows_tr)) + max(x$duplicate_group)
		}

	return(x)
}