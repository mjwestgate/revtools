# function to export data in .bib format
write.bib<-function(x, filename="bibviewr_export.bib"){

	# process basic text
	result<-lapply(x, function(a){
		if(any(names(a)=="author")){a$author<-paste(a$author, collapse=" and ")}
		a<-lapply(a, function(b){ 	# ensure only one entry per value
			if(length(b)>1){paste(b, collapse="; ")
			}else{b}})
		paste(names(a), "={", a, "},", sep="") # format as text
		})

	# add article identifier info
	export<-unlist(lapply(c(1:length(result)), function(a, source, entry.names){
		c(paste("@ARTICLE{", entry.names[a], ",", sep=""),
			source[a], "}", "")
		}, source=result, entry.names=names(x))
		)
	names(export)<-NULL
	
	# export
	write.table(export, filename, quote=FALSE, 
		row.names=FALSE, col.names=FALSE)
	}