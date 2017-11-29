# function to export data in .bib format
write_bibliography<-function(x, filename, format="ris"){

if(missing(filename)){stop("argument 'filename' is missing, with no default")}

if(format=="bib"){
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
	
	}

if(format=="ris"){

	# create lookup table to convert tags back to ris format
	lookup<-data.frame(
		tag=c("type", "author", "year", "title", "journal",
			"volume", "number", "startpage", "endpage", "abstract", "keywords",
			"doi", "call",
			"issn", "url", "accession", "institution", "publisher",
			"pubplace", "address", "editor", "edition", "language", "end"),
		ris=c("TY", "AU", "PY", "TI", "JO",
			"VL", "IS", "SP", "EP", "AB", "KW",
			"DO", "CN", 
			"SN", "UR", "AN", "CY", "PB", 
			"PP", "AD", "ED", "ET", "LA", "ER"), 
		stringsAsFactors=FALSE)

	result<-lapply(x, function(a, lookup){

		# convert to tagged vector
		b<-do.call(c, a)
		b<-data.frame(tag=c(names(b), "end"), entry=c(b, ""), stringsAsFactors=FALSE)
		rownames(b)<-NULL
		b$tag<-gsub("[[:digit:]]", "", b$tag)

		# page information needs to be treated separately
		if(any(b$tag=="pages")){
			page.row<-which(b$tag=="pages")
			page.sep<-strsplit(b$entry[page.row], "-")[[1]]
			if(length(page.sep)>1){
				new.rows<-data.frame(tag=c("startpage", "endpage"), 
					entry=page.sep,  stringsAsFactors=FALSE)
				b<-as.data.frame(rbind(
					b[c(1:(page.row-1)), ],
					new.rows,
					b[c((page.row+1):nrow(b)), ]))
			}}	
		b$order<-c(1:nrow(b))
			
		# substitute tags for ris format versions
		b<-merge(lookup, b, by="tag", all.x=FALSE, all.y=TRUE)
		b<-b[order(b$order), 2:3]

		# concatenate rows, return a vector of strings
		c(paste(b$ris, b$entry, sep="  - "), "")
	
		}, lookup=lookup)

	export<-do.call(c, result)
	} # end ris

# export
write.table(export, filename, quote=FALSE,  row.names=FALSE, col.names=FALSE)

} #  end function