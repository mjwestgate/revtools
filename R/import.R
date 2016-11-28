# This script contains files for importing common bibliographic formats in to R


# wrapper function to get all references, regardless of format
import.bib<-function(x){ # e.g. x="reference.csw"
	x.format<-strsplit(x, "\\.")[[1]][2]
	if(any(c("ciw", "ris", "bib")==x.format)==FALSE){
		stop(paste("No method currently exists in package(bibviewr) to import files of format '.", 
			x.format, "'", sep=""))}
	result<-switch(x.format,
		"ciw"={read.ciw(x)},
		"ris"={read.ris(x)},
		"bib"={read.bib(x)})
	class(result)<-"bibdata"
	return(result)
	}


# CIW format - default option for ISI web of knowledge
# Note: This was the first import function built for this package and all later options have this format
	# i.e. output a list of options with .ris -style colnames.
read.ciw<-function(x){

	# import x
	z<-scan(x, sep="\t", what="character", quote="", quiet=TRUE)
	z<-unlist(strsplit(z, "\n")) # avoid problem with missed escapes
	
	# extract designator info
	y<-lapply(strsplit(z, " "), function(a){
		if(length(a)==0){return(c("SPLIT", ""))}
		label<-a[1]
		text<-a[-1]
		if(any(text=="")){text<-text[-which(text=="")]}
		text<-paste(text, collapse=" ")
		return(c(label, text))})
	y <-as.data.frame(do.call(rbind, y), stringsAsFactors=FALSE)
	colnames(y)<-c("label", "value")
	
	# remove 'header' info
	if(any(y$label=="FN")){y<-y[-which(y$label=="FN"), ]}
	if(any(y$label=="VR")){y<-y[-which(y$label=="VR"), ]}
	
	# fill in gaps in y$label
	for(i in 2:nrow(y)){if(y$label[i]==""){y$label[i]<-y$label[(i-1)]}}
	
	# get data to allow splitting by blank lines
	y$split<-0; y$split[1]<-1
	y$split[which(y$label=="PT")]<-1
	y$entry<-cumsum(y$split)
	y$entry<-paste("v", y$entry, sep="")
	y<-y[-which(y$value==""), ]
	
	# split
	result<-split(y[, 1:2], y$entry)
	result.clean<-lapply(result, function(a){
		vals<-unique(a$label)
		split.fac<-factor(
			sapply(a$label, function(b, lookup){which(lookup==b)}, lookup=vals),
			levels=c(1:length(vals)), labels=vals)
		output<-split(a$value, split.fac)

		# clean any entries that occur over multiple lines
		if(any(names(output)=="AB")){
			if(length(output$AB)>1){output$AB<-paste(output$AB, collapse=" ")}}
		if(any(names(output)=="TI")){
			if(length(output$TI)>1){output$TI<-paste(output$TI, collapse=" ")}}
		if(any(names(output)=="SO")){
			if(length(output$SO)>1){output$SO<-paste(output$SO, collapse=" ")}}
		return(output)
		})

return(result.clean)
}


# RIS
# x<-"scopus.ris"
read.ris<-function(x){

	# import x
	z<-scan(x, sep="\t", what="character", quote="", quiet=TRUE)
	z<-unlist(strsplit(z, "\n")) # avoid problem with missed escapes
	
	# get labels
	row.labels<-substr(z, 1, 2) 
	row.labels[which(row.labels=="T2")]<-"SO"
	# remove empty rows
	keep.rows<-which(row.labels!="")
	row.labels<-row.labels[keep.rows]
	z<-z[keep.rows]

	# get info
	y<-unlist(lapply(strsplit(z, "- "), function(a){
		text<-a[-1]
		if(length(text)==0){return("")
		}else{
			# if(any(text=="")){text<-text[-which(text=="")]}
			if(length(text)>1){text<-paste(text, collapse=" ")}
			return(text)}
		}))

	# join labels and text
	y<-data.frame(label=row.labels, value=y, stringsAsFactors=FALSE)
	
	# get data to allow splitting by blank lines
	y$split<-0; y$split[1]<-1
	y$split[which(y$label=="TY")]<-1
	y$entry<-cumsum(y$split)
	y$entry<-paste("v", y$entry, sep="")

	# split
	result<-split(y[, 1:2], y$entry)
	result.clean<-lapply(result, function(a){
		vals<-unique(a$label)
		split.fac<-factor(
			sapply(a$label, function(b, lookup){which(lookup==b)}, lookup=vals),
			levels=c(1:length(vals)), labels=vals)
		output<-split(a$value, split.fac)

		# clean any entries that occur over multiple lines
		if(any(names(output)=="AB")){
			if(length(output$AB)>1){output$AB<-paste(output$AB, collapse=" ")}}
		if(any(names(output)=="TI")){
			if(length(output$TI)>1){output$TI<-paste(output$TI, collapse=" ")}}
		if(any(names(output)=="SO")){
			if(length(output$SO)>1){output$SO<-paste(output$SO, collapse=" ")}}
		return(output)
		})

return(result.clean)
}
# test<-read.ris("scopus_wAbstracts.ris")
# any(unlist(lapply(test, function(a){any(names(a)=="AB")}))) # check for abstracts


# BIB (xml?)
read.bib<-function(x){

	# import x
	z<-scan(x, sep="\t", what="character", quote="", quiet=TRUE)
	z<-unlist(strsplit(z, "\n")) # avoid problem with missed escapes

	# split by entry - in this case by finding the @ARTICLE tag	
	at.vector<-rep(0, length(z))
	first.char.vector<-lapply(as.list(z), function(a){substr(a, 1, 1)})
	at.vector[which(unlist(first.char.vector) =="@")]<-1
	split.vector<-cumsum(at.vector)
	z.split<-split(z, split.vector)

	# get names for each unique entry
	z.names<-unlist(lapply(z.split, function(a){substr(a[1], 10, nchar(a[1])-1)}))
	names(z.split)<-z.names

	# generate a lookup table
	lookup.table<-data.frame(
		bib=c("author", "title", "journal", "year", "volume", "issue", "number", "page_initial", "page_final", 
			"doi", "note", "url", "document_type", "source", "abstract"),
		ris=c("AU", "TI", "SO", "PY", "VL", "IS", "IS", "SP", "EP", "DO", "N1", "UR", "TY", "DB", "AB"),
		stringsAsFactors=FALSE)

	# sort out contents of each entry
	z.final<-lapply(z.split, function(a, check){
		#extract and reorganize data
		a<-a[c(2:(length(a)-1))]
		split.entries<-lapply(as.list(a), function(b){strsplit(b, "=\\{")})
		name.headings<-unlist(lapply(split.entries, function(a){a[[1]][1]}))
		entry.data<-unlist(lapply(split.entries, function(a){substr(a[[1]][2], 1, nchar(a[[1]][2])-2)}))
		result<-as.list(entry.data)
		names(result)<-name.headings

		# set format for authors
		if(any(names(result)=="author")==FALSE){result$author<-"Anon."}
		result$author<-strsplit(result$author, " and ")[[1]] # appears robust to n.authors = 1

		# split into start and end pages
		if(any(names(result)=="pages")){
			pages<-strsplit(result$pages, "-")[[1]]
			result<-result[-which(names(result)=="pages")]
			result$page_initial<-pages[1]
			result$page_final<-pages[2]
		}else{
			result$page_initial<-NA
			result$page_final<-NA}

		# ensure year is numeric
		result$year<-as.numeric(result$year)		

		# use lookup table to match headings to those in other formats 
		names(result)<-unlist(lapply(names(result), function(a, lookup){
			if(any(lookup$bib==a)){
				return(lookup$ris[which(lookup$bib==a)])
			}else{return(NA)}
			}, lookup=check))
		if(any(is.na(names(result)))){result<-result[-which(is.na(names(result)))]}

		return(result)

		}, check=lookup.table)
	
	return(z.final)
	} # end function
