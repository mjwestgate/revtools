# This script contains files for importing common bibliographic formats in to R


# wrapper function to get all references, regardless of format
import.bib<-function(x){ # e.g. x="reference.csw"
	x.format<-strsplit(x, "\\.")[[1]][2]
	if(any(c("ciw", "ris", "bib", "csv")==x.format)==FALSE){
		stop(paste("No method currently exists in package(bibviewr) to import files of format'", 
			x.format, "'", sep=""))}
	switch(x.format,
		"ciw"={read.ciw(x)},
		"ris"={read.ris(x)})
		#"bib"={read.bib(x)},
		#"csv"={read.csv.refs(x)})
	}


# CIW format - default option for ISI web of knowledge
# Note: This was the first import function built for this package and all later options have this format
	# i.e. output a list of options with .ris -style colnames.
read.ciw<-function(x){

	# import x
	z<-scan(x, sep="\t", what="character", quiet=TRUE)
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
	z<-scan(x, sep="\t", what="character", quiet=TRUE)
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


# TXT (scopus)

# BIB (xml?)

# csv (scopus)