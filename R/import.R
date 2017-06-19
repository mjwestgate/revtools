# This script contains files for importing common bibliographic formats in to R

# New function to import a file, regardless of format
# This is based on auto-detection of key parameters
import.bib<-function(
	x, 
	path
	){

	# import x
	if(missing(path)){file<-x}else{file<-paste(path, x, sep="/")}
	z<-scan(file, sep="\t", what="character", quote="", quiet=TRUE)
	z<-unlist(strsplit(z, "\n")) # avoid problem with missed escapes
	
	# detect whether this is bib-like or ris-like
	# most efficient way is to find the most common single characters
	zsub<-z[c(1:min(c(100, length(z))))]
	if(length(grep("=\\{", zsub))>30){data.type<-"bib"}else{data.type<-"ris"}

	if(data.type=="bib"){result<-read.bib(z)  # simple case - no further work needed
	}else{  #  ris format - can be inconsistent - custom code needed

		# test for fixed width
		space.loc.list<-gregexpr(" ", zsub)
		loc.index<-data.frame(n=unlist(lapply(space.loc.list, function(a){length(a)})))
		loc.index$index<-c(1:nrow(loc.index))

		space.finder<-data.frame(
			line=do.call(c, lapply(split(loc.index, loc.index$index), function(a){rep(a$index, a$n)})),
			location=do.call(c, lapply(space.loc.list, function(a){as.numeric(a)})))
		if(any(space.finder$location==-1)){
			space.finder<-space.finder[-which(space.finder$location==-1), ]}

		space.matrix<-xtabs(~ line + location, data=space.finder)
		empty.cols<-apply(space.matrix, 2, function(a){all(a, na.rm=TRUE)}) 
		is.fixed.width<-any(empty.cols)
	
		if(is.fixed.width){
			z.dframe<-data.frame(
				ris=substr(z, 1, 2) ,
				text=substr(z, max(which(empty.cols==TRUE)), nchar(z)),
				row.order=c(1:length(z)),
				stringsAsFactors=FALSE)	
		}else{
			# find rows where the first two letters are capitalized
			tag.detector<-gregexpr("^[[:upper:]]+$", lapply(z, function(a){substr(a, 1, 2)}))
			tagged.rows<-unlist(lapply(tag.detector, function(a){a[1]==1}))
			tagged.data<-data.frame(tagged=tagged.rows, data=z, stringsAsFactors=FALSE)
			z.split<-split(tagged.data, c(1:length(z)))
			z.split<-lapply(z.split, function(a){
				if(a$tagged){regmatches(a$data, regexpr(" - ", a$data), invert=TRUE)[[1]]
				}else{c("", a$data)}})
			z.dframe<-as.data.frame(do.call(rbind, z.split), stringsAsFactors=FALSE)
			colnames(z.dframe)<-c("ris", "text")
			}

		# remove any leading or trailing white space
		z.dframe$ris<-gsub("^\\s+|\\s+$", "", z.dframe$ris)
		z.dframe$text <-gsub("^\\s+|\\s+$", "", z.dframe$text)

		# now fill missing tags
		if(any(z.dframe$ris=="")){
			missing.rows<-which(z.dframe$ris=="")
			for(i in 1:length(missing.rows)){
				z.dframe$ris[missing.rows[i]]<-z.dframe$ris[(missing.rows[i]-1)]}}
		z.dframe$row.order<-c(1:nrow(z.dframe))

		# run scripts
		result<-read.ris(z.dframe)
	}
	return(result)
}



# RIS
read.ris<-function(x){

	# merge data with lookup info, to provide bib-style tags
	lookup<-data.frame(ris=c("TY", "AU", "PY", "TI", "T1", 
			"JO", "T2", "SO", # journal
			"VL", "IS", 
			"EP", "BP", "SP", # pages
			"AB", 
			"KW", "DE", # "ID", # keywords
			"DO", "CN", 
			"SN", "UR", "AN", "CY", "PB", 
			"PP", "AD", "ED", "ET", "LA"), 
		bib=c("type", "author", "year", rep("title", 2,),
			rep("journal", 3),
			"volume", "number", 
			rep("pages", 3),
			"abstract", 
			rep("keywords", 2), 
			"doi", "call",
			"issn", "url", "accession", "institution", "publisher",
			"pubplace", "address", "editor", "edition", "language"),
		order=c(1, 2, 3, 4, 4, 5, 5, 5, 6, 7, 8, 8, 8, 9, 10, 10, 11:22),
		stringsAsFactors=FALSE)
	x.merge<-merge(x, lookup, by="ris", all.x=TRUE, all.y=FALSE)
	x.merge <-x.merge[order(x.merge$row.order), ]

	# add a column that groups rows by their reference
	x.merge$ref<-cumsum(x.merge$ris=="ER")
	x.merge<-x.merge[which(is.na(x.merge$bib)==FALSE), ]
	# may need some extra code here to remove missing rows

	# convert into a list, where each reference is a separate entry
	x.split<-split(select(x.merge, bib, text, order), x.merge$ref)
	x.final<-lapply(x.split, function(a){
		result<-split(a$text, a$bib)
		if(length(result$title)>1){
			if(result$title[1]==result$title[2]){result$title<-result$title[1]
			}else{result$title<-paste(result$title, collapse=" ")}}
		if(any(names(result)=="keywords")){
			if(length(result$keywords)>1){result$keywords<-paste(result$keywords, collapse=" ")}
			result$keywords<-strsplit(result$keywords, "; ")[[1]]}
		if(length(result$abstract>1)){
			result$abstract <-paste(result$abstract, collapse=" ")}
		if(any(names(result)=="pages")){
			if(length(result$pages)>1){result$pages<-paste(sort(result$pages), sep="-")}}
		entry.order<-unlist(lapply(names(result), function(b, order){
				order$order[which(order$bib==b)[1]]}, order=a))
		result[order(entry.order)]
		})

# lapply(x.final, function(a){a$pages})


	# generate unique label for entries
	author.year.test<-all(unlist(lapply(x.final, function(a){length(a$author)}))>0 &
		unlist(lapply(x.final, function(a){length(a$year)}))>0)
	if(author.year.test){
		ref.labels<-unlist(lapply(x.final, function(a){paste(
			strsplit(a$author[1], ",")[[1]][1], a$year, sep="")}))
		if(anyDuplicated(ref.labels)){
			nonUniqueVals<-ref.labels[which(duplicated(ref.labels))]
			for(i in 1:length(nonUniqueVals)){
				rows<-which(ref.labels== nonUniqueVals[i])
				ref.labels[rows]<-paste(ref.labels[rows], letters[c(1:length(rows))], sep="")}
			}	
	}else{ref.labels<-paste("ref", c(1:length(x.final)), sep="")}
	ref.labels<-gsub(" ", "", ref.labels)

	names(x.final)<-ref.labels

	# final stuff
	class(x.final)<-"bibdata" #  change class
	return(x.final)
	}



# BIB
read.bib<-function(x){

	# convert to data.frame with tags and content in separate columns
	x.split<-strsplit(x, "=\\{")
	x.split<-lapply(x.split, function(a){
		if(length(a==1) & nchar(a[1])>8){
			if(substr(a[1], 2, 8)=="ARTICLE"){
				result<-c("@ARTICLE", substr(a, 10, nchar(a)))
				return(result)
			}else{return(a)}
		}else{return(a)}
		})
	x.split<-lapply(x.split, function(a){gsub("=|\\}|\\,[^\\,]*$", "", a)})

	x.new<-as.data.frame(do.call(rbind, x.split), stringsAsFactors=FALSE)
	colnames(x.new)<-c("tag", "text")

	# clean
	x.new<-x.new[which(x.new$text!=""), ]
	x.new$ref<-cumsum(x.new$tag=="@ARTICLE")

	# get labels
	ref.labels<-x.new$text[which(x.new$tag=="@ARTICLE")]
	if(anyDuplicated(ref.labels)){
		nonUniqueVals<-ref.labels[which(duplicated(ref.labels))]
		for(i in 1:length(nonUniqueVals)){
			rows<-which(ref.labels== nonUniqueVals[i])
			ref.labels[rows]<-paste(ref.labels[rows], letters[c(1:length(rows))], sep="")}
		}	
	
	# remove label rows, convert to list
	x.new<-x.new[which(x.new$tag!="@ARTICLE"), ]	
	x.split<-split(x.new[, 1:2], x.new$ref)
	x.split<-lapply(x.split, function(a){
		b<-as.list(a$text)
		names(b)<-a$tag
		if(any(names(b)=="author")){b$author<-strsplit(b$author, " and ")[[1]]}
		return(b)})
	names(x.split)<-ref.labels

	# final stuff
	class(x.split)<-"bibdata" #  change class
	return(x.split)
	
	}
