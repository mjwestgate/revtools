
# function to call 
prep.bib<-function(x, 
	stop.words,
	show.coauthors=TRUE
	){
	if(missing(stop.words)){stop.words<-stopwords("english")
	}else{stop.words<-unique(c(stopwords("english"), stop.words))}
	abstracts<-extract.abstracts(x)
	# remove empty entries
	if(any(is.na(abstracts))){
		rm.rows<-which(is.na(abstracts))
		abstracts<-abstracts[-rm.rows]
		x<-x[-rm.rows]}
	cat("Creating document term matrix ... ")
	dtm<-as.matrix(get.dtm(abstracts, stop.words)) # Document Term Matrix
	# remove empty rows
	keep.rows<-which(apply(dtm , 1, sum)>0)
	dtm<-dtm[keep.rows, ]
	x<-x[keep.rows]
	# remove identical articles
	dtm.dist<-dist(dtm, method="binary")
	dtm.dist.long<-make.long.format(as.matrix(dtm.dist))
	if(any(dtm.dist.long$value==0)){
		rm.rows <-as.numeric(dtm.dist.long[which(dtm.dist.long$value==0), 2])
		abstracts<-abstracts[-rm.rows]
		x<-x[-rm.rows]
		dtm<-dtm[-rm.rows, ]}
	if(show.coauthors & length(x)<=300){
		cat("Creating document author matrix ... ")
		dam<-get.author.matrix(x)
		cat("Creating coauthor network ... ")
		coauthor.network<-get.coauthorship(dam)
		cat("Complete")
	}else{
		coauthor.network<-data.frame(a1=NA, a2=NA)
		cat("Skipping coauthorship due to data length ... Complete")
		}
	return(list(bibliography=x, dtm= dtm, coauthors= coauthor.network))
	}


# function to make a square matrix from a data.frame
make.wide.format<-function(
	input	 # result from spaa()
	){
	if(class(input)!="data.frame"){stop("make.wide.format only works for class(input)=='data.frame'")}
	# work out properties of the input
	spp.names<-unique(c(input[, 1], input[, 2]))
	n.spp<-length(spp.names)
	if(nrow(input)==choose(n.spp, 2)){asymmetric<-FALSE}else{asymmetric<-TRUE}
	# create a matrix
	result<-matrix(data=NA, nrow= n.spp, ncol= n.spp)
	colnames(result)<-spp.names
	rownames(result)<-spp.names
	# fill with a loop
	for(i in 1:nrow(input)){
		sp1<-input[i, 1]
		sp2<-input[i, 2]
		row.i<-which(spp.names==sp2)
		col.i<-which(spp.names==sp1)
		if(asymmetric){
			result[row.i, col.i]<-input[i, 3]
		}else{
			result[row.i, col.i]<-input[i, 3]
			result[col.i, row.i]<-input[i, 3]}
	}
	rownames(result)<-spp.names
	colnames(result)<-spp.names
	return(result)
	}


# function to make a 3-column data.frame from a square matrix (i.e. inverse of make.wide.format)
make.long.format<-function(input){
	if(class(input)!="matrix"){stop("make.wide.format only works for class(input)=='matrix'")}
	# get basic summaries
	asymmetric<-any(c(input==t(input))==FALSE, na.rm=TRUE)
	if(length(colnames(input))==0){spp.names<-paste("V", c(1:ncol(input)), sep="")
		}else{spp.names<-colnames(input)}
	n.spp<-ncol(input)
	# generate an appropriately-sized data.frame for the matrix in question, fill with data	
	if(asymmetric){
		line.list<-rbind(t(combn(spp.names, 2)), t(combn(spp.names, 2))[, c(2, 1)],
			matrix(rep(spp.names, each=2), nrow= n.spp, ncol=2, byrow=TRUE))
		order.list<-rbind(
			t(combn(c(1: n.spp), 2)), 
			t(combn(c(1: n.spp), 2))[, c(2, 1)],
			matrix(rep(c(1: n.spp), each=2), nrow= n.spp, ncol=2, byrow=TRUE))
		line.list<-as.data.frame(line.list[order(order.list[, 1], order.list[, 2]), ], stringsAsFactors=FALSE)
		line.list$value<-as.numeric(input)
	}else{
		line.list<-data.frame(t(combn(spp.names, 2)), stringsAsFactors=FALSE)
		line.list$value<-as.numeric(as.dist(input))}
	# clean results
	colnames(line.list)[1:2]<-c("sp1", "sp2") # good colnames
	line.list<-line.list[which(c(line.list$sp1!=line.list$sp2)), ] # remove diagonals
	line.list<-line.list[order(line.list$sp1, line.list$sp2), ] # consistent order
	return(line.list) # export
	}


# function to take an input (preferably in long format) and return a sensible distance matrix
# this is taken from circleplot, and includes some extra unnecessary stuff for our purposes (e.g. outputs a list).
make.dist.format<-function(input){
	# get objects
	if(any(c("matrix", "data.frame")==class(input))==FALSE){
		stop("make.dist.format only accepts class matrix or data.frame")}
	if(class(input)=="matrix"){
		wide<-input
		long<-make.long.format(input)}
	if(class(input)=="data.frame"){
		wide<-make.wide.format(input)
		long<-input}
	# remove infinite values
	if(any(long[, 3]==Inf, na.rm=TRUE)){
		replace.locs<-which(long[, 3]==Inf)
		replace.vals<-max(long[-replace.locs, 3], na.rm=TRUE)*2
		long[replace.locs, 3]<-replace.vals}
	if(any(input[, 3]==-Inf, na.rm=TRUE)){
		replace.locs<-which(long[, 3]==-Inf)
		replace.vals<-min(long[-replace.locs, 3], na.rm=TRUE)
		if(replace.vals<0){replace.vals<-replace.vals*2}else{replace.vals<-replace.vals*0.5}
		long[replace.locs, 3]<-replace.vals}
	# make +ve definite
	if(min(long[, 3], na.rm=TRUE)<0){
		long[, 3]<-long[, 3]-min(long[, 3], na.rm=TRUE)}
	# invert to make into a distance
	long[, 3]<-max(long[, 3], na.rm=TRUE)-long[, 3]
	# convert to matrix, check for asymmetry
	asymmetric<-all(wide==t(wide), na.rm=TRUE)==FALSE
	if(asymmetric){
		wide.array<-array(data=NA, dim=c(dim(wide), 2))
		wide.array[,,1]<-wide
		wide.array[,,2]<-t(wide)
		wide.array<-apply(wide.array, c(1, 2), sum)
		colnames(wide.array)<-colnames(wide)
		rownames(wide.array)<-rownames(wide)
		result<-as.dist(wide.array)
	}else{
		result<-as.dist(wide)}
	# set na values to the mean (i.e. no effect on clustering)
	if(any(is.na(result))){
		result[which(is.na(result))]<-mean(result, na.rm=TRUE)}
	return(list(asymmetric= asymmetric, dist.matrix=result))
	}

