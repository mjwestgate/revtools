# This script contains functions to compare authors between articles


# create a more complex version of the above that seeks to retain as much information as possible
	# while looking for typos etc.
get.author.matrix<-function(x){ # where x is from an import function; e.g. read.ciw
	y<-lapply(x, function(a){a$AU})
	y.lc<-lapply(y, function(a){a<-tolower(gsub('[[:punct:]]','', a))})
	name.vec<-sort(unique(unlist(y.lc)))
	lookup.list<-lapply(y.lc, function(a, lookup){
		names.tr<-unlist(a)
		sapply(lookup, function(b, tr){if(any(tr==b)){1}else{0}}, tr=names.tr)
		}, lookup=name.vec)
	author.matrix<-do.call(rbind, lookup.list)

	result1<-remove.duplicate.authors(author.matrix)
	result2<-remove.duplicate.authors(result1) # repeat as earlier authors might be mis-characterised
	return(as.matrix(result2))
	}


# function to clean author.matrix
remove.duplicate.authors<-function(x){ # x=author.matrix

	# look for naming errors
	name.vec<-colnames(x)
	last.names<-unlist(lapply(strsplit(name.vec, " "), function(a){a[[1]]}))
	# not.last.names<-unlist(lapply(strsplit(name.vec, " "), function(a){paste(a[-1], collapse=" ")}))

	# look for duplicated last names, find their locations in the matrix
	name.xtab<-xtabs(rep(1, length(last.names)) ~ last.names)
	duplicate.names<-names(name.xtab)[which(name.xtab>1)]

	if(length(duplicate.names)<1){
		print("No duplicates found")
		return(x)
	}else{

		# find column locations of duplicate names
		lookup.list<-lapply(duplicate.names, function(a, lookup){
			which(lookup==a)
			}, lookup=last.names)
		names(lookup.list)<-duplicate.names
	
		# for each possible duplicate, find data with and without those names
		sub.lists<-lapply(lookup.list, function(a, data){
			list(check=data[, a], other=data[, -a])}, data=x)
		names(sub.lists)<-duplicate.names

		# get a subset of data with no duplicates
		no.duplicates<-x[, -as.numeric(unlist(lookup.list))]

		# now investigate each set of duplicates in turn, return a 'cleaned' set of data with no duplicates
		cleaned.data<-lapply(sub.lists, function(a){

		# for(i in 1:length(sub.lists)){
		#	a<-sub.lists[[i]]

			# get raw data for investigation of naming (given that we already know last names match)
			names.tr<-lapply(strsplit(colnames(a[[1]]), " "), function(b){b[-1]})
			names.combn<-t(combn(c(1:length(names.tr)), 2))

			# NAME ANALYSIS
			# look at initials
			initials<-lapply(names.tr, function(b){substr(b, 1, 1)})
			initials.results<-t(apply(names.combn, 1, function(b, lookup){
				i1<-lookup[[b[1]]]
				i2<-lookup[[b[2]]]
				initial.length<-c(length(i1), length(i2))
				maxval<-min(initial.length)
				outvals<-rep(NA, maxval)
				for(i in 1:maxval){outvals[i]<-c(i1[i]==i2[i])}
				c(initials.match=all(outvals), initials.n= maxval)
				}, lookup=initials))
			
			# repeat above for any full names in the dataset
			word.results<-t(apply(names.combn, 1, function(b, lookup){
				i1<-lookup[[b[1]]]
				i2<-lookup[[b[2]]]
				if(max(nchar(i1))>1 & max(nchar(i2))>1){
					n1<-length(which(nchar(i1)>1))
					n2<-length(which(nchar(i1)>1))
					maxval<-min(c(n1, n2))
					outvals<-rep(NA, maxval)
					for(i in 1:maxval){outvals[i]<-c(i1[i]==i2[i])}
					c(words.match=all(outvals), words.n=maxval)
				}else{c(words.match=NA, words.n=0)}
				}, lookup=names.tr))
		
			text.results<-as.data.frame(cbind(names.combn, initials.results, word.results))
			
			# ARTICLE ANALYSIS
			# now get number of shared papers and authors between these entries
			rows<-which(apply(a[[1]], 1, sum)>0)
			if(length(rows)==1){
				text.results$papers <-1 # this is proof authors are different
				text.results$authors<-1 # this is arbitrary
			}else{
				cols<-which(apply(a[[2]][rows, ], 2, sum)>0)
				matrix.tr<-a[[2]][rows, cols]

				# now use names.combn to check for overlap
				split.combn<-split(names.combn, c(1:nrow(names.combn)))
				article.list<-lapply(split.combn, function(b, check, data){
					row.list<-lapply(list(row1=check[, b[1]], row2=check[, b[2]]), function(d){as.numeric(which(d>0))})
					data.list<-lapply(row.list, function(d, ds){
						if(length(d)>1){apply(ds[d, ], 2, sum)}else{ds[d, ]}}, ds=data)
					data.matrix<-do.call(rbind, data.list)
					col.selector<-which(apply(data.matrix, 2, sum)>0)
					data.matrix<-data.matrix[, col.selector]
					if(length(col.selector)>1){
						prop.shared.authors<-(1/ncol(data.matrix)) * length(which(apply(data.matrix, 2, sum)>1))
					}else{
						if(all(data.matrix>0)){prop.shared.authors<-1}else{prop.shared.authors<-0}}
					n.shared<-length(which(apply(check[, b], 1, sum)>1))
					n.total<-length(which(apply(check[, b], 1, sum)>0))
					prop.shared.papers<-(1/n.total) * n.shared
					return(c(authors=prop.shared.authors, papers=prop.shared.papers))
					}, check=a[[1]], data=a[[2]])
				text.results<-cbind(text.results, do.call(rbind, article.list))
			}	
	
			# use this to calculate the probability that all authors are the same
			text.results$qty.shared<-apply(text.results, 1, function(a){
				initials<-prod(a[3:4])
				if(is.na(a[5])){stop.words<-1}else{stop.words<-a[5]}
				words<-prod(c(a[6], stop.words))
				if(a[3]==0 | stop.words == 0 | a[8]>0){def.stop<-TRUE}else{def.stop<-FALSE}
				if(def.stop){0}else{sum(initials, words, a[7])}
				})
			
			# make adjacency matrix, find groups
			author.dist<-make.dist.format(text.results[, c(1, 2, ncol(text.results))])$dist.matrix
			max.val<-max(author.dist)+1
			group.dframe<-data.frame(
				label=colnames(a[[1]]),
				group=cutree(hclust(max.val-author.dist), h=max(author.dist)),
				column=c(1:ncol(a[[1]])),
				stringsAsFactors=FALSE)
			group.list<-split(group.dframe, group.dframe$group)

			# use this list to amalgamate data
			result.list<-lapply(group.list, function(b, data){
				if(nrow(b)==1){
					result<-data.frame(x=data[, b$col])
					colnames(result)<-b$label
				}else{
					result<-data.frame(x=apply(data[, b$col], 1, max))
					colnames(result)<-b$label[which.max(nchar(b$label))]}
				return(result)
				}, data=a[[1]])

			return(do.call(cbind, result.list))

		}) # end lapply
	names(cleaned.data)<-NULL
	merged.data<-do.call(cbind, cleaned.data)
	final.data<-cbind(no.duplicates, merged.data)
	final.data<-final.data[, order(colnames(final.data))]
	cat(paste(" <Testing:", ncol(x)-ncol(final.data), "names amalgamated> ", sep=" "))
	return(final.data)
	} # end if(length(duplicate.names) >=1)
	
}	# end function



# function to find shared papers between authors
get.coauthorship<-function(x){
	x.split<-split(as.data.frame(x), c(1:nrow(x)))
	lookup.list<-lapply(x.split,  function(a, alldata){
		cols<-which(a>0)
		if(length(cols)>1){
			lookup<-apply(alldata[, cols], 1, sum)
			which(lookup>0)
		}else{which(alldata[, cols]>0)}
		}, alldata =x)
	for(i in 1:length(lookup.list)){
		lookup.list[[i]]<-lookup.list[[i]][-which(lookup.list[[i]]==i)]}
	nrow.list<-lapply(lookup.list, function(a){length(a)})
	for(i in 1:length(nrow.list)){nrow.list[[i]]<-rep(rownames(x)[i], nrow.list[[i]])}
	result.vector<-unlist(lapply(lookup.list, function(a){names(a)}))
	result.dframe<-data.frame(
		a1=unlist(nrow.list), a2=result.vector, stringsAsFactors=FALSE)
	return(result.dframe)
	}


