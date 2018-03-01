# This script contains files for importing common bibliographic formats in to R

# New function to import a file, regardless of format
# This is based on auto-detection of key parameters
read_bibliography<-function(
	x, 
	path
	){

	# import x
	if(missing(path)){
		file<-x
	}else{
		file<-paste(path, x, sep="/")
		}
	invisible(Sys.setlocale("LC_ALL", "C")) # gets around errors in import with special characters
	z<-scan(file, sep="\t", what="character", quote="", quiet=TRUE, blank.lines.skip=FALSE)
	Encoding(z)<-"latin1"
	z<-gsub("<[[:alnum:]]{2}>", "", z) # remove errors from above process

	# detect whether file is bib-like or ris-like via the most common single characters
	nrows<-min(c(200, length(z)))
	zsub<-z[c(1: nrows)]
	# invisible(Sys.setlocale('LC_ALL', 'C'))  # duplicated line
	if(length(grep("\\{", zsub))>30){data.type<-"bib"}else{data.type<-"ris"}

	if(data.type=="bib"){result<-read_bib(z)  # simple case - no further work needed
	}else{  #  ris format can be inconsistent; custom code needed

		# detect delimiters between references, starting with strings that start with "ER"
		if(any(grepl("^ER", zsub))){delimiter<-"endrow"
		}else{
			# special break: same character repeated >6 times, no other characters
			char_list<-strsplit(zsub, "")
			char_break_test<-unlist(lapply(char_list, function(a){length(unique(a))==1 & length(a>6)}))
			if(any(char_break_test)){delimiter<-"character"
			}else{
				# use space as a ref break (last choice)
				space_break_check<-unlist(lapply(char_list, function(a){all(a=="" | a==" ")}))
				if(any(space_break_check)){delimiter<-"space"
				}else{stop("import failed: unknown reference delimiter")}
			}
		}

		# detect tags
		zlist<-as.list(z)
		zlist<-lapply(zlist, function(a){
			if(a==""){return(a)
			}else{
				caps_present<-gregexpr("^[[:upper:]]{2,4}|[[:upper:]]{1}[[:digit:]]{1}", a)[[1]]
				if(any(caps_present==1)){
					end_caps<-attr(caps_present, "match.length")[1]
					tag<-substr(a, 1, end_caps)
					content<- gsub("^\\s+|\\s+$", "", substr(a, (end_caps+1), nchar(a)))
					content<-gsub("^-|:", "", content)
					content<-gsub("^\\s+|\\s+$", "", content)
					return(c(tag, content))
				}else{return(c("", gsub("^\\s+|\\s+$", "", a)))}
			}
			})
		z.dframe<-as.data.frame(do.call(rbind, zlist), stringsAsFactors=FALSE)
		colnames(z.dframe)<-c("ris", "text")
		z.dframe$row.order<-c(1:nrow(z.dframe))

		# replace tag information for delimiter == character | space
		if(delimiter=="character"){
			z.dframe$ris[which(
				unlist(lapply(strsplit(z, ""), function(a){length(unique(a))==1 & length(a>6)}))
				)]<-"ER"}
		if(delimiter=="space"){
			z.dframe$ris[which(
				unlist(lapply(strsplit(z, ""), function(a){all(a=="" | a==" ")})))]<-"ER"
			# ensure multiple consecutive empty rows are removed
			rollingsum<-function(a, n=2L){tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)}
			z_rollsum<-rollingsum(z.dframe$ris == "ER")
			if(any(z_rollsum>1)){z.dframe<-z.dframe[which(z_rollsum <=1), ]}
			}
		if(delimiter=="endrow"){
			row_df<-data.frame(
				start=which(z.dframe$ris=="TY"),
				end=which(z.dframe$ris=="ER")
				)
			z.list<-apply(row_df, 1, function(a){c(a[1]:a[2])})
			z.list<-lapply(z.list, function(a, lookup){lookup[a, ]}, lookup=z.dframe)
			z.dframe<-as.data.frame(do.call(rbind, z.list))
		}

		# cleaning
		z.dframe$ref<-c(0, cumsum(z.dframe$ris=="ER")[c(1:(nrow(z.dframe)-1))]) # split by reference
		z.dframe<-z.dframe[which(z.dframe$text!=""), ] # remove empty rows
		z.dframe<-z.dframe[which(z.dframe$ris!="ER"), ] # remove end rows

		# fill missing tags
		z.split<-split(z.dframe, z.dframe$ref)
		z.split<-lapply(z.split, function(a){		
			if(a$ris[1]==""){a$ris[1]<-"ZZ"}
			accum_ris<-Reduce(c, a$ris, accumulate=TRUE)
			a$ris<-unlist(lapply(accum_ris, function(b){
				good_vals<-which(b!="")
				b[good_vals[length(good_vals)]]
				}))
			return(a)})
		z.dframe<-as.data.frame(do.call(rbind, z.split))

		# import appropriate format
		if(any(z.dframe$ris=="PMID")){result<-read_medline(z.dframe)
		}else{result<-read_ris(z.dframe)}

	}
	return(result)
}



read_medline<-function(x){

	names(x)[3]<-"order"
	# data from https://www.nlm.nih.gov/bsd/mms/medlineelements.html	
	lookup<-data.frame(ris=c(
		"AB", "CI", 
		"AD", "IRAD", "AID", 
		"AU", "AUID", "FAU", "BTI", "CTI", 
		"COI", "CN", "CRDT", "DCOM", "DA", 
		"LR", "DEP", "DP", "EN", "ED", "FED", 
		"EDAT", "GS", "GN", "GR", "IR", "FIR", 
		"ISBN", "IS", "IP", "TA", "JT", 
		"LA", "LID", "MID", "MHDA", "MH", 
		"JID", "RF", "OAB", "OCI", "OID", 
		"OT", "OTO", "OWN", "PG", "PS",
		"FPS", "PL", 
		"PHST", "PST", "PT", "PUBM", 
		"PMC", "PMCR", "PMID", 
		"RN", "NM", "SI", "SO", "SFM", 
		"STAT", "SB", "TI", "TT", "VI", "VTI"),   
	bib=c("abstract", "copyright_info", 
		"affiliation", "investigator_affiliation", "article_id", 
		"author", "author_id", "author_full", "book_title", "collection_title", 
		"conflict_of_interest", "author_corporate", "date_added", "date_completed", "date_created", 
		"date_revised", "date_published_elec", "date_published", "edition", "editor", "editor_full", 
		"date_added", "gene_symbol", "general_note", "grant_number", "investigator", "investigator_full", 
		"isbn", "issn", "issue", "journal_abbreviated", "journal", 
		"language", "location_id", "manuscript_id", "mesh_date", "mesh_terms", 
		"nlm_id", "references_n", "abstract_other", "copyright_info_other", "id_other", 
		"term_other", "term_owner_other", "owner", "pages", "personal_name_as_subject", 
		"personal_name_as_subject_full", "place_published", 
		"publication_history_status", "publication_status", "publication_type", "publishing_model", 
		"pubmed_central_identitfier", "pubmed_central_release", "pubmed_id", 
		"registry_number", "substance_name", "secondary_source_id", "source", "space_flight_mission", 
		"status", "subset", "title", "title_transliterated", "volume", "volume_title"),
	stringsAsFactors=FALSE)
	x.merge<-merge(x, lookup, by="ris", all.x=TRUE, all.y=FALSE)
	x.merge<-x.merge[order(x.merge$order), ]

	# convert into a list, where each reference is a separate entry
	x.split<-split(x.merge[c("bib", "text")], x.merge$ref)
	x.final<-lapply(x.split, function(a){
		result<-split(a$text, a$bib)
		if(any(names(result)=="abstract")){
			result$abstract<-paste(result$abstract, collapse=" ")}
		if(any(names(result)=="title")){
			if(length(result$title)>1){result$title<-paste(result$title, collapse=" ")}}
		if(any(names(result)=="term_other")){
			names(result)[which(names(result)=="term_other")]<-"keywords"}
		if(any(names(result)=="date_published")){
			result$year<-substr(result$date_published, 1, 4)}
		if(any(names(result)=="article_id")){
			doi_check<-grepl("doi", result$article_id)
			if(any(doi_check)){
				result$doi<-strsplit(result$article_id[which(doi_check)], " ")[[1]][1]}}
		return(result)
		})

	names(x.final)<-unlist(lapply(x.final, function(a){a$pubmed_id}))
	class(x.final)<-"bibliography"
	return(x.final)
}


# generate unique label for entries, using as much author & year data as possible
generate_bibliographic_names<-function(x){
	nonunique_names<-unlist(lapply(x, function(a){
		name_vector<-rep("", 3)
		if(any(names(a)=="author")){
			name_vector[1]<-strsplit(a$author[1], ",")[[1]][1]}
		if(any(names(a)=="year")){name_vector[2]<-a$year}
		if(any(names(a)=="journal")){
			journal_info<-strsplit(a$journal, " ")[[1]]
			name_vector[3]<-paste(substr(journal_info, 1, min(nchar(journal_info), 4)), collapse="")
			}
		name_vector<-name_vector[which(name_vector!="")]
		if(length(name_vector)==0){return("ref")
		}else{return(paste(name_vector, collapse="_"))}		
		}))

	# where this is not possible, give a 'ref1' style result only as a last resort.
	if(any(nonunique_names=="ref")){
		rows.tr<-which(nonunique_names=="ref")
		nonunique_names[rows.tr]<-paste("ref", c(1:length(rows.tr)), sep="_")
		}

	# ensure names are unique
	if(length(unique(nonunique_names))<length(nonunique_names)){
		name_counts<-xtabs(~nonunique_names)
		duplicated_names<-names(which(name_counts>1))
		for(i in 1:length(duplicated_names)){
			rows<-which(nonunique_names== duplicated_names[i])
			new_names<-paste(nonunique_names[rows], letters[1:length(rows)], sep="_")
			nonunique_names[rows]<-new_names}}
			
	return(nonunique_names)
}
			

# RIS
read_ris<-function(x){

	# merge data with lookup info, to provide bib-style tags
	lookup<-data.frame(ris=c("TY", 
			"AU", paste("A", c(1:5), sep=""), # author
			"PY", "Y1", # year
			"TI", "T1", # title
			"JO", "T2", "T3", "SO", "JT", "JF", "JA", # journal
			"VL", "IS", 
			"EP", "BP", "SP", # pages
			"AB", "N2", # abstract
			"KW", "DE", # "ID", # keywords
			"DO", "CN", 
			"SN", "UR", "AN", "CY", "PB", 
			"PP", "AD", "ED", "ET", "LA"), 
		bib=c("type", rep("author", 6), rep("year", 2), rep("title", 2,),
			rep("journal", 7),
			"volume", "number", 
			rep("pages", 3),
			rep("abstract", 2), 
			rep("keywords", 2), 
			"doi", "call",
			"issn", "url", "accession", "institution", "publisher",
			"pubplace", "address", "editor", "edition", "language"),
		order=c(1, rep(2, 6), 3, 3, 4, 4, rep(5, 7), 6, 7, 8, 8, 8, 9, 9, 10, 10, 11:22),
		stringsAsFactors=FALSE)
	x.merge<-merge(x, lookup, by="ris", all.x=TRUE, all.y=FALSE)
	x.merge<-x.merge[order(x.merge$row.order), ]

	# find a way to store missing .bib data rather than discard
	if(any(is.na(x.merge$bib))){
		rows.tr<-which(is.na(x.merge$bib))
		x.merge$bib[rows.tr]<-"further_info"
		x.merge$order[rows.tr]<-99
		}

	# method to systematically search for year data
	# if(any(x.merge$bib=="year", na.rm=TRUE))
	year_check<-regexpr("\\d{4}", x.merge$text)
	if(any(year_check>0)){
		check_rows<-which(year_check>0)
		year_strings<-as.numeric(substr(x.merge$text[check_rows], 
			year_check[check_rows],  year_check[check_rows]+3))
		if(any(x.merge$bib[check_rows]=="year", na.rm=TRUE)){
			year_rows<-which(x.merge$bib[check_rows]=="year")
			x.merge$text[check_rows[year_rows]]<-year_strings[year_rows]
		}else{
			possible_rows<-which(year_strings>1850 & year_strings <= as.numeric(format(Sys.Date(), "%Y")))
			tag_frequencies<-as.data.frame(
				xtabs(~x.merge$ris[check_rows[possible_rows]]), 
				stringsAsFactors=FALSE)
				colnames(tag_frequencies)<-c("tag", "n")
			# now work out what proportion of each tag contain year data
			# compare against number of references to determine likelihood of being 'the' year tag
			tag_frequencies$prop<-tag_frequencies$n/(max(x.merge$ref)+1) # number of references
			if(any(tag_frequencies$prop>0.9)){
				year_tag<-tag_frequencies$tag[which.max(tag_frequencies$prop)]
				rows.tr<-which(x.merge$ris==year_tag)
				x.merge$bib[rows.tr]<-"year"
				x.merge$order[rows.tr]<-3
				x.merge$text[rows.tr]<-substr(x.merge$text[rows.tr], year_check[rows.tr],  year_check[rows.tr]+3)
				}
			}
		}

	# use code from blog.datacite.org for doi detection
	# then return a consistent format - i.e. no www.dx.doi.org/ etc.
	# regexpr("/^10.d{4,9}/[-._;()/:A-Z0-9]+$/i", test) # original code
	doi_check<-regexpr("/10.\\d{4,9}/", x.merge$text) # my version
	if(any(doi_check>0)){
		check_rows<-which(doi_check>0)
		x.merge$bib[check_rows]<-"doi"
		x.merge$order[check_rows]<-11	
		x.merge$text[check_rows]<-substr(x.merge$text[check_rows], 
			start=doi_check[check_rows]+1, 
			stop=nchar(x.merge$text[check_rows]))
		}

	# ensure author data from a single ris tag
	if(any(x.merge$bib=="author")){
		lookup.tags<-xtabs(~x.merge$ris[which(x.merge$bib=="author")])
		if(length(lookup.tags)>1){
			max.tags<-which(lookup.tags==max(lookup.tags))
			if(length(max.tags)>1){
				tag_nchar<-unlist(lapply(as.list(names(max.tags)), function(a, data){
					mean(nchar(data$text[which(data$ris==a)]))
					}, data=x.merge))
				final_tag<-names(tag_nchar[which.max(tag_nchar)])
			}else{final_tag<-names(max.tags)}
			replace_rows<-which(x.merge$bib=="author" & x.merge$ris!=final_tag)
			x.merge$bib[replace_rows]<-"further_info"
			x.merge$order[replace_rows]<-99
		}
	}

	# convert into a list, where each reference is a separate entry
	x.split<-split(x.merge[c("bib", "ris", "text", "order")], x.merge$ref)

	# convert to list format
	x.final<-lapply(x.split, function(a){
		result<-split(a$text, a$bib)
		# MISC
		if(any(names(result)=="further_info")){
			names(result$further_info)<-a$ris[which(a$bib=="further_info")]}
		# YEAR
		if(any(names(result)=="year")){	
			if(any(nchar(result$year)>=4)){
				year_check<-regexpr("\\d{4}", result$year)
				if(any(year_check>0)){
					result$year<-substr(result$year[which(year_check>0)], year_check[1], year_check[1]+3)
				}else{result$year<-""}
			}else{result$year<-""}
			}
		# TITLE
		if(any(names(result)=="title")){
			if(length(result$title)>1){
				if(result$title[1]==result$title[2]){result$title<-result$title[1]
				}else{result$title<-paste(result$title, collapse=" ")}}
			result$title <-gsub("   ", " ", result$title) # in case of error (3 spaces)
			result$title <-gsub("  ", " ", result$title) # ditto (2 spaces)
			result$title <-sub("\\.$", "", result$title) # remove final full stops
			}
		# JOURNAL
		if(any(names(result)=="journal")){
			unique_journals<-unique(result$journal)
			if(length(unique_journals)>1){
				unique_journals<-unique_journals[order(nchar(unique_journals), decreasing=FALSE)]
				result$journal<-unique_journals[1]
				result$journal_secondary<-paste(unique_journals[c(2:length(unique_journals))], collapse=" ")
			}else{result$journal<-unique_journals}		
			result$journal <-gsub("  ", " ", result$journal)
			result$journal <-sub("\\.$", "", result$journal) 
			}
		# ABSTRACT
		if(length(result$abstract>1)){
			result$abstract <-paste(result$abstract, collapse=" ")
			result$abstract<-gsub("   ", " ", result$abstract) # in case of error (3 spaces)
			result$abstract<-gsub("  ", " ", result$abstract) # ditto (2 spaces)
			}
		# PAGE NUMBER
		if(any(names(result)=="pages")){
			if(length(result$pages)>1){result$pages<-paste(sort(result$pages), collapse="-")}}
		entry.order<-unlist(lapply(names(result), function(b, order){
				order$order[which(order$bib==b)[1]]}, order=a))
		final_result<-result[order(entry.order)]

		return(final_result)
		})

	names(x.final)<-generate_bibliographic_names(x.final)
	class(x.final)<-"bibliography"
	return(x.final)
	}



# BIB
read_bib<-function(x){

	# which lines start with @article?
	group_vec<-rep(0, length(x))
	row_id<-which(regexpr("^@", x)==1) 
	group_vec[row_id]<-1
	group_vec<-cumsum(group_vec)
	x.split<-split(x[-row_id], group_vec[-row_id])
	length_vals<-unlist(lapply(x.split, length))
	x.split<-x.split[which(length_vals>3)]

	x.final<-lapply(x.split, function(z){
		delimiter_lookup<-regexpr("=\\s\\{|=\\s\\{\\{|=\\{|=\\{\\{", gsub("\\s", "", z))
		single_entry_matrix<-apply(cbind(z, delimiter_lookup), 1, function(a){c(
			tag=substr(a[1], 1, as.numeric(a[2])-1),
			value=substr(a[1], as.numeric(a[2]), nchar(a[1]))
			)
		})
		entry_dframe<-as.data.frame(t(single_entry_matrix), stringsAsFactors=FALSE)
		colnames(entry_dframe)<-c("tag", "value")
		if(any(entry_dframe$value=="}")){entry_dframe<-entry_dframe[c(1:which(entry_dframe$value=="}")[1]-1), ]}
	
		# strip curly brackets etc
		bib_tag_string<-"=\\s\\{|=\\s\\{\\{|=\\{|=\\{\\{|}\\}|\\},|\\}\\}}|\\}\\},"
		entry_dframe$value<-gsub(bib_tag_string, "", entry_dframe$value)
		entry_dframe$value<-gsub("^\\s+|\\s+$",  "", entry_dframe$value)
	
		# convert each entry to a list
		label_group<-rep(0, nrow(entry_dframe))
		tag_rows<-which(entry_dframe$tag!="")
		label_group[tag_rows]<-1
		tag_names<-entry_dframe$tag[tag_rows]
		entry_list<-split(entry_dframe$value, cumsum(label_group)+1)
		names(entry_list)<-tolower(gsub("^\\s+|\\s+$",  "", tag_names))
	
		entry_list<-lapply(entry_list, function(a){paste(a, collapse=" ")})
		if(any(names(entry_list)=="author")){entry_list$author<-strsplit(entry_list$author, " and ")[[1]]}
		return(entry_list)
	})
	
	names(x.final)<-generate_bibliographic_names(x.final)
	class(x.final)<-"bibliography"
	return(x.final)
	
	}
