format_citation <- function(x, ...){
  UseMethod("format_citation")
  }



# Function to display bibliographic information on selected articles
format_citation.list <- function(
	x, # list of data from a standard import function
  details = TRUE, # whether to allow or suppress bibliographic details - name, year, journal
	abstract = FALSE, # option to return only the citation for an article
  add_html = FALSE
	){
  	if(!details){
      # author.info <- ""
      result <- as.character(x["title"])
    }else{
  		if(any(names(x) == "author")){
  			# author info
  			# remove any additional characters that display affiliations (i.e. those after last ".")
  			author.data<-unlist(lapply(strsplit(x$author, ""), function(a){
  				dot.lookup<-a %in% "."
  				if(any(dot.lookup)){a<-a[1:max(which(dot.lookup))]}
  				return(paste(a, collapse=""))
  				}))
  			if(any(grepl(",", author.data))){
  				author.data <- unlist(lapply(
            strsplit(author.data, ", "),
            function(a){paste(a[2], a[1], sep=" ")}
          ))
  			}
  			n.authors<-length(x$author)
  			if(n.authors>=4){
          n.authors<-4
        }
  			author.info<-switch(as.character(n.authors),
  				"0" = "Anon.",
  				"1" = author.data,
  				"2" = {paste(author.data, collapse=" & ")},
  				"3" = {paste0(author.data[1], ", ", author.data[2], " & ", author.data[3])},
  				"4" = {paste0(author.data[1], ", ", author.data[2], " et al.")})
  		}
    # }
  		# paste info in the correct order
  		lookup.headers <- c("year", "title", "journal", "volume", "pages")
  		lookup.result <- lookup.headers %in% names(x)
  		if(all(lookup.result)){
  			result <- paste0(
          author.info,
          " (", x$year, ") ",
  				x$title, ". ", x$journal, " ", x$volume, ": ", x$pages
        )
  		}else{
  			result <- paste(
          author.info,
          paste(x[lookup.headers[lookup.result]], collapse=" "),
          sep=" "
        )
      }
		  # note - the above doesn't add brackets around year
    }
		# add abstract if required
		if(abstract & any(names(x) == "abstract")){
			result <- paste0(result, ".<br><br><strong>Abstract</strong><br>", x$abstract)
  	}

	return(result)
	}


format_citation.bibliography <-  function(
	x,
  details = TRUE,
	abstract = FALSE,
  add_html = FALSE
	){
  lapply(x, function(a, details, abstract, add_html){
    format_citation.list(a, details, abstract, add_html)
    },
    details = details,
    abstract = abstract,
    add_html = add_html
  )
}

# duplicate version for calling apply on a data.frame
format_citation.data.frame <- function(
  x,
  details = TRUE, # whether to allow or suppress bibliographic details - name, year, journal
  abstract = FALSE,
  add_html = FALSE
  ){
  if(
    all(c("author", "year", "title", "journal") %in% names(x)) &
    details &
    names(x)[1] == "label"
  ){
	# if(all(c("author", "year", "title", "journal") %in% colnames(df)) & !hide_details){
		author_vector <- strsplit(x[['author']], " and ")[[1]]
		if(length(author_vector) == 1){
      author_text <- x[['author']]
		}else{
      author_text <- paste0(author_vector[1], " et al.")
    }
    if(add_html){
      journal_text <- paste0("<i>", x[['journal']], "</i>. ")
    }else{
      journal_text <- paste0(x[['journal']], ". ")
    }
		text_vector <- paste0(
      author_text,
      " (", x[['year']], ") ",
      x[['title']], ". ",
      journal_text
    )
	}else{
    if((details == FALSE) & (names(x)[1] == "label")){
      if(any(names(x) == "title")){
        text_vector <- x[["title"]]
      }else{
        text_vector <- x[[1]]
      }
    }else{
      text_vector <- x[[1]]
    }
	}
  return(text_vector)
}


# now organize so that line breaks are added at word breaks every y characters
add_line_breaks <- function(x){
	split_vector <- strsplit(x, " ")[[1]]
	result_dframe <- data.frame(
		text = split_vector,
		nchars = nchar(split_vector),
		stringsAsFactors = FALSE
  )
	result_dframe$sum <- cumsum(result_dframe$nchars)
	result_dframe$group <- cut(result_dframe$sum,
		breaks = seq(0, max(result_dframe$sum)+49, 50),
		labels = FALSE)
	result_list <- split(result_dframe$text, result_dframe$group)
	result <- paste(
    unlist(
      lapply(result_list, function(a){paste(a, collapse = " ")})
    ),
    collapse = "\n")
  return(result)
}