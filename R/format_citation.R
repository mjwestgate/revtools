format_citation <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  ...
){
  UseMethod("format_citation")
  }


# Function to display bibliographic information on selected articles
format_citation.list <- function(
	data, # list of data from a standard import function
  details = TRUE, # whether to allow or suppress bibliographic details - name, year, journal
	abstract = FALSE, # option to return only the citation for an article
  add_html = FALSE,
  ...
	){
  	if(!details){
      result <- as.character(data["title"])
    }else{
  		if(any(names(data) == "author")){
  			# author info
  			# remove any additional characters that display affiliations (i.e. those after last ".")
  			author.data <- unlist(lapply(
          strsplit(as.character(data$author), ""),
          function(a){
    				dot.lookup <- a %in% "."
    				if(any(dot.lookup)){
              a <- a[seq_len(max(which(dot.lookup)))]
            }
    				return(paste(a, collapse = ""))
    			}
        ))
  			if(any(grepl(",", author.data))){
  				author.data <- unlist(lapply(
            strsplit(author.data, ", "),
            function(a){paste(a[2], a[1], sep = " ")}
          ))
  			}
  			n.authors <- length(data$author)
  			if(n.authors >= 4){
          n.authors <- 4
        }
  			author.info <- switch(as.character(n.authors),
  				"0" = "Anon.",
  				"1" = author.data,
  				"2" = {paste(author.data, collapse = " & ")},
  				"3" = {paste0(author.data[1], ", ", author.data[2], " & ", author.data[3])},
  				"4" = {paste0(author.data[1], ", ", author.data[2], " et al.")})
  		}
    # }
  		# paste info in the correct order
  		lookup.headers <- c("year", "title", "journal", "volume", "pages")
  		lookup.result <- lookup.headers %in% names(data)
  		if(all(lookup.result)){
  			result <- paste0(
          author.info,
          " (", data$year, ") ",
  				data$title, ". ", data$journal, " ", data$volume, ": ", data$pages
        )
  		}else{
  			result <- paste(
          author.info,
          paste(data[lookup.headers[lookup.result]], collapse = " "),
          sep = " "
        )
      }
		  # note - the above doesn't add brackets around year
    }
		# add abstract if required
		if(abstract & any(names(data) == "abstract")){
			result <- paste0(result, ".<br><br><strong>Abstract</strong><br>", data$abstract)
  	}

	return(result)
	}


format_citation.bibliography <-  function(
	data,
  details = TRUE,
	abstract = FALSE,
  add_html = FALSE,
  ...
	){
  lapply(data, function(a, details, abstract, add_html){
    format_citation.list(a, details, abstract, add_html)
    },
    details = details,
    abstract = abstract,
    add_html = add_html
  )
}

# duplicate version for calling apply on a data.frame
format_citation.data.frame <- function(
  data,
  details = TRUE, # whether to allow (TRUE) or suppress (FALSE) bibliographic details
  abstract = FALSE,
  add_html = FALSE,
  ...
  ){
  colnames(data) <- clean_names(colnames(data))
  if(any(names(data) == "journal")){
    source <- "journal"
  }else{
    source_check <- grepl("source", names(data))
    if(any(source_check)){
      source <- names(data)[which(source_check)[1]]
    }else{
      source <- "NA"
    }
  }

  if(
    all(c("author", "year", source, "title") %in% names(data)) &
    (details == TRUE)
  ){
	data_list <- split(data, seq_len(nrow(data)))
  data_out <- unlist(lapply(data_list, function(a){
		author_vector <- strsplit(a[['author']], " and ")[[1]]
		if(length(author_vector) == 1){
      author_text <- a[['author']]
		}else{
      author_text <- paste0(author_vector[1], " et al.")
    }
    if(add_html){
      journal_text <- paste0("<i>", a[[source]], "</i>. ")
    }else{
      journal_text <- paste0(a[[source]], ". ")
    }
		text_vector <- paste0(
      author_text,
      " (", a[['year']], ") ",
      a[['title']], ". ",
      journal_text
    )
    return(text_vector)
  }))
	}else{
    if(details == FALSE){ #} & (names(x)[1] == "label")){
      if(any(names(data) == "title")){
        data_out <- data[["title"]]
      }else{
        data_out <- data[, 1]
      }
    }else{
      data_out <- data[, 1]
    }
	}
  return(data_out)
}


# now organize so that line breaks are added at word breaks every y characters
add_line_breaks <- function(data){
	split_text <- strsplit(as.character(data), " ")
  out_list <- lapply(split_text, function(a){
    if(length(a) == 0){
      return("")
    }else{
    	result <- data.frame(
    		text = a,
    		nchars = nchar(a),
    		stringsAsFactors = FALSE
      )
    	result$sum <- cumsum(result$nchars)
    	result$group <- cut(result$sum,
    		breaks = seq(0, max(result$sum)+49, 50),
    		labels = FALSE)
    	result_list <- split(result$text, result$group)
    	result <- paste(
        unlist(
          lapply(result_list, function(a){paste(a, collapse = " ")})
        ),
        collapse = "\n")
      return(result)
    }
  })
  return(unlist(out_list))
}