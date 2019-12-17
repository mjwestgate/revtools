format_citation <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
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
  line_breaks = FALSE,
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
				data$title, ". ",
        tools::toTitleCase(tolower(data$journal)),
        " ",
        data$volume,
        ": ",
        data$pages
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

  if(is.logical(line_breaks)){
    if(line_breaks){
      result <- add_line_breaks(result)
    }
  }else{
    if(is.numeric(line_breaks)){
      result <- add_line_breaks(result, line_breaks)
    }
  }

return(result)
}


format_citation.bibliography <-  function(
	data,
  details = TRUE,
	abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
  lapply(data, function(a, details, abstract, add_html, line_breaks){
    format_citation.list(a, details, abstract, add_html, line_breaks)
    },
    details = details,
    abstract = abstract,
    add_html = add_html,
    line_breaks = line_breaks
  )
}

# duplicate version for calling apply on a data.frame
format_citation.data.frame <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
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
      source <- NA
    }
  }

  # this section should be made more flexible to use any available information
  # if(details){
  data_list <- split(data, seq_len(nrow(data)))
  data_out <- unlist(lapply(data_list, function(a){
    cols_tr <- names(a)
    text_list <- as.list(rep(NA, 4))
    names(text_list) <- c("author", "year", "title", "journal")
    # title
    if(any(cols_tr == "title")){
      title_text <- tools::toTitleCase(tolower(a$title))
      if(grepl("[[:punct:]]$", title_text)){
        text_list$title <- title_text
      }else{
        text_list$title <- paste0(title_text, ".")
      }
    }else{
      text_list$title <- ""
    }
    if(details){
      # year
      if(any(cols_tr == "year")){
        text_list$year <- paste0("(", a$year, ")")
      }else{
        text_list$year <- NA
      }
      # journal
      if(!is.na(source)){
        if(!is.na(a[[source]])){
          journal_text <- tools::toTitleCase(tolower(a[[source]]))
          if(add_html){
            text_list$journal <- paste0("<i>", journal_text, "</i>. ")
          }else{
            text_list$journal <- paste0(journal_text, ". ")
          }
        }else{
          text_list$journal <- NA
        }
      }
      # authors
      if(any(cols_tr == "author")){
        author_vector <- strsplit(a[['author']], " and ")[[1]]
        if(length(author_vector) == 1){
          text_list$author <- a[['author']]
        }else{
          text_list$author <- paste0(author_vector[1], " et al.")
        }
      }else{
        if(!all(is.na(text_list))){
          text_list$author <- "Anon."
        }
      }
    } # end if(details)
    text_vec <- unlist(text_list)
    if(all(is.na(text_vec))){
      return(a[1])
    }else{
      return(
        paste(text_vec[!is.na(text_vec)], collapse = " ")
      )
    }
  }))

  # add line breaks if required
  if(is.logical(line_breaks)){
    if(line_breaks){
      data_out <- add_line_breaks(data_out)
    }
  }else{
    if(is.numeric(line_breaks)){
      data_out <- add_line_breaks(data_out, line_breaks)
    }
  }
  return(data_out)
}