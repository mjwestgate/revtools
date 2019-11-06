# function to export data in .bib format
write_bibliography <- function(x, filename, format = "ris"){

  if(missing(filename)){
    stop("argument 'filename' is missing, with no default")
  }
  if(!any(c("bibliography", "data.frame") == class(x))){
    stop("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(class(x) == "data.frame"){
    x <- as.bibliography(x)
  }

  if(format == "bib"){
  	# process basic text
  	result <- lapply(x, function(a){
  		if(any(names(a) == "author")){
        a$author <- paste(a$author, collapse=" and ")
      }
  		a <- lapply(a, function(b){ 	# ensure only one entry per value
  			if(length(b) > 1){
          paste(b, collapse = "; ")
  			}else{
          b
        }
      })
  		paste0(names(a), "={", a, "},") # format as text
  	})

  	# add article identifier info
  	export <- unlist(lapply(
      seq_len(length(result)),
      function(a, source, entry_names){
    		c(
          paste0("@ARTICLE{", entry_names[a], ","),
    			source[a],
          "}",
          ""
        )
  		},
      source = result,
      entry_names = names(x))
  		)
  	names(export) <- NULL

  }

  if(format == "ris"){

  	result <- lapply(x, function(a, lookup){

  		# convert to tagged vector
  		b <- do.call(c, a)
  		b <- data.frame(
        tag = c(names(b), "end"),
        entry = c(b, ""),
        stringsAsFactors = FALSE
      )
  		rownames(b) <- NULL
  		b$tag <- gsub("[[:digit:]]", "", b$tag)

  		# page information needs to be treated separately
  		if(any(b$tag == "pages")){
  			page_row <- which(b$tag == "pages")
  			page_sep <- strsplit(b$entry[page_row], "-")[[1]]
        page_sep <- page_sep[grepl("^[[:digit:]]+$", page_sep)]
  			if(length(page_sep) > 1){
  				new_rows <- data.frame(
            tag = c("startpage", "endpage"),
  					entry = page_sep[1:2],
            stringsAsFactors = FALSE
          )
  				b <- as.data.frame(rbind(
  					b[c(1:(page_row-1)), ],
  					new_rows,
  					b[c((page_row+1):nrow(b)), ])
          )
  			}}
  		b$order <- seq_len(nrow(b))

  		# substitute tags for ris format versions
  		b <- merge(lookup, b,
        by.x = "bib",
        by.y = "tag",
        all.x = FALSE,
        all.y = TRUE
      )
  		b <- b[order(b$order), 2:3]
      b <- b[which(!is.na(b$ris)), ]

  		# concatenate rows, return a vector of strings
  		c(paste(b$ris, b$entry, sep = "  - "), "")

  		},
      lookup = tag_lookup(type = "ris_write")[, 1:2]
    )

  	export <- do.call(c, result)
  	} # end ris

  # export
  write.table(
    export,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE)

} #  end function