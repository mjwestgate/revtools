summary.bibliography <- function(object, ...){

	# are any abstracts completely missing?
	null_check <- unlist(lapply(
    object,
    function(a){is.null(a$abstract)}
  ))
	null_count <- length(object) - length(which(null_check))
	null_percent <- round((100/length(object)) * null_count, 1)

	# how many sources?
	sources <- unlist(lapply(
    object,
    function(a){a$journal}
  ))
	if(!is.null(sources)){
		n_sources <- length(unique(sources))
		source_freq <- sort(
      xtabs(~ sources),
      decreasing = TRUE
    )[seq_len(min(c(5, n_sources)))]
		# put text together
		result <- paste(
			paste0(
        "Object of class 'bibliography' containing ",
        length(object),
        " entries.",
        "\n  ",
  			"Number containing abstracts: ",
        null_count,
        " (",
        null_percent,
        "%)",
        "\n",
			  "Number of sources: ",
        n_sources,
        "\n",
			  "Most common sources:",
        "\n  "
      ),
		  paste(
        names(source_freq),
        " (n = ",
        as.numeric(source_freq),
        ")",
        sep = "",
        collapse = "\n  "
      ),
  		sep = "",
  		collapse = "\n")
	}else{
		result <- paste0(
			"Object of class 'bibliography' containing ",
      length(object),
      " entries.",
      "\n  ",
			"Number containing abstracts: ",
      null_count,
      " (",
      null_percent,
      "%)",
      "\n"
    )
	}
	cat(result, sep = "\n")
}


print.bibliography <- function(x, n, ...){
	length_tr <- length(x)
	if(missing(n)){
		n <- min(c(length_tr, 5))
	}else{
		if(n > length_tr){
      n <- length_tr
    }
	}
	text_tr <- format_citation(x[seq_len(n)])
	cat(paste(unlist(text_tr), collapse = "\n\n"))
}


'[.bibliography' <- function(x, n){
  class(x) <- "list"
  if(all(n %in% seq_len(length(x))) == FALSE){
    stop("subset out of bounds")
  }
  z <- x[n]
  class(z) <- "bibliography"
  return(z)
}


c.bibliography <- function(...){
  result <- lapply(list(...), function(a){
    class(a) <- "list"
    return(a)
  })
  result <- do.call(c, result)
  class(result) <- "bibliography"
  return(result)
}


# function to convert an object of class 'bibliography' into a data.frame
as.data.frame.bibliography <- function(x, ...){

	cols <- unique(unlist(lapply(x, names)))

  # fix bug where ris tags get placed first if they appear before bib tags
  col_n <- nchar(cols)
  if(any(col_n < 3)){
    cols <- cols[c(which(col_n >= 3), which(col_n < 3))]
  }

	x_list <- lapply(x, function(a, cols){
		result <- lapply(cols, function(b, lookup){
			if(any(names(lookup) == b)){
				data_tr <- lookup[[b]]
				if(length(data_tr) > 1){
          data_tr <- paste0(data_tr, collapse = " and ")
        }
				return(data_tr)
			}else{
        return(NA)
      }
		},
    lookup = a)
		names(result) <- cols
		return(
      as.data.frame(
        result,
        stringsAsFactors=FALSE
      )
    )
		},
    cols = cols
  )

	x_dframe <- data.frame(
		label = make.names(names(x_list), unique=TRUE),
		do.call(rbind, x_list),
		stringsAsFactors = FALSE
  )
	rownames(x_dframe) <- NULL

	return(x_dframe)
}


as.bibliography <- function(x, ...){

	if(class(x) != "data.frame"){
		stop("as.bibliography can only be called for objects of class 'data.frame'")
	}

	# get labels for each entry
	x_cols <- colnames(x)
	if(any(x_cols == "label")){
		label_col <- which(x_cols == "label")
		label_vec <- x[, label_col]
		x <- x[, -label_col]
	}else{
		label_vec <- create_index("ref", nrow(x))
	}

	x_list <- lapply(
    split(x, seq_len(nrow(x))),
    function(a){
  		a <- as.list(a)
  		if(any(names(a) == "author")){
        a$author <- strsplit(a$author, " and ")[[1]]
      }
  		if(any(names(a) == "keywords")){
        a$keywords <- strsplit(a$keywords, " and ")[[1]]
      }
  		return(a)
    }
	)
	names(x_list) <- label_vec
	class(x_list) <- "bibliography"
	return(x_list)
}