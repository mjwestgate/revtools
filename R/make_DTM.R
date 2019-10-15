# function to take a data.frame with bibliographic information, extract useful information, and make a DTM
make_DTM <- function(
  x, # a vector
	stop_words,
  min_freq = 0.01,
  max_freq = 0.85,
  ngram_check = TRUE,
  ngram_quantile = 0.8,
  return_matrix = FALSE
){
  make_dtm(x, stop_words,
    min_freq, max_freq,
    ngram_check, ngram_quantile,
    return_matrix
  )
}

make_dtm <- function(
	x,
	stop_words,
  min_freq = 0.01,
  max_freq = 0.85,
  ngram_check = TRUE,
  ngram_quantile = 0.8,
  return_matrix = FALSE
){

  # check format
  if(class(x) != "character"){
	  stop("make_dtm only accepts arguments of class 'character'")
	}
  n <- length(x)

	# sort out stop words
	if(missing(stop_words)){
    stop_words <- revwords()
	}else{
    stop_words <- unique(
      tolower(stop_words)
    )
  }

  # clean up text using tm functions
  x <- tolower(x)
  x <- gsub(" - ", " ", x) # remove separated dashes
  x <- tm::removePunctuation(x,
    preserve_intra_word_contractions = FALSE,
    preserve_intra_word_dashes = TRUE
  )

  # ngrams
  if(ngram_check){
    ngrams <- ngram::get.phrasetable(ngram::ngram(x))
    ngrams <- ngrams[ngrams$freq > 2, ]
    ngrams <- ngrams[ngrams$freq > stats::quantile(ngrams$freq, ngram_quantile), ]

    if(nrow(ngrams) > 0){
      # split into pairs
      ngram_list <- strsplit(ngrams$ngrams, " ")
      ngram_df <- as.data.frame(
        do.call(rbind, ngram_list),
        stringsAsFactors = FALSE
      )

      # remove small & stop words
      keep_rows <- apply(ngram_df[, 1:2], 1, function(a, sw){
        all(nchar(a) > 4) & !any(a %in% sw)
      }, sw = stop_words)
      if(any(keep_rows)){
        ngram_df <- ngram_df[keep_rows, ]

        source_text <- apply(ngram_df, 1, function(a){paste(a, collapse = " ")})
        replacement_text <- apply(ngram_df, 1, function(a){paste(a, collapse = "_")})

        # replace in a loop
        for(i in seq_along(source_text)){
          x <- gsub(source_text[i], replacement_text[i], x)
        }

      } # end if any kept ngrams
    } # end if >0 ngrams
  } # end if ngram_check

  # continue tm
  x <- tm::removeWords(x, stop_words)
  x <- tm::removeNumbers(x)

	# calculate how rare and common terms should be treated
  bound_values <- c(
    max(c(ceiling(n * min_freq), 3)),
    floor(n * max_freq)
  )
  # apply to dataset
	dtm <- tm::DocumentTermMatrix(
    x = tm::Corpus(tm::VectorSource(x)),
    control = list(
      wordLengths = c(4, Inf), # default
      bounds = list(global = bound_values)
    )
  )

  # get stem of words using SnowballC
  stem_terms <- SnowballC::wordStem(dtm$dimnames$Terms)

  # create a lookup data.frame to swtich from unstemmed to stemmed words
  lookup <- data.frame(
    initial_n = seq_along(dtm$dimnames$Terms),
    initial = dtm$dimnames$Terms,
    stemmed = SnowballC::wordStem(dtm$dimnames$Terms),
    stringsAsFactors = FALSE
  )

  # does this contain duplicates?
  if(base::anyDuplicated(lookup$stemmed) > 0){

    # convert dtm to a df
    dtm_df <- data.frame(
      i = dtm$i, # articles
      j = dtm$j, # words
      v = dtm$v  # counts
    )
    # more efficient to use the shortest word, not the most common term
    lookup$n <- nchar(lookup$initial)

    # determine which source word should replace the stemmed word
    text_split <- split(lookup[, c("initial_n", "n")], lookup$stemmed)
    text_match <- data.frame(
      initial_n = unlist(lapply(text_split, function(a){a$initial_n})),
      final_n = unlist(lapply(text_split,
        function(a){
          if(nrow(a) > 1){
            rep(a$initial_n[order(a$n, decreasing = FALSE)[1]], nrow(a))
          }else{
            a$initial_n
          }
        }))
      )
    lookup$final_n <- text_match$final_n[order(text_match$initial_n)]


    # create new dtm_df that ranks by stemmed words
    dtm_df$j_new <- lookup$final_n[dtm_df$j]
    dtm_list <- split(dtm_df[c("j_new", "v")], dtm_df$i)
    dtm_df2 <- do.call(rbind, lapply(
      seq_along(dtm_list),
      function(a, data){
        result <- unlist(lapply(split(data[[a]]$v, data[[a]]$j_new), sum))
        return(data.frame(
          i = a,
          j = as.numeric(names(result)),
          v = result
        ))
      }, data = dtm_list
    ))

    # the new index has a higher maximum than length
    # ensure index is continuous from 1 to length
    unique_j <- sort(unique(lookup$final_n))
    lookup2 <- data.frame(
      index = seq_len(max(lookup$final_n)),
      end = NA
    )
    lookup2$end[unique_j] <- seq_along(unique_j)

    # use to create simple_triplet_matrix
    dtm2 <- slam::simple_triplet_matrix(
      i = dtm_df2$i,
      j = lookup2$end[dtm_df2$j],
      v = dtm_df2$v,
      dimnames = list(
        "Docs" = as.character(seq_along(unique(dtm_df2$i))),
        "Terms" = lookup$initial[sort(unique(lookup$final_n))]
      )
    )

  }else{
    dtm2 <- dtm
  } # end if duplicated

  # optionally return a traditional matrix
  if(return_matrix){
    output <- as.matrix(dtm2) # convert back to matrix
    rownames(output) <- paste0(
      "V",
      seq_len(nrow(output))
    )
    return(output)
  }else{
    return(dtm2)
  }

}