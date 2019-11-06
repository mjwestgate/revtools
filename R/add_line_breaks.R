# add line breaks every n characters
add_line_breaks <- function(x, n = 50, html = FALSE){
  if(html){break_string <- "<br>"}else{break_string <- "\n"}
	split_text <- strsplit(as.character(x), " ")
  out_list <- lapply(split_text, function(a){
    if(length(a) == 0){
      return("")
    }else{
      result <- data.frame(
        text = a,
        nchars = nchar(a, allowNA = TRUE, keepNA = TRUE),
        stringsAsFactors = FALSE
      )
      if(any(is.na(result$nchars))){
        result$nchars[which(is.na(result$nchars))] <- 2
      }
    	result$sum <- cumsum(result$nchars)
    	result$group <- cut(result$sum,
    		breaks = seq(0, max(result$sum)+n-1, n),
    		labels = FALSE)
    	result_list <- split(result$text, result$group)
    	result <- paste(
        unlist(
          lapply(result_list, function(a){paste(a, collapse = " ")})
        ),
        collapse = break_string)
      return(result)
    }
  })
  return(unlist(out_list))
}