format_duplicates <- function(x){
  # remove columns that contain no data for this entry
  if(any(is.na(x))){
    x <- x[which(!is.na(x))]
  }
  # expand out author names
  if(any(names(x) == "author")){
    value <- which(names(x) == "author")
    x[[value]] <- paste(
      strsplit(x[[value]], split = " and ")[[1]],
      collapse = "<br>&nbsp;&nbsp;&nbsp;"
    )
  }
  # expand out keywords
  if(any(names(x) == "keywords")){
    value <- which(names(x) == "keywords")
    x[[value]] <- paste(
      strsplit(x[[value]], split = " and ")[[1]],
      collapse = "; "
    )
  }
  # title case column names
  labels <- tools::toTitleCase(names(x))

  cat(
    paste(
      unlist(lapply(
        labels,
        function(a){paste0("<b>", a, ":</b> ")}
      )),
      x,
      collapse = "<br>"
    )
  )
}