format_duplicates <- function(
  x,
  columns,
  breaks
){
  if(missing(columns)){
    columns <- names(x)
  }
  if(breaks){
    collapse_val <- "<br>&nbsp;&nbsp;&nbsp;"
  }else{
    collapse_val <- "; "
  }

  # show data only for selected columns
  x <- x[which(names(x) %in% columns)]

  # remove columns that contain no data for this entry
  if(any(is.na(x))){
    x <- x[which(!is.na(x))]
  }
  # expand out author names
  if(any(names(x) == "author")){
    value <- which(names(x) == "author")
    x[[value]] <- paste(
      strsplit(x[[value]], split = " and ")[[1]],
      collapse = collapse_val
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