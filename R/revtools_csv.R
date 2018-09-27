# import csv in a format suitable for revtools apps
revtools_csv <- function(filename){
  data <- read.csv(filename, stringsAsFactors = FALSE)
  colnames(data) <- clean_names(colnames(data))
  if(colnames(data)[1] != "label"){
    if(
      length(unique(data[, 1])) < nrow(data) |
      any(c("author", "title", "year", "journal") == colnames(data)[1])
    ){
      data <- data.frame(
        label = create_index("ref", nrow(data)),
        data,
        stringsAsFactors = FALSE
      )
    }
  }
  if(any(colnames(data) == "author")){
    data$author <- clean_author_delimiters(data$author)
  }
  return(data)
}

# detect author delimiters
clean_author_delimiters <- function(x){
  if(all(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", "\\sand\\s", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and", x, perl = TRUE)
  }
  return(x)
}

# function to create a string of named length in format "string_number" that sorts in correct order
create_index <- function(string, n, sep = "_"){
  if(missing(string)){
    string <- "V"
  }
  if(missing(n)){
    stop("n is missing from create_index with no default")
  }
  if(n < 1){
    stop("n must be > 0 for create_index to function")
  }
  if(length(n) > 1){
    n <- length(n)
  }
  size <- log10(n) + 1
  vector <- seq_len(n)
  return(
    paste(
      string,
      formatC(vector, width  = size, format = "d", flag = 0),
      sep = sep
    )
  )
}

# csv files without messy column names
clean_names <- function(
  x # colnames
){
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  x <- tolower(x)
  x <- sub("authors", "author", x) # remove plural authors
  x <- make.unique(x, sep = "_")
  return(x)
}