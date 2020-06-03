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