# Duplicate of functions from the Python library fuzzywuzzy
# # https://github.com/seatgeek/fuzzywuzzy
# these functions coded by Martin Westgate on 4th June 2018 based on description given here:
# http://chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python/

fuzzdist <- function(a, b,
  method = c("fuzz_m_ratio", "fuzz_partial_ratio", "fuzz_token_sort_ratio", "fuzz_token_set_ratio")
){
  method <- match.arg(method)
  do.call(
    method,
    list(a, b)
  )
}

fuzz_m_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_list <- lapply(strsplit(z, ""),
        function(x, minval){x[1:minval]},
        minval = min(nchar(z))
      )
      z_match <- apply(
        do.call(cbind, z_list),
        1,
        function(x){x[1] == x[2]}
      )
      return(
        1 - (2 * length(which(z_match)) / sum(nchar(z)))
      )
    }
  },
  a = a)
  return(as.numeric(out))
}


fuzz_partial_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      zn <- nchar(z)
      n_reps <- (max(zn) - min(zn))
      z_list <- lapply(
        c(0: n_reps),
        function(x, lookup, keep){lookup[(keep + x)]},
        lookup = strsplit(z[which.max(zn)], "")[[1]],
        keep = seq_len(min(zn))
      )
      z_ratio <- lapply(z_list, function(x, comparison){
      	match_value <- apply(
          cbind(x, comparison),
          1,
          function(y){y[1] == y[2]}
        )
      	length(which(match_value))/length(x)
      },
      comparison = strsplit(z[which.min(zn)], "")[[1]]
      )
      return(1 - max(as.numeric(z_ratio)))
    }
  },
  a = a)
  return(as.numeric(out))
}


fuzz_token_sort_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_list <- lapply(
        strsplit(z, " "),
        function(x){paste(sort(x), collapse = " ")}
      )
      return(
        fuzz_m_ratio(z_list[[1]], z_list[[2]])
      )
    }
  },
  a = a)
  return(as.numeric(out))
}


fuzz_token_set_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_split <- strsplit(z, " ")
      in_check <- z_split[[1]] %in% z_split[[2]]
      intersection <- sort(z_split[[1]][which(in_check)])
      string_list <- list(
        t0 = intersection,
        t1 = c(intersection,
          sort(z_split[[1]][which(!in_check)])
        ),
        t2 = c(intersection,
          sort(z_split[[2]][which(!(z_split[[2]] %in% intersection))])
        )
      )
      string_list <- lapply(string_list, function(x){
        if(length(x) < 1){
          return("")
    	  }else{
          return(paste(x, collapse = " "))
    	  }
      })
      result <- c(
        fuzz_m_ratio(string_list$t0, string_list$t1),
        fuzz_m_ratio(string_list$t0, string_list$t2),
        fuzz_m_ratio(string_list$t1, string_list$t2)
        )
      return(max(result))
    }
  },
  a = a)
  return(as.numeric(out))
}