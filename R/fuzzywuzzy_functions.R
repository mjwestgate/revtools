# Duplicate of functions from the Python library fuzzywuzzy
# # https://github.com/seatgeek/fuzzywuzzy
# Note: alternative of installing library(Rfuzzywuzzy) not used as it requires Python to be installed

# these functions coded by Martin Westgate on 4th June 2018 based on description given here:
# http://chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python/

fuzz_m_ratio <- function(x, y){
  out<-lapply(y, function(y, x){
    z <- c(x, y)
    z_list <- lapply(strsplit(z, ""), function(a, minval){a[1:minval]}, minval=min(nchar(z)))
    z_match <- apply(do.call(cbind, z_list), 1, function(a){a[1]==a[2]})
    2*length(which(z_match))/sum(nchar(z))
  }, x=x)
  return(as.numeric(out))
}


fuzz_partial_ratio<-function(x, y){
  out<-lapply(y, function(y, x){
    z <- c(x, y)
    zn <- nchar(z)
    n_reps <- (max(zn)-min(zn))
    z_list <- lapply(
      c(0: n_reps),
      function(a, lookup, keep){lookup[(keep+a)]},
      lookup=strsplit(z[which.max(zn)], "")[[1]],
      keep=c(1:min(zn))
    )
    z_ratio <- lapply(z_list, function(a, comparison){
    	match_value<-apply(cbind(a, comparison), 1, function(b){b[1]==b[2]})
    	length(which(match_value))/length(a)
    }, comparison=strsplit(z[which.min(zn)], "")[[1]])
    return(max(as.numeric(z_ratio)))
  }, x=x)
  return(as.numeric(out))
}


fuzz_token_sort_ratio<-function(x, y){
  out<-lapply(y, function(y, x){
    z <- c(x, y)
    z_list <- lapply(strsplit(z, " "), function(a){paste(sort(a), collapse=" ")})
    return(fuzz_m_ratio(z_list[[1]], z_list[[2]]))
  }, x=x)
  return(as.numeric(out))
}


fuzz_token_set_ratio<-function(x, y){
  out<-lapply(y, function(y, x){
    z <- c(x, y)
    z_split <- strsplit(z, " ")
    in_check <- z_split[[1]] %in% z_split[[2]]
    intersection <- sort(z_split[[1]][which(in_check)])
    string_list <- list(
      t0 = intersection,
      t1 = c(intersection, sort(z_split[[1]][which(!in_check)])),
      t2 = c(intersection, sort(z_split[[2]][which(!(z_split[[2]] %in% intersection))]))
    )
    string_list <- lapply(string_list, function(a){
      if(length(a)<1){return("")
  	  }else{return(paste(a, collapse=" "))
  	  }
    })
    result <- c(
      fuzz_m_ratio(string_list$t0, string_list$t1),
      fuzz_m_ratio(string_list$t0, string_list$t2),
      fuzz_m_ratio(string_list$t1, string_list$t2)
      )
    return(max(result))
  }, x=x)
  return(as.numeric(out))
}
