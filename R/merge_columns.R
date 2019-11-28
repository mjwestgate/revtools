# function to rbind two or more data.frames with different column names/orders
merge_columns <- function(
  x, # either a data.frame or a list of the same
  y # a data.frame, optional
){
  if(missing(x)){
    stop("object x is missing with no default")
  }

  if(!any(c("data.frame", "list") == class(x))){
    stop("object x must be either a data.frame or a list")
  }

  if(class(x) == "data.frame"){
    if(missing(y)){
      stop("If x is a data.frame, then y must be supplied")
    }
    x <- list(x, y)
  }else{ # i.e. for lists
    if(!all(unlist(lapply(x, class)) == "data.frame")){
      stop("x must only contain data.frames")
    }
  }

  # get unique colnames
  col_list <- lapply(x, colnames)
  col_names_all <- unique(unlist(col_list))

  # ensure order col_names_all is related to their order in x
  col_lookup <- as.data.frame(lapply(col_list, function(a, lookup){
    unlist(lapply(lookup, function(b){if(any(a == b)){which(a == b)}else{NA}}))
  },
  lookup = col_names_all
  ))
  colnames(col_lookup) <- seq_along(col_list)
  col_order <- apply(col_lookup, 1, function(a){mean(a, na.rm = TRUE)})
  col_names_all <- col_names_all[order(col_order)]

  # merge data in this order
  result_list <- lapply(x, function(a, cn){
    missing_names <- !(cn %in% colnames(a))
    if(any(missing_names)){
      new_names <- cn[missing_names]
      result <- data.frame(
        c(a, sapply(new_names, function(b){NA})),
        stringsAsFactors = FALSE)
      return(result[, cn])
    }else{
      return(a[, cn])
    }
  },
  cn = col_names_all
  )

  return(do.call(rbind, result_list))

}