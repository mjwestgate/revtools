# function to create merged datasets
create_grouped_dataframe <- function(data, response_variable, text_variables){
  if(is.null(text_variables)){
    text_variables <- response_variable
  }
  if(is.null(data)){
  	return(NULL)
  }else{
    if(is.null(response_variable)){
      return(data)
    }else{
      data_split <- split(data, data[, response_variable])

      if(length(data_split) == nrow(data)){ # i.e. if there is no grouping, retain whole data.frame
        data_list <- lapply(data_split, function(a, inc_columns, response){
          a$text <- paste(a[inc_columns], collapse = " ")
          a <- a[c(response, colnames(a)[which(colnames(a) != response)])]
          return(a)
        },
        inc_columns = text_variables,
        response = response_variable
        )
      }else{
        data_list <- lapply(data_split, function(a, inc_columns, n_cols, response){
          n_rows <- nrow(a)
          if(n_rows > 1){
            if(n_cols == 1){
      	      text_tr <- paste(a[[inc_columns]], collapse=" ")
      	    }else{
              text_tr <- paste(
                apply(a[, inc_columns], 1, function(b){paste(b, collapse=" ")}),
                collapse = " ")
      	    }
          }else{
            if(n_cols == 1){
              text_tr <- a[[inc_columns]]
      	    }else{
              text_tr <- paste(a[inc_columns], collapse=" ")
            }
          }
          result <- data.frame(
            response = a[[response]][1],
            n = n_rows,
            text = text_tr,
            stringsAsFactors = FALSE
          )
          colnames(result)[1] <- response
          return(result)
        },
        inc_columns = text_variables,
        n_cols = length(text_variables),
        response = response_variable
        )
      } # end if grouped
      out <- do.call(rbind, data_list)
      rownames(out) <- NULL
      return(out)
    }
  }

}