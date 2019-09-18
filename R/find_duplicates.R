find_duplicates <- function(
  data,
  match_variable, # = "title",
  group_variables, # = NULL,
  match_function, # = c("stringdist", # "fuzzdist", "exact"),
  method, # = "osa",
  threshold, # = 5,
  to_lower = FALSE,
  remove_punctuation = FALSE
){

  # error catching
  # data
  if(missing(data)){
    stop("'data' is missing: Please provide a data.frame")
  }
  if(missing(group_variables)){
    group_variables <- NULL
  }else{
    if(!all(group_variables %in% colnames(data))){
      group_variables <- NULL
    }
  }

  # match variable
  if(missing(match_variable)){
    if(any(colnames(data) == "doi")){
      match_variable <- "doi"
      if(missing(match_function)){match_function <- "exact"}
    }else{
      if(any(colnames(data) == "title")){
        match_variable <- "title"
      }else{
        stop("match_variable is missing, with no default;
          please specify which column should be searched for duplicates"
        )
      }
    }
  }else{
    if(!any(colnames(data) == match_variable)){
      stop(paste0(
        match_variable,
        " is not a valid column name in ",
        data,
        ": Please specify which column should be searched for duplicates"
      ))
    }
  }

  # methods
  if(missing(match_function)){match_function <- "stringdist"}
  if(!any(c("fuzzdist", "stringdist", "exact") == match_function)){
    stop(
      paste0(
        match_function,
        " is an invalid input to match_function; please specify one of 'fuzzdist', 'stringdist' or 'exact'."
      )
    )
  }
  if(missing(method)){
    if(match_function == "stringdist"){method <- "osa"}
    if(match_function == "fuzzdist"){method <- "fuzz_m_ratio"}
  }
  if(missing(threshold)){
    if(match_function == "stringdist"){threshold <- 5}
    if(match_function == "fuzzdist"){threshold <- 0.1}
  }
  if(match_function == "exact"){
    method <- NA
    threshold <- NA
  }else{
    valid_methods <- eval(formals(match_function)$method)
    if(!any(valid_methods == method)){
      stop(paste0("'",
        method,
        "' is not a valid method for function '",
        match_function,
        "'; Please specify one of the following arguments: '",
        paste(valid_methods, collapse = "', '"),
        "'"
      ))
    }
  }

  # prep columns
  if(any(colnames(data) == "checked") == FALSE){
    data$checked <- FALSE
  }
  if(any(colnames(data) == "group") == FALSE){
    data$group <- NA
  }
  if(to_lower){
    data[, match_variable] <- tolower(data[, match_variable])
  }
  if(remove_punctuation){
    data[, match_variable] <- tm::removePunctuation(data[, match_variable])
  }

  # run while loop
  progress <- 1
	while(all(data$checked) == FALSE){
    remaining_rows <- which(data$checked == FALSE)
    if(length(remaining_rows) == 1){
			data$group[remaining_rows] <- progress
		  data$checked[remaining_rows] <- TRUE
		}else{
      # locate relevant information
			row_start <- remaining_rows[1]
      # if this entry is empty, then skip (i.e. never match NAs)
      if(is.na(data[row_start, match_variable])){
        data$checked[row_start] <- TRUE
        data$group[row_start] <- progress
      }else{
        # include only those entries in the same grouping categories as the current entry
        # plus any entries that are missing those values
        if(is.null(group_variables)){
          rows_tr <- remaining_rows
        }else{
          match_list <- lapply(group_variables, function(a, data, row){
            (data[, a] == data[row_start, a]) | is.na(data[, a])
            },
            data = data,
            row = row_start
          )
          if(length(group_variables) == 1){
            rows_tr <- which(unlist(match_list))
          }else{
            rows_tr <- which(apply(
              do.call(cbind, match_list),
              1,
              function(a){all(a)}
            ))
          }
        }
        rows_tr <- rows_tr[which(rows_tr != row_start)]

        if(length(rows_tr) > 0){
          if(match_function == "exact"){
            match_result <- (data[rows_tr, match_variable] == data[row_start, match_variable])
          }else{
            match_result <- do.call(
              match_function,
              list(
                a = data[row_start, match_variable],
                b = data[rows_tr, match_variable],
                method = method
              )
            ) <= threshold
          }
          if(any(match_result, na.rm = TRUE)){
            rows_selected <- rows_tr[which(match_result)]
            data$checked[c(row_start, rows_selected)] <- TRUE
            data$group[c(row_start, rows_selected)] <- progress
          }else{
            data$checked[row_start] <- TRUE
            data$group[row_start] <- progress
          }
        }else{
          data$checked[row_start] <- TRUE
          data$group[row_start] <- progress
        }
      } # end if(is.na(data[row_start, match_variable]))
    } # end if(length(remaining_rows) == 1)
    progress <- progress + 1
  } # end while loop

  # add attributes
  result <- data$group
  attr(result, "match_variable") <- match_variable
  if(is.null(group_variables)){
    attr(result, "group_variables") <- NA
  }else{
    attr(result, "group_variables") <- group_variables
  }
  attr(result, "match_function") <- match_function
  attr(result, "method") <- method
  attr(result, "threshold") <- threshold
  return(result)
}