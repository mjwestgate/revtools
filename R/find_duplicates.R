find_duplicates <- function(
  data,
  match_variable,
  group_variables = NULL,
  match_function = "fuzzdist",
  algorithm = "m_ratio",
  threshold = 0.1
){
  # prep columns
  if(any(colnames(data) == "checked") == FALSE){
    data$checked <- FALSE
  }
  if(any(colnames(data) == "group") == FALSE){
    data$group <- NA
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
      # include only those entries in the same grouping categories as the current entry
      if(is.null(group_variables)){
        rows_tr <- remaining_rows[-1]
      }else{
        match_list <- lapply(group_variables, function(a, data, row){
          data[, a] == data[row_start, a]
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
        match_result <- do.call(
          match_function,
          list(
            data[row_start, match_variable],
            data[rows_tr, match_variable],
            algorithm
          )
        )
        if(any(match_result <= threshold)){
          rows_selected <- which(match_result <= threshold)
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
    }
    progress <- progress + 1
  }
  return(data)
}