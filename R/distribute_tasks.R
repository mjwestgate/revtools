# functions for allocating screening tasks among a team

# take inputs to allocate_reviewer_effort and return a clean list of parameters
get_clean_reviewers <- function(
  reviewers,
  effort
){ # x = input to create_reviewer_matrix
  if(!(class(reviewers) %in% c("numeric", "integer", "character"))){
    stop("reviewers must be either a number or a list of names")
  }

  if(is.numeric(reviewers) | is.integer(reviewers)){
    if(length(reviewers) > 1){
      n <- length(reviewers)
      if(is.null(names(reviewers))){
        names <- seq_len(length(reviewers))
      }else{
        names <- names(reviewers)
      }
    }else{ # i.e. length(reviewers) == 1
      if(reviewers < 2){
        stop("Effort can only be divided among two or more reviewers")
      }else{
        n <- reviewers
        names <- seq_len(reviewers)
      }
    }
  }
  if(is.character(reviewers)){
    n <- length(reviewers)
    names <- reviewers
  }

  if(is.null(effort)){
    effort_out <- rep(1.1 / n, n) # arbitrary
  }else{
    if(!(class(effort) %in% c("numeric", "integer"))){
      stop("effort must be numeric")
    }
    if(length(effort) > 1){
      if(length(effort) != n){
        stop("length(effort) != length(reviewers)")
      }else{
        effort_out <- effort
      }
    }else{ # i.e. length(effort) == 1
      effort_out <- rep(effort, n)
    }
  }

  # ensure the person with the most effort is placed first - probably not necessary
  # if(all(!is.na(effort_out))){
  #   effort_order <- order(effort_out, decreasing = TRUE)
  #   effort_out <- effort_out[effort_order]
  #   names <- names[effort_order]
  # }

  result <- list(
    n = n,
    names = names,
    effort = effort_out,
    seq = seq_len(n)
  )
  class(result) <- "reviewer_info"
  return(result)
}


# function to divide up effort among many reviewers
allocate_reviewer_effort <- function(
  reviewers, # can be one of
    # single number (of reviewers)
    # string (their reviewers)
  effort, # either a single number of the proportion of effort per reviewer,
    # or a vector of proportions
  proportion_checked,
    # proportion of articles checked by two or more reviewers
  max_reviewers = 3, # i.e. most people to review a single document
  quiet = TRUE
){
  # catch errors
  if(missing(reviewers)){
    stop("'reviewers' is missing, with no default")
  }
  if(missing(effort)){
    effort <- NULL
  }
  if(!missing(proportion_checked)){
    if(proportion_checked < 0 | proportion_checked > 1){
      stop("proportion_checked should be between 0 and 1")
    }
    if(proportion_checked < 0.001){
      max_reviewers <- 1
    }
  }
  rev <- get_clean_reviewers(reviewers, effort)

  # create matrix for calculating sums
  optimize_df <- expand.grid(
    split(rep(c(0, 1), rev$n), rep(seq_len(rev$n), each = 2))
  )[-1, ]
  colnames(optimize_df) <- rev$names
  optimize_matrix <- t(as.matrix(optimize_df))
  formula_sums <- rev$effort

  if(!missing(max_reviewers)){
    keep_cols <- apply(optimize_matrix, 2, sum) <= max_reviewers
    optimize_matrix <- optimize_matrix[, keep_cols]
    optimize_df <- optimize_df[keep_cols, ]
  }
  estimate_n <- ncol(optimize_matrix)

  # work out exact case if effort is equal
  if(max(rev$effort) - min(rev$effort) < 0.01){
    if(missing(proportion_checked)){ # equal effort
      optimize_df$proportion <- 1 / estimate_n
    }else{
      multi_rows <- apply(optimize_matrix, 2, sum) > 1
      optimize_df$proportion <- 0
      optimize_df$proportion[multi_rows] <- proportion_checked / length(which(multi_rows))
      optimize_df$proportion[!multi_rows] <- (1 - proportion_checked) / length(which(!multi_rows))
    }
  }else{ # use optim
    if(!missing(proportion_checked)){
      optimize_matrix <- rbind(optimize_matrix,
        as.numeric(apply(optimize_df, 1, sum) > 1)
      )
      formula_sums <- c(formula_sums, proportion_checked)
    }
    # optimize
    # note that it should be possible to make a better estimate of par
    # e.g. for two reviewers the solution is trivial in most cases
    result <- stats::optim(
      par = rep(0.5, estimate_n),
      fn = function(a, x, n, sums){
        z <- a * x
        z <- z/sum(z[n, ]) # only reviewer times should sum; not proportions
        row_result <- sums - apply(z, 1, sum)
        sum(row_result^2)
      },
      x = optimize_matrix,
      sums = formula_sums,
      n = rev$seq,
      method = "L-BFGS-B",
      lower = 0,
      upper = 2,
      control = list(maxit = 10^5)
    )
    # note: consider changing weights between % allocated and researcher weights;
      # current approach means that researchers get n-1 times as much weight.
      # as a result the proportion_checked argument doesn't strongly affect the result
    if(result$convergence > 0){
      stop("unable to optimize matrix")
    }
    optimize_df$proportion <- result$par / sum(result$par)
  }

  # if rounding applied to optimize_df$proportion:
  # if((1- sum(optimize_df$proportion))^2 > 0){
  #   rounding_error <- sum(optimize_df$proportion) - 1
  #   highest_val <- which.max(optimize_df$proportion)
  #   optimize_df$proportion[highest_val] <- optimize_df$proportion[highest_val] - rounding_error
  # }

  if(!quiet){
    nums <- apply(t(optimize_matrix) * optimize_df$proportion, 2, sum)
    reviewer_count <- apply(optimize_matrix[rev$seq ,], 2, sum)
    reviewer_prop <- as.data.frame(xtabs(optimize_df$proportion ~ reviewer_count))
    reviewer_prop$reviewer_count <- paste0(reviewer_prop$reviewer_count, " people")
    if(any(reviewer_prop$reviewer_count == "1 people")){
      row <- which(reviewer_prop$reviewer_count == "1 people")
      reviewer_prop$reviewer_count[row] <- "1 person"
    }
    nums <- nums[rev$seq]
    if(all(rev$names == rev$seq)){
      name_text <- paste0("reviewer ", names(nums))
    }else{
      name_text <- names(nums)
    }
    cat(
      paste0(
        "Proportion of articles per reviewer:\n",
        paste(name_text, nums, sep = ": ", collapse = "\n"),
        "\nProportion of articles reviewed by:\n",
        paste(reviewer_prop$reviewer_count, reviewer_prop$Freq, sep = ": ", collapse = "\n"),
        "\n"
      )
    )
  }

  return(optimize_df)
}


# function to create_reviewer_matrix to a real dataset
# and optionally output files
distribute_tasks <- function(
  data, # a dataset to split
  reviewers, # either a data.frame as returned by create_reviewer_matrix,
    # or a value that can be passed to create_reviewer_matrix::reviewers
  write_csv = TRUE, # TRUE or FALSE
  file_name = "reviewer.csv", # note that reviewer_name will be appended after this for each instance
  return_data = FALSE, # should a list be returned giving the data split as in source files?
  ... # extra info passed to allocate_reviewer_effort
){
  # error catching
  if(missing(data)){
    stop("'data' is missing, with no default")
  }
  if(class(data) != "data.frame"){
    data <- as.data.frame(data)
  }
  # catch errors
  if(missing(reviewers)){
    stop("'reviewers' is missing, with no default")
  }

  # catch errors in reviewer_matrix
  if(class(reviewers) != "data.frame"){
    reviewer_data <- allocate_reviewer_effort(reviewers, ...)
  }else{
    if(colnames(reviewers)[ncol(reviewers)] != "proportion"){
      stop("reviewers is a data.frame, but final column is not named 'proportion'")
    }
    reviewer_data <- reviewers
  }

  # basic info
  data_n <- nrow(data)
  reviewer_matrix <- reviewer_data[, -ncol(reviewer_data)]
  reviewer_names <- colnames(reviewer_matrix)
  reviewer_n <- length(reviewer_names)

  # ensure rounding errors don't lead to missed refs
  article_counts <- round(reviewer_data$proportion * data_n)
  count_sum <- sum(article_counts)
  if(count_sum < data_n){
    row <- which.max(article_counts)
    article_counts[row] <- article_counts[row] + data_n - count_sum
  }

  # now make into string of numbers
  n_rows <- seq_len(length(article_counts))
  article_vector <- unlist(lapply(
    n_rows,
    function(a, x){rep(a, x[a])},
    x = article_counts
  ))
  # the use these numbers to look up which rows relate to each reviewer
  article_list <- lapply(
    reviewer_matrix,
    function(a, x){
      as.numeric(x %in% which(a > 0))
    },
    x = article_vector
  )
  # convert to data.frame, sort randomly
  reviewer_dframe <- as.data.frame(article_list)[order(rnorm(data_n)), ]
  colnames(reviewer_dframe) <- reviewer_names

  # use reviewer_dframe to create a list of data subsets
  result <- lapply(reviewer_dframe, function(a, x){
    x[which(a > 0), ]
    },
    x = data
  )

  if(write_csv){
    file_name <- sub(".csv$", "", file_name, perl = TRUE)
    invisible(lapply(names(result), function(a, x, file){
      write.csv(
        x[[a]],
        file = paste0(file, "_", a, ".csv"),
        row.names = FALSE
      )
    },
    x = result,
    file = file_name
  ))
  }

  if(return_data){
    invisible(return(result))
  }

}


# function to bring datasets back together
aggregate_tasks <- function(
  file_names, # vector or list of file names
  match_column, # column to join data on (unique ID)
  selection_column, # column showing selections from screening
  reviewer_names # optional list of reviewer names.
){
  # import data
  n <- length(file_names)
  n_seq <- seq_len(n)
  data_list <- lapply(file_names, revtools_csv)

  # catch errors in columns specs, but also choose defaults to match revtools standard practice
  col_names <- unique(unlist(lapply(data_list, colnames)))
  if(missing(match_column)){
    if(any(col_names == "label")){
      match_column <- "label"
    }else{
      stop("unable to identify match_column")
    }
  }else{
    if(!(match_column %in% col_names)){
      stop(paste0(match_column, " is not a valid column name in the specified files"))
    }
  }
  # ditto for data selection
  if(missing(selection_column)){
    if(any(col_names == "selected")){
      selection_column <- "selected"
    }else{
      stop("unable to identify selection_column")
    }
  }else{
    if(!(selection_column %in% col_names)){
      stop(paste0(selection_column, " is not a valid column name in the specified files"))
    }
  }
  # and reviewer names
  default_reviewer_names <- paste0("reviewer_", seq_len(length(file_names)))
  if(missing(reviewer_names)){
    reviewer_names <- default_reviewer_names
  }else{
    if(length(reviewer_names) != length(file_names)){
      reviewer_names <- default_reviewer_names
    }
  }
  reviewer_names <- paste0("results_", reviewer_names)

  # merge into a single data.frame and label each row with a file index
  data_all <- do.call(rbind, data_list)
  data_all$source_file <- unlist(lapply(n_seq, function(a, n){
    rep(a, n[[a]])
  }, n = lapply(data_list, nrow)
  ))

  # split by file index and look up all results for each file
  data_split <- split(data_all, data_all[[match_column]])
  data_split <- lapply(data_split, function(a, sel_col, rev_names){
    data_tr <- matrix(data = NA, ncol = length(reviewer_names), nrow = 1)
    colnames(data_tr) <- reviewer_names
    data_tr[1, a[["source_file"]]] <- a[[sel_col]]
    result <- cbind(
      a[1, !colnames(a) %in% c("source_file", sel_col) ],
      as.data.frame(data_tr)
    )
  },
  sel_col = selection_column,
  rev_names = reviewer_names
  )
  data_final <- do.call(rbind, data_split) # merge back to a single file
  return(data_final)
}