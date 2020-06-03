#' Divide a set of articles among two or more reviewers
#'
#' A common task in systematic review is to divide a dataset of articles
#' located by a search (typically involving >1 databases) and distributing them
#' amongst a team of reviewers for screening. This function takes a dataset
#' divides it among the specified number of reviewers, returning the resulting
#' data.frames either in a list to the workspace, or (by default) as a set of
#' .csv files in the specified directory. The resulting files can be passed to
#' any of the screening functions provided by revtools, i.e.
#' \code{\link{screen_titles}}, \code{\link{screen_abstracts}}, or
#' \code{\link{screen_topics}}.
#'
#' The dataset is allocated each author in the proportion of articles specified
#' by \code{\link{allocate_effort}}, with the identity of articles passed to
#' reviewer being chosen by \code{rnorm}. As a result, this function is very
#' sensitive to the inputs provided to \code{\link{allocate_effort}}, so it is
#' often worth running that function first and checking the results to be
#' certain that effort is being distributed in a way that you are happy with.
#'
#' @param data a vector of strings
#' @param reviewers Either a \code{data.frame} as returned by
#' \code{allocate_effort}, an integer giving the number of reviewers, or a
#' vector of strings giving reviewer names.
#' @param write_csv Logical - should the function write a set of csv files (1
#' per reviewer)? Defaults to TRUE
#' @param file_name a file path & name showing where .csv files should be
#' saved. Ignored if \code{write_csv} is FALSE. Defaults to
#' 'reviewer_[name].csv'.
#' @param return_data Logical - should a list be (invisibly) returned, in which
#' each entry is the data sent to a single reviewer? Defaults to FALSE.
#' @param ... Further arguments passed to \code{allocate_effort}
#' @return Invisibly returns a list of data.frames, each with same columns as
#' \code{data} but containing only a subset of rows.
#' @seealso \code{\link{allocate_effort}} for a detailed description of how the
#' division among reviewers is accomplished.
#' @examples
#'
#' # import some data
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location)
#' result <- distribute_tasks(x, 4, write_csv = FALSE) # split evenly among 4 reviewers
#'
#' @export distribute_tasks

distribute_tasks <- function(
  data, # a dataset to split
  reviewers, # either a data.frame as returned by create_reviewer_matrix,
    # or a value that can be passed to create_reviewer_matrix::reviewers
  write_csv = TRUE, # TRUE or FALSE
  file_name = "reviewer.csv", # note that reviewer_name will be appended after this for each instance
  return_data = FALSE, # should a list be returned giving the data split as in source files?
  ... # extra info passed to allocate_effort
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
    reviewer_data <- allocate_effort(reviewers, ...)
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


#' Combine (potentially overlapping) article sets generated by screening among
#' a team of reviewers.
#'
#' A common task in systematic review is to divide a dataset of articles
#' located by a search (typically involving >1 databases) and distributing them
#' amongst a team of reviewers for screening. This function takes a dataset
#' divided using \code{link{distribute_tasks}} and recombines them into a
#' single \code{data.frame}.
#'
#'
#' @param file_names a vector or list of file paths used to locate screened
#' files. Must be in .csv format.
#' @param match_column The name of the column used to match identical
#' references. In revtools this is 'label', which is the default here.
#' @param selection_column The name of the column used to store 'selection'
#' data; i.e. which entries have been retained and which excluded. In revtools
#' this is 'selected', which is the default here.
#' @param reviewer_names Optional vector of names used to label the 'results'
#' columns in the resulting \code{data.frame}.
#' @return Returns a data.frame with one row per unique value of
#' \code{match_column}, showing the content of \code{selection_column} for each
#' reviewer.
#' @seealso \code{\link{distribute_tasks}} for the inverse problem of dividing
#' a single dataset amongst many reviewers.
#' @export aggregate_tasks
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
