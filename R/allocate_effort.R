#' Determine optimal way to divide articles among 2 or more reviewers
#'
#' This function takes information on the number (and optionally, identity) of
#' reviewers and combines it with data on how it should be allocated to return
#' a data.frame showing the proportion of articles to be assessed by each
#' person.
#'
#' This function makes an attempt to return a sensible distribution of effort
#' among a number of reviewers. If effort is given as a single value (or not
#' provided at all), then the result is calculated exactly such that all
#' proportions sum to 1. Conversely, if effort is given as a numeric vector of
#' length >1 and contains a range of values, then the function tries to
#' optimize the proportion of articles allocated to each person, while also
#' matching constrains given by \code{proportion_checked} and
#' \code{max_reviewers}. In this case, and depending on the inputs, it is
#' possible that no perfect solution will exist, meaning that some reviewers
#' may be allocated a higher proportion of articles than anticipated. For this
#' reason it is often worth setting \code{quiet = FALSE} when running
#' \code{allocate_effort} with variation in reviewer effort, to allow
#' assessment of the results.
#'
#' @param reviewers Either an integer giving the number of reviewers, or a
#' vector of strings giving reviewer names.
#' @param effort Either a single number giving the proportion of articles to be
#' reviewed by all reviewers, or a numeric vector giving a unique proportion
#' for each reviewer. If the latter must be consistent with number given by
#' 'reviewers' above.
#' @param proportion_checked Numeric value giving the proportion of entries
#' that should be screened by two or more reviewers.
#' @param min_reviewers the minimum number of reviewers that should screen any
#' single article. Useful for ensuring articles are checked multiple times.
#' @param max_reviewers the maximum number of reviewers that should screen any
#' single article. Useful for avoiding redundancy when working with large
#' teams.
#' @param precision Number of decimal places with which to report results.
#' Defaults to 2.
#' @param treatments Optional number of treatments that reviewers should be
#' split into. Ensures that the same article is never reviewed twice in the
#' same treatment or by the same reviewer. Currently experimental.
#' @param quiet Logical - should the function return a summary of the
#' proportion of articles allocated to each reviewer? Defaults to TRUE.
#' @return Invisibly returns a data.frame giving one column per reviewer plus
#' an extra column of proportions. The reviewer columns are binary and show
#' which proportions apply to each person or combination of people.
#' @seealso \code{\link{distribute_tasks}} for how to use the output of
#' \code{allocate_effort} to split a dataset.
#' @examples
#'
#' # import some data
#' file_location <- system.file(
#'   "extdata",
#'   "avian_ecology_bibliography.ris",
#'   package = "revtools")
#' x <- read_bibliography(file_location)
#' # simple case - split evenly among 4 reviewers
#' result <- allocate_effort(4, quiet = TRUE)
#' # more complex - specify names and amount of overlap
#' result <- allocate_effort(
#'   reviewers = c("john", "paul", "george", "ringo"),
#'   proportion_checked = 0.2,
#'   max_reviewers = 3,
#'   quiet = TRUE
#'   )
#' # most complex - specify uneven effort among reviewers (experimental)
#' # result <- allocate_effort(
#' #   reviewers = 4,
#' #   effort = c(0.9, 0.7, 0.5, 0.3),
#' #   max_reviewers = 3,
#' #   quiet = TRUE
#' #   )
#'
#' @export allocate_effort
allocate_effort <- function(
  reviewers, # can be one of
    # single number (of reviewers)
    # string (their reviewers)
  effort, # either a single number of the proportion of effort per reviewer,
    # or a vector of proportions
  proportion_checked,
    # proportion of articles checked by two or more reviewers
  max_reviewers = 3, # i.e. most people to review a single document
  precision = 2, # number of decimal places to report result to
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
      optimize_df$proportion <- round_preserve_sum(1 / estimate_n, precision)
    }else{
      multi_rows <- optimize_df$n > 1
      proportion <- rep(0, nrow(optimize_df))
      proportion[multi_rows] <- proportion_checked / length(which(multi_rows))
      proportion[!multi_rows] <- (1 - proportion_checked) / length(which(!multi_rows))
      optimize_df$proportion <- round_preserve_sum(proportion, precision)
    }
  }else{ # use optim
    if(!missing(proportion_checked)){
      optimize_matrix <- rbind(optimize_matrix,
        as.numeric(apply(optimize_df, 1, sum) > 1)
      )
      formula_sums <- c(formula_sums, proportion_checked)
    }
    # optimize
    optim_fun <- function(a, x, n, sums){
      a_bin <- plogis(a)
      a_sum <- a_bin / sum(a_bin)
      z <- rep(a_sum, each = nrow(x)) * x
      row_result <- (sums - apply(z, 1, sum))
      if(nrow(x) > n){ # add extra weight to proportion_checked row
        weight_rows <- c((n + 1) : nrow(x))
        row_result[weight_rows] <- row_result[weight_rows] * n
      }
      sum(row_result^2)
    }
    result <- stats::optim(
      par = rep(1, estimate_n),
      fn = optim_fun,
      x = optimize_matrix,
      sums = formula_sums,
      n = length(rev$seq),
      method = "L-BFGS-B",
      lower = -5,
      upper = 5,
      control = list(maxit = 10^5)
    )
    if(result$convergence > 0){
      stop("unable to optimize matrix")
    }
    par_t <- plogis(result$par)
    par_f <- par_t / sum(par_t)
    optimize_df$proportion <- round_preserve_sum(par_f, precision)
  }

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


# take inputs to allocate_effort and return a clean list of parameters
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

  result <- list(
    n = n,
    names = names,
    effort = effort_out,
    seq = seq_len(n)
  )
  class(result) <- "reviewer_info"
  return(result)
}


# function for rounding while preserving sum of original vector
# original code from http://biostatmatt.com/archives/2902
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}