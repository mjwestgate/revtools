load_title_data <- function(data){

  x <- list(
    data = list(
      raw = NULL,
      current = NULL,
      n_current = NULL,
      n_previous = NULL
    ),
    selector = list(
      yes = c(0),
      no = c(0),
      maybe = c(0)
    )
  )

  if(!is.null(data)){
    # throw a warning if a known file type isn't given
    accepted_inputs <- c("bibliography", "data.frame")
    if(!any(accepted_inputs == class(data))){
      stop("only classes 'bibliography' or 'data.frame' accepted by screen_titles")}
    if(class(data) == "bibliography"){
      data <- as.data.frame(data)
    }

    # create citation
    if(!any(colnames(data) == "citation")){
      data$citation <- format_citation(
        data = data,
        details = FALSE,
        add_html = TRUE
      )
    }
    # add extra columns as needed
    if(!any(colnames(data) == "selected")){data$selected <- NA}
    if(!any(colnames(data) == "notes")){data$notes <- NA}
    if(!any(colnames(data) == "color")){data$color <- "#000000"}

    # save progress
    x$data$raw <- data
    x$data$n_current <- min(c(
      8,
      length(which(is.na(data$selected)))
    ))
    x$data$current <- seq_len(x$data$n_current)
    x$data$n_previous <- 8
    rep_zeroes <- rep(0, x$data$n_current)
    x$selector$yes <- rep_zeroes
    x$selector$no <- rep_zeroes
    x$selector$maybe <- rep_zeroes

  }

  return(x)
}


# function to add a single title + selector buttons to the ui
add_reference_ui <- function(
  entry_number, # an index to record which entry these data are linked to
  ui_selector # i.e. where in the UI should this go? Starts with a #
){
  insertUI(
    selector = paste0(  # formerly '#placeholder',
      "#", ui_selector
    ),
    ui = div(
      list(
        br(),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 10px",
          HTML("<br>")
        ),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 80px",
          actionButton(
            inputId = paste0(
              "citation_",
              entry_number,
              "_yes"
            ),
            label = "Select",
            style = "
              width: 80px;
              background-color: #7c93c1;
              color: #fff;"
          )
        ),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 80px",
          actionButton(
            inputId = paste0(
              "citation_",
              entry_number,
              "_no"
            ),
            label = "Exclude",
            style = "
              width: 80px;
              background-color: #c17c7c;
              color: #fff;"
          )
        ),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 80px",
          actionButton(
            inputId = paste0(
              "citation_",
              entry_number,
              "_maybe"
            ),
            label = "Unknown",
            style = "
              width: 80px;
              background-color: #adadad;
              color: #fff;"
          )
        ),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 10px",
          HTML("<br>")
        ),
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            width: 700px",
          tableOutput(
            outputId = paste0(
              "citation_",
              entry_number,
              "_render"
            )
          )
        )
      ),
      id = paste0(
        'citation_',
        entry_number
      )
    )
  )
}

# create a data.frame of the names & values of actionButtons
# that match a particular regex
input_tracker <- function(input, string){
  object_check <- grepl(
    string,
    names(input),
    perl = TRUE
  )
  object_names <- names(input)[which(object_check)]
  result <- data.frame(
    name = object_names,
    id = as.integer(unlist(lapply(
      strsplit(object_names, "_"),
      function(a){a[2]}
    ))),
    value = unlist(lapply(
      object_names,
      function(a){input[[a]]}
    )),
    stringsAsFactors = FALSE
  )
  result <- result[order(result$id), ]
  return(result)
}