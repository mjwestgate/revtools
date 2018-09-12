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