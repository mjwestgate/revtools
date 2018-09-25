format_duplicates <- function(
  x,
  columns,
  breaks
){
  if(missing(columns)){
    columns <- names(x)
  }
  if(breaks){
    collapse_val <- "<br>&nbsp;&nbsp;&nbsp;"
  }else{
    collapse_val <- "; "
  }

  # show data only for selected columns
  x <- x[which(names(x) %in% columns)]

  # remove columns that contain no data for this entry
  if(any(is.na(x))){
    x <- x[which(!is.na(x))]
  }
  # expand out author names
  if(any(names(x) == "author")){
    value <- which(names(x) == "author")
    x[[value]] <- paste(
      strsplit(x[[value]], split = " and ")[[1]],
      collapse = collapse_val
    )
  }
  # expand out keywords
  if(any(names(x) == "keywords")){
    value <- which(names(x) == "keywords")
    x[[value]] <- paste(
      strsplit(x[[value]], split = " and ")[[1]],
      collapse = "; "
    )
  }
  # title case column names
  labels <- tools::toTitleCase(names(x))

  cat(
    paste(
      unlist(lapply(
        labels,
        function(a){paste0("<b>", a, ":</b> ")}
      )),
      x,
      collapse = "<br>"
    )
  )
}

save_modal <- function(
  x, # typically data$raw
  title = "Save As"
  ){
  if(is.null(x)){
    showModal(
      modalDialog(
        HTML(
          "Import some data to begin<br><br>
          <em>Click anywhere to exit</em>"
        ),
        title = "Error: No data to save",
        footer = NULL,
        easyClose = TRUE
      )
    )
  }else{
    showModal(
      modalDialog(
        selectInput(
          inputId = "save_type",
          label = "Save As",
          choices = c("csv", "rds"),
          multiple = FALSE
        ),
        textInput(
          inputId = "save_filename",
          label = "File Name"
        ),
        actionButton(
          inputId = "save_data_execute",
          label = "Save"
        ),
        modalButton("Cancel"),
        title = title,
        footer = NULL,
        easyClose = FALSE
      )
    )
  }
}

error_modal <- function(text){
  showModal(
    modalDialog(
      HTML(text),
      title = "Error: insufficient data",
      footer = NULL,
      easyClose = TRUE
    )
  )
}

calculating_modal <- function(){
  showModal(
    modalDialog(
      HTML("Depending on the size of your dataset, this may take some time"),
      title = "Calculating duplicates",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

no_duplicates_modal <- function(){
  showModal(
    modalDialog(
      HTML("Click anywhere to exit"),
      title = "No duplicates found",
      footer = NULL,
      easyClose = TRUE
    )
  )
}