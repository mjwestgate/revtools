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
      title = "No were duplicates found using your search parameters",
      footer = NULL,
      easyClose = TRUE
    )
  )
}

clear_data_modal <- function(){
  showModal(
    modalDialog(
      HTML("If you proceed, all data will be removed from this window,
      including any progress you have made screening your data.
      If you have not saved your data,
      you might want to consider doing that first.<br><br>
      Are you sure you want to continue?<br><br>"
      ),
      actionButton(
        inputId = "clear_data_confirmed",
        label = "Confirm"),
      modalButton("Cancel"),
      title = "Clear all data",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

exit_modal <- function(){
  showModal(
    modalDialog(
      HTML("If you proceed, this app will close.
      If you have specified an object in your workspace, your progress will be invisibly saved to that object; otherwise your progress will be lost.<br><br>
      Are you sure you want to continue?<br><br>"
      ),
      actionButton(
        inputId = "exit_app_confirmed",
        label = "Confirm"),
      modalButton("Cancel"),
      title = "Exit App",
      footer = NULL,
      easyClose = FALSE
    )
  )
}