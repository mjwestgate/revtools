save_modal <- function(
  x # typically data$raw
  ){
  if(is.null(x)){
    save_error_modal()
  }else{
    save_proceed_modal()
  }
}
#

save_proceed_modal <- function(){
  showModal(
    modalDialog(
      textInput("save_filename",
        label = "File Name"
      ),
      selectInput("save_data_filetype",
        label = "File Type",
        choices = c("csv", "rds")
      ),
      actionButton("save_data_execute", "Save"),
      modalButton("Cancel"),
      title = "Save As",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

save_error_modal <- function(){
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

abstract_complete_modal <- function(){
  showModal(
    modalDialog(
      HTML(
        "All articles have been screened. Would you like to save your progess?<br><br>
        <i>If you have specified an object in your workspace and click 'Exit App',
        your progress will be invisibly saved to that object.</i><br><br>"
      ),
      textInput("save_filename",
        label = "File Name"
      ),
      selectInput("save_data_filetype",
        label = "File Type",
        choices = c("csv", "rds")
      ),
      actionButton(
        inputId = "save_data_execute",
        label = "Save to File"
      ),
      actionButton(
        inputId = "exit_app_confirmed",
        label = "Save to Workspace"
      ),
      modalButton("Cancel"),
      title = "Save As",
      footer = NULL,
      easyClose = FALSE
    )
  )
}