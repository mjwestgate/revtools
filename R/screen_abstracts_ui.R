screen_abstracts_ui <- function(){

  # build user interface
  header <- dashboardHeader(
    tag("li",
      list(
        class = "dropdown",
        uiOutput("selector_bar")
      )
    ),
    title = plotOutput("header")
  )

  sidebar <- shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data",
        icon = shiny::icon("bar-chart-o"),
        startExpanded = TRUE,
        fileInput(
          inputId = "data_in",
          label = "Import",
          multiple = TRUE
        ),
        actionButton(
          inputId = "clear_data",
          label = "Clear Data",
          width = "85%"
        ),
        actionButton(
          inputId = "exit_app",
          label = "Save to Workspace",
          width = "85%"
        ),
        actionButton(
          inputId = "save_data",
          label = "Save to File",
          width = "85%"
        ),
        br()
      ),
      menuItem("Appearance",
        icon = icon("paint-brush"),
        selectInput("hide_names",
          label = "Hide identifying information?",
          choices = c("Yes" = "TRUE", "No" = "FALSE"),
          multiple = FALSE
        ),
        selectInput(
          inputId = "hide_screened",
          label = "Hide screened entries?",
          choices = c("Yes" = "TRUE", "No" = "FALSE"),
          multiple = FALSE
        ),
        selectInput(
          inputId = "order",
          label = "Order citations by:",
          choices = list(
            "Random" = "random",
            "Input" = "initial",
            "Alphabetical" = "alphabetical",
            "User-defined" = "user_defined"
          )
        ),
        uiOutput("column_selector"),
        actionButton(
          inputId = "order_result_go",
          label = "Re-order",
          width = "85%"
        ),
        br()
      ),
      menuItem("Add fields",
        icon = shiny::icon("plus"),
        br(),
        actionButton(
          inputId = "add_dropdown",
          label = "Add dropdown",
          width = "85%"
        ),
        br(),
        actionButton(
          inputId = "add_button",
          label = "Add button",
          width = "85%"
        ),
        br(),
        actionButton(
          inputId = "add_text",
          label = "Add text",
          width = "85%"
        ),
        br()
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        tableOutput("citation"),
        br(),
        br(),
        uiOutput(outputId = "render_notes_toggle"),
        uiOutput(outputId = "render_notes"),
        # shinyjqui::jqui_draggable(
        # # shinyjqui::jqui_resizable( # appears impossible to both resize and drag
        #   selectInput(
        #     inputId = "test",
        #     label = "test input",
        #     choices = c("yes", "no"),
        #     width = "200px"
        #   )
        # )
      ),
      column(width = 1)
    )
  )

  return(
    list(
      header = header,
      sidebar = sidebar,
      body = body
    )
  )

}