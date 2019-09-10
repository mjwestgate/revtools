screen_abstracts_ui <- function(){

  # build user interface
  header <- shinydashboard::dashboardHeader(
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
          inputId = "save_data",
          label = "Save Data",
          width = "85%"
        ),
        actionButton(
          inputId = "clear_data",
          label = "Clear Data",
          width = "85%"
        ),
        actionButton(
          inputId = "exit_app",
          label = "Exit App",
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
        uiOutput(outputId = "render_notes")
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