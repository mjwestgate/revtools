screen_abstracts_ui <- function(){

  # build user interface
  header <- shinydashboard::dashboardHeader(
    title = plotOutput("header")
  )

  sidebar <- shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data",
        icon = shiny::icon("bar-chart-o"),
        startExpanded = TRUE,
        fileInput("data_in", label = "Import"),
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
        selectInput(
          inputId = "order",
          label = "Order citations by:",
          choices = list(
            "Random" = "order_random",
            "Input" = "order_initial",
            "Alphabetical" = "order_alphabetical"
          )
        ),
        selectInput("hide_names",
          label = "Hide identifying information?",
          choices = c("TRUE", "FALSE"),
          multiple = FALSE
        )
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 8,
        tableOutput("citation")
      ),
      column(
        width = 1
      ),
      column(
        width = 2,
        uiOutput(outputId = "selector_buttons"),
        uiOutput(outputId = "render_notes"),
        tableOutput(outputId = "progress_text")
      )
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