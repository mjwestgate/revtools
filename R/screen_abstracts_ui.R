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
        selectInput(
          inputId = "order",
          label = "Order citations by:",
          choices = list(
            "Random" = "order_random",
            "Input" = "order_initial",
            "Alphabetical" = "order_alphabetical",
            "User-defined" = "order_selected"
          )
        ),
        uiOutput("column_selector"),
        actionButton(
          inputId = "order_result_go",
          label = "Re-order",
          width = "85%"
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
    # fluidRow(
    #   column(width = 1),
    #   column(
    #     width = 10,
    #     uiOutput(outputId = "render_notes")
    #   )
    # )
  )

  return(
    list(
      header = header,
      sidebar = sidebar,
      body = body
    )
  )

}