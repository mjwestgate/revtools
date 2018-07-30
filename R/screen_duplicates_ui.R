screen_duplicates_ui <- function(){

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
        uiOutput("response_selector"),
        menuItem("Grouping Variable(s)",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = FALSE,
          uiOutput("group_selector")
        )
      ),
      menuItem("Matching",
        icon = shiny::icon("clone"),
        startExpanded = FALSE,
        selectInput(
          inputId = "match_function",
          label = "Select function",
          choices = c(
            "stringdist" = "stringdist::stringdist",
            "fuzzdist" = "fuzzdist"
          ),
          selected = "fuzzdist"
        ),
        uiOutput("algorithm_selector"),
        uiOutput("threshold_selector")
      ),
      shiny::br(),
      actionButton(
        inputId = "calculate_duplicates",
        label = "Calculate Duplicates"
      ),
      shiny::br()
    )
  )

  body <- dashboardBody(
    revtools_css(),
    fluidRow(
      column(
        width = 6,
        h4("Returned Text"),
        br(),
        tableOutput("test_text")
      ),
      column(
        width = 6,
        strong("Other text")
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