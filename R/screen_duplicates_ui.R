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
            "fuzzdist" = "fuzzdist",
            "exact" = "exact"
          ),
          selected = "fuzzdist"
        ),
        uiOutput("algorithm_selector"),
        uiOutput("threshold_selector"),
        selectInput(
          inputId = "match_lower",
          label = "Make lower case?",
          choices = c(FALSE, TRUE)
        ),
        selectInput(
          inputId = "match_punctuation",
          label = "Remove punctuation?",
          choices = c(FALSE, TRUE)
        ),
        shiny::br(),
        actionButton(
          inputId = "calculate_duplicates",
          label = "Calculate Duplicates"
        ),
        shiny::br()
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    fluidRow(
      column(
        width = 6,
        tableOutput("match_summary")
      ),
      column(
        width = 2,
        uiOutput("selector_previous"),
        br()
      ),
      column(
        width = 2,
        uiOutput("selector_none"),
        br()
      ),
      column(
        width = 2,
        uiOutput("selector_next"),
        br()
      )
    ),
    fluidRow(
      column(
        width = 6,
        uiOutput("selector_1"),
        br(),
        tableOutput("text_1")
      ),
      column(
        width = 6,
        uiOutput("selector_2"),
        br(),
        tableOutput("text_2")
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