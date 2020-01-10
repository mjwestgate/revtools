screen_duplicates_ui <- function(){

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
        uiOutput("data_selector"),
        uiOutput("response_selector"),
        menuItem("Grouping Variable(s)",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = FALSE,
          uiOutput("group_selector")
        ),
        br(),
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
      menuItem("Matching",
        icon = shiny::icon("clone"),
        startExpanded = FALSE,
        selectInput(
          inputId = "match_function",
          label = "Select function",
          choices = c(
            "stringdist" = "stringdist",
            "fuzzdist" = "fuzzdist",
            "exact" = "exact"
          ),
          selected = "exact"
        ),
        uiOutput("algorithm_selector"),
        uiOutput("threshold_selector"),
        checkboxInput(
          inputId = "match_lower",
          label = "Make lower case?",
          value = FALSE
        ),
        checkboxInput(
          inputId = "match_punctuation",
          label = "Remove punctuation?",
          value = FALSE
        ),
        actionButton(
          inputId = "calculate_duplicates",
          label = "Calculate Duplicates",
          width = "85%"
        ),
        br()
      ),
      menuItem(
        text = "Display",
        icon = icon("bar-chart-o"),
        checkboxInput(
          inputId = "author_line_breaks",
          label = "Add line breaks to author data?",
          value = FALSE
        ),
        uiOutput("display_selector"),
        br()
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    # fluidRow(
    #   # column(
    #   #   width = 6,
    #   #   tableOutput("match_summary")
    #   # ),
    #   column(
    #     width = 2,
    #     uiOutput("selector_previous"),
    #     br()
    #   ),
    #   column(
    #     width = 2,
    #     uiOutput("selector_none"),
    #     br()
    #   ),
    #   column(
    #     width = 2,
    #     uiOutput("selector_next"),
    #     br()
    #   )
    # ),
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