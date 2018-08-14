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
        fileInput("data_in", label = "Import")
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
        width = 5,
        shiny::h4("Abstract Screening"),
        br(),
        strong("Citation"),
        br(),
        tableOutput("citation"),
        br(),
        radioButtons(
          inputId = "abstract_selector",
          label = "Selection",
          choices = c("Select", "Exclude"),
          inline = TRUE
        ),
        textAreaInput(
          inputId = "abstract_notes",
          label = "Notes",
          resize = "vertical",
          width = "100%",
          height = "150px"
        ),
        splitLayout(
          actionButton(
            inputId = "abstract_previous",
            label = "Previous",
            width = "80px",
            style = "background-color: #6b6b6b;"
          ),
          p(""),
          actionButton(
            inputId = "abstract_save",
            label = "Save",
            width = "80px"
          ),
          p(""),
          actionButton(
            inputId = "abstract_next",
            label = "Next",
            width = "80px",
            style = "background-color: #6b6b6b;"
          ),
          cellWidths = c("20%", "5%", "20%", "5%", "20%")
        )
      ),
      column(
        width = 7,
        strong("Abstract"),
        tableOutput("abstract_text")
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