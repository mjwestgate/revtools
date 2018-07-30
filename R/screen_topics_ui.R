screen_topics_ui <- function(){

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
        menuItem("Variables",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = TRUE,
          uiOutput("variable_selector")
        ),
        shiny::br()
      ),
      menuItem("Model",
        icon = icon("calculator"),
        selectInput("model_type",
          label = "Model Type",
          choices = c("LDA", "CTM")
        ),
        sliderInput("n_topics",
          label = "# Topics",
          min = 4,
          max = 30,
          step = 1,
          value = 5
        ),
        sliderInput("n_iterations",
          label = "# Iterations",
          min = 1000,
          max = 20000,
          step = 1000,
          value = 2000
        ),
        actionButton("calc_model",
          label = "Calculate Model",
          width = "80%"
        ),
        shiny::br()
      ),
      menuItem("Plot",
        icon = icon("bar-chart-o"),
        selectInput("hide_names",
          label = "Hide Identifying Information?",
          choices = c("FALSE", "TRUE"),
          multiple = FALSE
        ),
        selectInput("plot_type",
          label = "Display",
          choices = c(articles = "x", words = "y")
        ),
        selectInput("plot_dims",
          label = "Dimensions",
          choices = c("2D", "3D")
        )
      ),
      menuItem("Appearance",
        icon = icon("paint-brush"),
        selectInput("palette",
          label = "Palette",
          choices = c(
            Magma = "A",
            Viridis = "D",
            Inferno = "B",
            Plasma = "C",
            Cividis = "E"
          ),
          multiple = FALSE
        ),
        sliderInput("color_alpha",
          label = "Opacity",
          min = 0.2,
          max = 1,
          step = 0.1,
          value = 0.9
        ),
        sliderInput("color_hue",
          label = "Hue",
          min = 0,
          max = 1,
          step = 0.05,
          value = c(0.1, 0.9)
        ),
        sliderInput("point_size",
          label = "Point Size",
          min = 0,
          max = 20,
          step = 2,
          value = 12
        )
      ),
      menuItem("Save", # use modal save in this version
        icon = icon("save"),
        actionButton("save_data",
          label = "Save Data",
          width = "80%"
        ) # ,
        # shiny::actionButton("save_scatter",
        #   label = "Save Main Plot",
        #   width = "80%",
        # ),
        # shiny::actionButton("save_bar",
        #   label = "Save Barplot",
        #   width = "80%",
        # )
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    fluidRow(
      column(
        width = 8,
        plotlyOutput("plot_main", height = "600px"),
        tableOutput("abstract_text")
      ),
      column(
        width = 4,
        plotlyOutput("plot_topic", height = "450px"),
        tableOutput("selector_text"),
        uiOutput("select_choice"),
        uiOutput("select_notes"),
        uiOutput("select_save")
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