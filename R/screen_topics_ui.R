screen_topics_ui <- function(){

  # build user interface
  header <- shinydashboard::dashboardHeader(
    title = plotOutput(outputId = "header")
  )

  sidebar <- shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Data",
        icon = shiny::icon("bar-chart-o"),
        startExpanded = TRUE,
        fileInput(
          inputId = "data_in",
          label = "Import"
        ),
        uiOutput(
          outputId = "response_selector"
        ),
        menuItem(
          text = "Variables",
          tabName = "variable_tab",
          icon = icon("pencil"),
          startExpanded = TRUE,
          uiOutput(
            outputId = "variable_selector"
          )
        ),
        shiny::br(),
        actionButton(
          inputId = "clear_data",
          label = "Clear All Data",
          width = "80%"
        ),
        shiny::br()
      ),
      menuItem(
        text = "Model",
        icon = icon("calculator"),
        selectInput(
          inputId = "model_type",
          label = "Model Type",
          choices = c("LDA", "CTM")
        ),
        sliderInput(
          inputId = "n_topics",
          label = "# Topics",
          min = 4,
          max = 30,
          step = 1,
          value = 5
        ),
        sliderInput(
          inputId = "n_iterations",
          label = "# Iterations",
          min = 1000,
          max = 20000,
          step = 1000,
          value = 2000
        ),
        actionButton(
          inputId = "calc_model",
          label = "Calculate Model",
          width = "80%"
        ),
        shiny::br()
      ),
      menuItem(
        text = "Plot",
        icon = icon("bar-chart-o"),
        selectInput(
          inputId = "hide_names",
          label = "Hide Identifying Information?",
          choices = c("FALSE", "TRUE"),
          multiple = FALSE
        ),
        selectInput(
          inputId = "plot_type",
          label = "Display",
          choices = c(articles = "x", words = "y")
        ),
        selectInput(
          inputId = "plot_dims",
          label = "Dimensions",
          choices = c("2D", "3D")
        )
      ),
      menuItem(
        text = "Appearance",
        icon = icon("paint-brush"),
        selectInput(
          inputId = "palette",
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
        sliderInput(
          inputId = "color_alpha",
          label = "Opacity",
          min = 0.2,
          max = 1,
          step = 0.1,
          value = 0.9
        ),
        sliderInput(
          inputId = "color_hue",
          label = "Hue",
          min = 0,
          max = 1,
          step = 0.05,
          value = c(0.1, 0.9)
        ),
        sliderInput(
          inputId = "point_size",
          label = "Point Size",
          min = 0,
          max = 20,
          step = 2,
          value = 12
        )
      ),
      menuItem(
        text = "Save", # use modal save in this version
        icon = icon("save"),
        actionButton(
          inputId = "save_data",
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
        plotlyOutput(
          outputId = "plot_main",
          height = "600px"
        ),
        tableOutput(
          outputId = "abstract_text"
        )
      ),
      column(
        width = 4,
        plotlyOutput(
          outputId = "plot_topic",
          height = "450px"
        ),
        tableOutput(
          outputId = "selector_text"
        ),
        uiOutput(
          outputId = "select_choice"
        ),
        uiOutput(
          outputId = "select_notes"
        ),
        uiOutput(
          outputId = "select_save"
        )
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