screen_topics_ui <- function(){

  # build user interface
  header <- shinydashboard::dashboardHeader(
    tag("li",
      list(
        class = "dropdown",
        div(
          style = "
            display: inline-block;
            vertical-align: top;
            text-align: right;
            width: 780px",
          textOutput(outputId = "progress_text")
        )
      )
    ),
    title = plotOutput("header")
  )

  sidebar <- shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Data",
        icon = shiny::icon("table"),
        startExpanded = TRUE,
        fileInput(
          inputId = "data_in",
          label = "Import",
          multiple = TRUE
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
        br(),
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
      menuItem(
        text = "Model",
        icon = icon("calculator"),
        sliderInput(
          inputId = "min_freq",
          label = "Lower threshold for removing rare terms (%):",
          min = 0,
          max = 5,
          step = 0.5,
          value = 1
        ),
        sliderInput(
          inputId = "max_freq",
          label = "Upper threshold for removing common terms (%):",
          min = 80,
          max = 100,
          step = 1,
          value = 85
        ),
        selectInput(
          inputId = "model_type",
          label = "Model type",
          choices = c("LDA", "CTM")
        ),
        sliderInput(
          inputId = "n_topics",
          label = "Number of topics",
          min = 4,
          max = 30,
          step = 1,
          value = 5
        ),
        sliderInput(
          inputId = "n_iterations",
          label = "Number of iterations",
          min = 2000,
          max = 30000,
          step = 2000,
          value = 10000
        ),
        actionButton(
          inputId = "calc_model",
          label = "Calculate Model",
          width = "85%"
        ),
        br()
      ),
      menuItem(
        text = "Display",
        icon = icon("bar-chart-o"),
        br(),
        HTML(
          "<b>&nbsp&nbspSelect display type:</b>"
        ),
        menuItem(
          text = "Show entries",
          tabName = "entries",
          selected = TRUE
        ),
        menuItem(
          text = "Show words",
          tabName = "words"
        ),
        selectInput(
          inputId = "hide_names",
          label = "Hide identifying information?",
          choices = c(TRUE, FALSE),
          multiple = FALSE
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
          label = "Point size",
          min = 0,
          max = 20,
          step = 2,
          value = 12
        )
      )
    )
  )

  body <- shinydashboard::dashboardBody(
    revtools_css(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "entries",
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
              outputId = "plot_topics",
              height = "450px"
            ),
            tableOutput(
              outputId = "selector_text"
            ),
            uiOutput(
              outputId = "select_choice"
            ),
            uiOutput(
              outputId = "render_notes"
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "words",
        fluidRow(
          column(
            width = 8,
            plotlyOutput(
              outputId = "plot_words",
              height = "600px"
            ),
            br(),
            splitLayout(
              HTML("Search: "),
              textInput(
                inputId = "search_text",
                label = NULL
              ),
              cellWidths = c("10%", "90%")
            ),
            tableOutput(
              outputId = "search_results"
            ),
            div(
              id = "search_placeholder"
            )
          ),
          column(
            width = 4,
            tableOutput(
              outputId = "selector_n2"
            ),
            plotlyOutput(
              outputId = "plot_topics_2",
              height = "450px"
            ),
            br(),
            uiOutput(
              outputId = "word_selector"
            )
          )
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