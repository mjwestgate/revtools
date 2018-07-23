screen_visual_ui <- function(){

  # build user interface
  header <- shinydashboard::dashboardHeader(
    title = shiny::plotOutput("header")
  )

  # dropdownMenuOutput might work to render a plot drawn in server.R

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Data",
        icon = shiny::icon("bar-chart-o"),
        startExpanded = TRUE,
        shiny::fileInput("data_in", label = "Import"),
        shiny::uiOutput("response_selector"),
        shinydashboard::sidebarMenuOutput("variable_menu")
      ),
      shinydashboard::menuItem("Model",
        icon = shiny::icon("calculator"),
        shiny::selectInput("model_type",
          label = "Model Type",
          choices = c("LDA", "CTM")
        ),
        shiny::sliderInput("n_topics",
          label = "# Topics",
          min = 4,
          max = 30,
          step = 1,
          value = 5
        ),
        shiny::sliderInput("n_iterations",
          label = "# Iterations",
          min = 1000,
          max = 20000,
          step = 1000,
          value = 2000
        ),
        shiny::actionButton("calc_model",
          label = "Calculate Model",
          width = "80%"
        ),
        br()
      ),
      shinydashboard::menuItem("Plot",
        icon = shiny::icon("bar-chart-o"),
        shiny::selectInput("hide_names",
          label = "Hide authors etc?",
          choices = c("FALSE", "TRUE"),
          multiple = FALSE
        ),
        shiny::selectInput("plot_type",
          label = "Display",
          choices = c(articles = "x", words = "y")
        ),
        shiny::selectInput("plot_dims",
          label = "Dimensions",
          choices = c("2D", "3D")
        )
      ),
      shinydashboard::menuItem("Appearance",
        icon = shiny::icon("paint-brush"),
        shiny::selectInput("palette", "Palette",
          choices = c(
            Viridis = "D",
            Magma = "A",
            Inferno = "B",
            Plasma = "C",
            Cividis = "E"
          ),
          multiple = FALSE
        ),
        shiny::sliderInput("color_alpha",
          label = "Opacity",
          min = 0.2,
          max = 1,
          step = 0.1,
          value = 0.9
        ),
        shiny::sliderInput("color_hue",
          label = "Hue",
          min = 0,
          max = 1,
          step = 0.05,
          value = c(0, 0.9)
        ),
        shiny::sliderInput("point_size",
          label = "Point Size",
          min = 0,
          max = 20,
          step = 2,
          value = 12
        )
      ),
      shinydashboard::menuItem("Save", # use modal save in this version
        icon = shiny::icon("save"),
        shiny::actionButton("save_data",
          label = "Save Data",
          width = "80%"
          # style = "color: #fff; background-color: #428bca;"
        ) # ,
        # shiny::actionButton("save_scatter",
        #   label = "Save Main Plot",
        #   width = "80%",
        #   style = "color: #fff; background-color: #428bca;"
        # ),
        # shiny::actionButton("save_bar",
        #   label = "Save Barplot",
        #   width = "80%",
        #   style = "color: #fff; background-color: #428bca;"
        # )
      )
    )
  )

  body<-shinydashboard::dashboardBody(
    shiny::tag("style", shiny::HTML("
      .content-wrapper,
        .right-side {
          background-color: #e2e2e2;
        }
    		.skin-black .main-header .logo {
    			background-color: #4a3384;
          color: #ffffff;
    		}
    		.skin-black .main-header .logo:hover {
    			background-color: #4a3384;
    		}
    		.skin-black .main-header .navbar {
    			background-color: #4a3384;
    		}
        .action-button {
          color: #fff;
          background: #4a3384;
          border-width: 0px;
        }
        .irs-bar {    /* slider info */
          background: #4a3384;
          border: #4a3384;
        }
        .irs-bar-edge {
          background: #4a3384;
          border: #4a3384;
        }
        .irs-grid-text {
          color: white;
        }
        .irs-grid-pol{
          background: white;
        }
        .irs-single {
          color: white;
          background: #4a3384;
        }
        .irs-from {
          color: white;
          background: #4a3384;
        }
        .irs-to {
          color: white;
          background: #4a3384;
        }
    ")),
    # upload button
    # checkbox
    # radiobutton
    shiny::fluidRow(
      shiny::column(width = 8,
        plotly::plotlyOutput("plot_main", height = "600px"),
        shiny::tableOutput("abstract_text")
      ),
      shiny::column(width=4,
        plotly::plotlyOutput("plot_topic", height = "450px"),
        shiny::tableOutput("selector_text"),
        # shiny::splitLayout(
          shiny::uiOutput("select_choice"),
          # uiOutput("select_save"),
        #   cellWidths = c("70%", "30%")
        # ),
        shiny::uiOutput("select_notes"),
        shiny::uiOutput("select_save")
        # old form:
        # shiny::splitLayout(
        #   uiOutput("select_yes"),
        #   uiOutput("select_no"),
        #   cellWidths = c("25%", "25%")
        # ),
        # shiny::splitLayout(
        #   shiny::uiOutput("topic_yes"),
        #   shiny::uiOutput("topic_no"),
        #   cellWidths = c("25%", "25%")
        # )
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