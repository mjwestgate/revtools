screen_visual_ui <- function(){

  # build user interface
  header<- shinydashboard::dashboardHeader(
    title="revtools"
  )

  sidebar<-shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data",
        icon = icon("bar-chart-o"),
        startExpanded = TRUE,
        fileInput("data_in", label = "Import"),
        uiOutput("response_selector"),
        sidebarMenuOutput("variable_menu"),
        selectInput("model_type", label = "Model Type", choices = c("LDA", "CTM")),
        sliderInput("n_topics", "# Topics", min = 4, max = 30, step = 1, value = 5),
        sliderInput("n_iterations", "# Iterations", min = 1000, max = 20000, step = 1000, value = 2000),
        actionButton("calc_model", "Calculate Model")
      ),
      sidebarMenu(
        menuItem("Plot", icon = icon("bar-chart-o"),
          selectInput("hide_names", "Hide authors etc?",
            choices = c("FALSE", "TRUE"),
            multiple = FALSE
          ),
          selectInput("plot_type", "Display", choices = c(
            articles = "x",
            words = "y"
            )
          ),
          selectInput("plot_dims", "Dimensions", choices = c("2D", "3D"))
        )
      ),
      sidebarMenu(
        menuItem("Appearance", icon = icon("paint-brush"),
          selectInput("palette", "Palette",
            choices = c(
              Viridis = "D",
              Magma = "A",
              Inferno = "B",
              Plasma = "C",
              Cividis = "E"
            ),
            multiple = FALSE
          ),
          sliderInput("color_alpha", "Opacity", min = 0.2, max = 1, step = 0.1, value = 0.9),
          sliderInput("color_hue", "Hue", min = 0, max = 1, step = 0.05, value = c(0, 0.9)),
          sliderInput("point_size", "Point Size", min = 0, max = 20, step = 2, value = 12)
        )
      )
      # actionButton("save", "Save")	# add modal save in this version
    )
  )

  body<-shinydashboard::dashboardBody(
    shiny::tag("style", shiny::HTML("
    .content-wrapper,
      .right-side {
        background-color: #e2e2e2;
      }
      .small-box.bg-black {
  			background-color: #777777 !important;
  			color: #fff !important;
  		}
  		.skin-black .main-header .logo {
  			background-color: #777777;
  		}
  		.skin-black .main-header .logo:hover {
  			background-color: #777777;
  		}
  		.skin-black .main-header .navbar {
  			background-color: #777777;
  		}
  		.box.box-solid.box-primary>.box-header {
  			color:#fff;
  			background:#444444
  		}
  		.box.box-solid.box-primary{
  			border-bottom-color:#444444;
  			border-left-color:#444444;
  			border-right-color:#444444;
  			border-top-color:#444444;
  		}
  		.download_class{
  			background-color: #444444;
  			color: #e2e2e2;
  		}
  		#view
    ")),
    fluidRow(
      column(width = 8,
        plotly::plotlyOutput("plot_main", height = "600px"),
        shiny::tableOutput("abstract_text")
      ),
      column(width=4,
        shinydashboard::box(
          width = NULL,
          title = "Topics",
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          plotly::plotlyOutput("plot_topic")
        ),
        shinydashboard::box(
          width = NULL,
          title = "Selected Text",
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          shiny::tableOutput("selector_text"),
          shiny::splitLayout(
            uiOutput("select_yes"),
            uiOutput("select_no"),
            cellWidths = c("25%", "25%")
          ),
          shiny::splitLayout(
            shiny::uiOutput("topic_yes"),
            shiny::uiOutput("topic_no"),
            cellWidths = c("25%", "25%")
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
