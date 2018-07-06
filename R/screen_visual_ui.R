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
        fileInput("data_in", label="Import"),
        uiOutput("response_selector"),
        sidebarMenuOutput("variable_menu"),
        selectInput("model_type", label="Model Type", choices=c("LDA", "CTM")),
        sliderInput("n_topics", "# Topics", min=4, max=30, step=1, value=5),
        sliderInput("n_iterations", "# Iterations", min=1000, max=20000, step=1000, value= 2000),
        actionButton("calc_model", "Calculate Model")
      ),
      # sidebarMenuOutput("plot_menu"),
      sidebarMenu(
        menuItem("Plot", icon = icon("bar-chart-o"),
          selectInput("plot_type", "Display", choices = c(
            articles = "x",
            words = "y"
            )
          ),
          selectInput("plot_dims", "Dimensions", choices = c("2D", "3D")),
          sliderInput("screen_size", "Window Height (px)", min = 400, max = 1400, step = 100, value = 600)
        )
      ),
      # sidebarMenuOutput("appearance_menu")
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
    fluidRow(
      column(width = 8,
        shinydashboard::box(width=NULL, height=800, title="Plot",  solidHeader=TRUE, status="primary",
          # tableOutput("example_text")
          plotly::plotlyOutput("plot_main")
        )
      ),
      column(width=4,
        shinydashboard::box(width=NULL, title="Topics",  solidHeader=TRUE, status="primary",
          plotly::plotlyOutput("plot_topic")
        ),
        shinydashboard::box(width=NULL, title="Text",  solidHeader=TRUE, status="primary",
          # plotly::plotlyOutput("plot_main")
          tableOutput("example_text")
        )
      )
    )
  )

return(list(header=header, sidebar=sidebar, body=body))

}
