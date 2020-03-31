preload_screen_abstracts_ui <- function(){

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

  sidebar <- shinydashboard::dashboardSidebar(disable = TRUE)

  body <- shinydashboard::dashboardBody(
    revtools_css(navbar_size = "870px"),
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        div(style = "height:500px;",
          tableOutput("citation")
        )
      )
    ),
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        hr(),
        uiOutput(outputId = "render_notes"),
        actionButton(
          inputId = "save_progress",
          label = "Save Progress to File",
          width = "200px"
        )
      )
    )
  )

  return(
    shinydashboard::dashboardPage(
      title =  "revtools | screen_abstracts_preloaded",
    	header,
    	sidebar,
    	body,
    	skin = "black"
    )
  )

}