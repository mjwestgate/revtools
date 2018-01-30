revtools_ui<-function(){

# build user interface
header<- shinydashboard::dashboardHeader(title="revtools")
sidebar<-shinydashboard::dashboardSidebar(
	sidebarMenu(
		id="tabs",
		menuItem("Plot", icon=icon("bar-chart-o"),	
			p("Display"),
			menuSubItem("Articles", tabName="articles", selected=TRUE),
			menuSubItem("Words", tabName="words"),
			p("Dimensions"),
			menuSubItem("2D", tabName="2d", selected=TRUE),
			menuSubItem("3D", tabName="3d"),
			sliderInput("screen_size", "Window Height (px)", min=400, max=1400, step=100, value= 600)
		),
		menuItem("Colors", icon=icon("paint-brush"),
			p("Palette"),
			menuSubItem("Viridis", tabName="viridis"),
			menuSubItem("Magma", tabName="magma", selected=TRUE),
			menuSubItem("Inferno", tabName="inferno"),
			menuSubItem("Plasma", tabName="plasma"),
			menuItem("Options",			
				sliderInput("color_alpha", "Opacity", min=0.2, max=1, step=0.1, value= 0.9),
				sliderInput("color_hue", "Hue", min=0, max=1, step=0.05, value= c(0, 0.9)),
				sliderInput("point_size", "Point Size", min=0, max=20, step=2, value= 12)
			)
		),
		menuItem("Topic Model", icon=icon("calculator"),
			p("Model Type"),
			menuSubItem("LDA", tabName="lda", selected=TRUE),
			menuSubItem("CTM", tabName="ctm"),
			p("Model Specification"),
			sliderInput("iterations", "# Iterations", min=1000, max=20000, step=1000, value= 2000),
			sliderInput("n_topics", "# Topics", min=4, max=30, step=1, value=5),
			actionButton("go_LDA", strong("Recalculate"))
		),
		menuItem("Save", icon=icon("save"),
			selectInput("save_type", "File Type", 
				choices=c(".csv (selection data)", ".rds (all data)")),
			textInput("saveas", "Save As:", "revtools_results.csv"),
			actionButton("save", "Save")
		)
	)
)

body<-shinydashboard::dashboardBody(
	fluidRow(
		column(width=8,
			shinydashboard::box(width=NULL,
				shinycssloaders::withSpinner(plotly::plotlyOutput("plot_main"))
			)
		),
		column(width=4, 
			shinydashboard::box(title="Topics", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=FALSE,
				plotly::plotlyOutput("plot_topic")
			),
			shinydashboard::box(
				title="Selected Text", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=FALSE,
				tableOutput("plot_click"),
				shiny::splitLayout(
					uiOutput("select_yes"),
					uiOutput("select_no"),
					cellWidths=c("25%", "25%")
				),
				shiny::splitLayout(
					uiOutput("topic_yes"),
					uiOutput("topic_no"),
					cellWidths=c("25%", "25%")
				)
			),
			shinydashboard::box(title="Abstract", width=NULL, solidHeader=TRUE, status="primary",
				collapsible=TRUE, collapsed=TRUE,
				tableOutput("abstract_info")
			)
		)
	)
)

return(list(header=header, sidebar=sidebar, body=body))
}