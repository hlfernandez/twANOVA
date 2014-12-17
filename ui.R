library(shiny)

shinyUI(fluidPage(
  titlePanel("Welcome to twANOVA!"),

  # Sidebar with a controls panel and a main panel with output
  sidebarLayout(
    sidebarPanel(    
      h4("Two-way analysis of variance"),      
      a(
	"Two-way ANOVA (independent samples)",
	href="http://en.wikipedia.org/wiki/Two-way_analysis_of_variance",
	target="_blank"
      ),
      tags$hr(),      
      
      h3("Input data"),
      fileInput('file', 'Choose the file containing your sample data:',
	accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')
	),                

      tags$hr(),      
      h4("Data format"),
      helpText("A CSV file with one column by factor and header with the factor names."),
      pre("
	a1,a1,a2,a2
	b1,b2,b1,b2
	1,2,3,4
	1,3,4,4"
      ),
      downloadButton("downloadSample", "Download sample data")
     ),     
      
    mainPanel(
      tabsetPanel(
	tabPanel("Two-way ANOVA", htmlOutput("result"))
     )
    )
  )
))