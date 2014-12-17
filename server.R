library(shiny)
source("anova.R")
source("util.R")

# Define server logic required to perform the analysis
shinyServer(function(input, output) {

  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
    
  loadDataset <- reactive({
    if (is.null(input$file)) {
	return (read.csv("data/testData.csv", header=FALSE, stringsAsFactors=FALSE));
    } else {
      return (read.csv(input$file$datapath, header=FALSE, stringsAsFactors=FALSE));
    }
  });
  
  performAnova <- reactive({
    if (is.null(input$file)) {
	return (twoWayAnova(loadDataset()));
    } else {
      return (read.csv(input$file$datapath, header=FALSE, stringsAsFactors=FALSE));
    }
  });
  
  output$result <- renderUI({         
      data <- loadDataset()
      result <- performAnova()
      
      HTML(
	"<h4>Input data</h4>",
	dataFrameToHTML(data),
	"<h4>AB data matrix</h4>",
	ABMatrixToHTML(result$aFactor, result$bFactor, result$simpleEffects),
	"<h4>Basic coefficients</h4>",
	basicCoefficientsToHTML(result$twAnovaPartials),
	"<h4>ANOVA summary table with the effect of the interaction</h4>",
	twAnovaInteractionResultToHTML(result$interactionResult),
	"<h4>ANOVA summary table without the effect of the interaction</h4>",
	twAnovaWithoutInteractionResultToHTML(result$withoutInteractionResult),
	"<br/>"
    )   
  })
  
  output$downloadSample <- downloadHandler(
    filename = function() { 
	"sampleDataTwoWay.csv" 
    },
    
    content = function(file) {
	write.csv(read.csv("data/testData.csv", header=FALSE, stringsAsFactors=FALSE), file, row.names=FALSE, quote=FALSE);
    }
  )  
})
 
