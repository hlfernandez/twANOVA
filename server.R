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
  
  output$plots <- renderPlot({
    data <- loadDataset()
    result <- performAnova()
    aFactorLevels <- result$aFactor@levels
    aFactorMeans <- vector("list", length(aFactorLevels))
    names(aFactorMeans) <- aFactorLevels

    bFactorLevels <- result$bFactor@levels
    bFactorMeans <- vector("list", length(bFactorLevels))
    names(bFactorMeans) <- bFactorLevels

    aFactorMeans <- lapply(aFactorMeans, function(element) { element <- 0 } )
    bFactorMeans <- lapply(bFactorMeans, function(element) { element <- 0 } )

    for(col in 1:ncol(data)) {
      factorAName <- as.character(data[1,col])
      factorBName <- as.character(data[2,col])
      aFactorMeans[[factorAName]] <- aFactorMeans[[factorAName]] +
      sum(as.numeric(as.character(data[3:nrow(data),col])))
      bFactorMeans[[factorBName]] <- bFactorMeans[[factorBName]] + sum(as.numeric(as.character(data[3:nrow(data),col])))
    }

    aFactorMeans <- lapply(aFactorMeans, function(element) { element <- element / (length(aFactorLevels) * (nrow(data) - 2) )} )
    bFactorMeans <- lapply(bFactorMeans, function(element) { element <- element / (length(bFactorLevels) * (nrow(data) - 2)) } )

    aFactorMeans <- unlist(aFactorMeans)
    bFactorMeans <- unlist(bFactorMeans)

    aFactorByBMeans <- vector("list", length(aFactorLevels))
    names(aFactorByBMeans) <- aFactorLevels
    aFactorByBMeans <- lapply(aFactorByBMeans, function(element) { element <- vector() } )

    bFactorByBMeans <- vector("list", length(bFactorLevels))
    names(bFactorByBMeans) <- bFactorLevels
    bFactorByBMeans <- lapply(bFactorByBMeans, function(element) { element <- vector() } )


    simpleEffects <- result$simpleEffects
    for(i in 1:length(simpleEffects)) {
      simpleEffect <- simpleEffects[[i]]
      
      factorAName <- simpleEffect@factorA
      aFactorByBMeans[[factorAName]] <- c(aFactorByBMeans[[factorAName]], mean(result$simpleEffects[[i]]@values))
      
      factorBName <- simpleEffect@factorB
      bFactorByBMeans[[factorBName]] <- c(bFactorByBMeans[[factorBName]], mean(result$simpleEffects[[i]]@values))
    }

    bColors <- colorRampPalette(c("blue", "red"))(length(bFactorByBMeans))
    aColors <- colorRampPalette(c("blue", "red"))(length(aFactorByBMeans))


    par(mfrow=c(2,2))
    plot(aFactorMeans, typ='l', ylim=c(min(aFactorMeans-10), max(aFactorMeans)+10),   main="A: main effects means", xlab="A factor levels", ylab="",xaxt='n')
    axis(1, at=1:length(aFactorLevels), labels=aFactorLevels) 
      
    plot(bFactorMeans, typ='l', ylim=c(min(bFactorMeans-10), max(bFactorMeans)+10),  main="B: main effects means", xlab="B factor levels", ylab="",xaxt='n')
    axis(1, at=1:length(bFactorLevels), labels=bFactorLevels) 

    plot(bFactorByBMeans[[1]], typ='l', ylim=c(min(unlist(bFactorByBMeans)),max(unlist(bFactorByBMeans))), col=bColors[1],  main="A: simple effect means respect factor B", xlab="A factor levels", ylab="",xaxt='n')
    for(i in 2:length(bFactorByBMeans)) {
      lines(bFactorByBMeans[[i]], col=bColors[i])
    }
    legend('topright',bFactorLevels,  lty=1,col=bColors)
    axis(1, at=1:length(aFactorLevels), labels=aFactorLevels) 

    plot(aFactorByBMeans[[1]], typ='l', ylim=c(min(unlist(aFactorByBMeans)),max(unlist(aFactorByBMeans))), col=aColors[1],  main="B: simple effect means respect factor B", xlab="B factor levels", ylab="",xaxt='n')
    for(i in 2:length(aFactorByBMeans)) {
      lines(aFactorByBMeans[[i]], col=aColors[i])
    }
    legend('topright',aFactorLevels,  lty=1,col=aColors)
    axis(1, at=1:length(bFactorLevels), labels=bFactorLevels) 
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
 
