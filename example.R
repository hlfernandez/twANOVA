source("anova.R")

data <- read.csv("data/testData.csv", header=FALSE, stringsAsFactors=FALSE)

result <- twoWayAnova(data)

cat("AB Matrix:\n")
printABMatrix(result$aFactor, result$bFactor, result$simpleEffects)

cat("Partial calculations:\n")
print(result$twAnovaPartials)

cat("ANOVA summary table with the effect of the interaction:\n")
print(result$interactionResult)

cat("ANOVA summary table without the effect of the interaction:\n")
print(result$withoutInteractionResult)

# Two-way ANOVA with the aov function
dataT 	<- as.data.frame(t(data))
n	<- nrow(dataT) * (ncol(dataT)-2)
dataAOV <- data.frame(A=character(n), B=character(n), response=numeric(n), stringsAsFactors=FALSE)
row 	<- 1
for(i in 1:nrow(dataT)) {  
  for(j in 3:ncol(dataT)) {
    dataAOV[row,1] <- as.character(dataT[i,1])
    dataAOV[row,2] <- as.character(dataT[i,2])
    dataAOV[row,3] <- as.numeric(as.character((dataT[i,j])))
    row <- row + 1
  }
}
dataF 	<- within(dataAOV, { A <- factor(A); B <- factor(B) } )
anova 	<- aov(response ~  A+B , data=dataF)
cat("\nResult without the effect of the interaction using aov(response ~  A+B , data=dataF):\n")
print(summary(anova))
 
anova 	<- aov(response ~  A*B , data=dataF)
cat("\nResult with the effect of the interaction using aov(response ~  A*B , data=dataF):\n")
print(summary(anova))
 

 