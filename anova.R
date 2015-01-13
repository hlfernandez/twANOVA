# Represents an ANOVA twAnovaFactor.
twAnovaFactor <- setClass("twAnovaFactor", 
  representation(
    name="character",		# Factor name
    levels="character",		# Factor levels
    marginalSums="numeric",	# Marginal sums
    sum="numeric"		# Sum of observations
)); 

# Set show method for factor
setMethod("show", "twAnovaFactor",
  function(object){
    cat("[Factor = ",object@name, ". Levels = ", object@levels,". Marginal = ",object@marginalSums,". Sum = ",object@sum,"]\n");
  }
)

# Represents an ANOVA simple effect.
simpleEffect <- setClass("simpleEffect", 
  representation(
    factorA="character",	# Factor A name
    factorB="character", 	# Factor B name
    values="numeric",		# Observations
    AB="numeric"		# Sum of observations
)); 

# Set show method for simpleEffect
setMethod("show", "simpleEffect",
  function(object){
    cat("[Factor A = ",object@factorA, ". Factor B = ", object@factorB,". Values = ",object@values,". AB =" ,object@AB,"]\n");
  }
)

# Stores the results of a two-way ANOVA.
twAnovaPartials <- setClass("twAnovaPartials", 
  representation(
    A="numeric",
    B="numeric",
    AB="numeric",
    T="numeric",
    Y="numeric"
  )
)

# Set show method for twAnovaPartials
setMethod("show", "twAnovaPartials",
  function(object){
    cat("A =",object@A,"\n")
    cat("B =",object@B,"\n")
    cat("AB =",object@AB,"\n")
    cat("T =",object@T,"\n")
    cat("Y =",object@Y,"\n")
    cat("\n")
  }
)

# Stores the results of a two-way ANOVA with the effect of the interaction.
twAnovaInteractionResult <- setClass("twAnovaInteractionResult", 
  representation(
    Fa="numeric",
    pa="numeric",
    Fb="numeric",
    pb="numeric",
    Fab="numeric",
    pab="numeric",
    
    SCa="numeric",
    DFa="numeric",
    MCa="numeric",
    
    SCb="numeric",
    DFb="numeric",
    MCb="numeric",
    
    SCab="numeric",
    DFab="numeric",
    MCab="numeric",
    
    SCsab="numeric",
    DFsab="numeric",
    MCsab="numeric",
    
    SCt="numeric",
    DFt="numeric"
));

# Set show method for twAnovaInteractionResult
setMethod("show", "twAnovaInteractionResult",
  function(object){
    cat("S.V\tSum Sq\tDf\tMean Sq\tF value\tPr(>f)\n")
    cat("A\t",	round(object@SCa,2),"\t",	object@DFa,"\t",		round(object@MCa,2),"\t",	round(object@Fa,4),"\t",	round(object@pa,6),significanceCode(object@pa),"\n")
    cat("B\t",	round(object@SCb,2),"\t",		object@DFb,"\t",	round(object@MCb,2),"\t",	round(object@Fb,4),"\t",	round(object@pb,6),significanceCode(object@pb),"\n")
    cat("AxB\t",round(object@SCab,2),"\t",		object@DFab,"\t",	round(object@MCab,2),"\t",	round(object@Fab,4),"\t",	round(object@pab,6),significanceCode(object@pab),"\n")
    cat("S/AxB\t",	round(object@SCsab,2),"\t",object@DFsab,"\t",	round(object@MCsab,2),"\n")
    cat("Total\t",		round(object@SCt,2),"\t",object@DFt,"\n")
    cat("\n")
  }
)

# Stores the results of a two-way ANOVA without the effect of the interaction.
twAnovaWithoutInteractionResult <- setClass("twAnovaWithoutInteractionResult", 
  representation(
    Fa="numeric",
    pa="numeric",
    Fb="numeric",
    pb="numeric",
    Fab="numeric",
    pab="numeric",
    
    SCa="numeric",
    DFa="numeric",
    MCa="numeric",
    
    SCb="numeric",
    DFb="numeric",
    MCb="numeric",

    SCsab="numeric",
    DFsab="numeric",
    MCsab="numeric",
    
    SCt="numeric",
    DFt="numeric"
));

significanceCode <- function(p) {
   code = " "
    if(p < 0.001) {
      code = " ***"
    } else if(p < 0.01) {
      code = " **"
    } else if(p < 0.05) {
      code = " *"
    }
    code
}

# Set show method for twAnovaInteractionResult
setMethod("show", "twAnovaWithoutInteractionResult",
  function(object){
    cat("S.V\tSum Sq\tDf\tMean Sq\tF value\tPr(>f)\n")
    cat("A\t",	round(object@SCa,2),"\t",object@DFa,"\t",	round(object@MCa,2),"\t",	round(object@Fa,4),"\t",round(object@pa,6),significanceCode(object@pa),"\n")
    cat("B\t",	round(object@SCb,2),"\t",object@DFb,"\t",	round(object@MCb,2),"\t",	round(object@Fb,4),"\t",round(object@pb,6),significanceCode(object@pb),"\n")
    cat("Intra (S/AxB)\t",	round(object@SCsab,2),"\t",object@DFsab,"\t",	round(object@MCsab,2),"\n")
    cat("Total\t",		round(object@SCt,2),"\t",	object@DFt,"\n")
    cat("\n")
  }
)


printABMatrix <- function(aFactor, bFactor, simpleEffects){
  factorAlevels <- aFactor@levels
  factorBlevels <- bFactor@levels
  cat("\n")
  cat("\t")
  for(factorAIndex in 1:length(factorAlevels)) {
    cat(factorAlevels[factorAIndex],"\t")
  }
  cat("\n") 
  for(factorBIndex in 1:length(factorBlevels)) {
    factorBName <- factorBlevels[factorBIndex]
    cat(factorBName,"\t")
    for(s in 1:length(simpleEffects)){
      simpleEffect <- simpleEffects[[s]]
      if(simpleEffect@factorB == factorBName){
	cat(simpleEffect@AB,"\t")
      }
    }
    cat(bFactor@marginalSums[factorBIndex])
    cat("\n")
  } 
  cat("\t")
  for(factorAIndex in 1:length(factorAlevels)) {
    cat(aFactor@marginalSums[factorAIndex],"\t")
  }
  cat(aFactor@sum)
  cat("\n")
  cat("\n")
}    

twoWayAnova <- function(data) {

  n 	<- (nrow(data) - 2)

  factorAlevels <- unique(unlist(data[1,]))
  a 		<- length(factorAlevels)
  aFactor	<- twAnovaFactor(
    name="A",
    levels=factorAlevels,
    marginalSums=0,
    sum=0
  )
  
  factorBlevels <- unique(unlist(data[2,]))
  b 		<- length(factorBlevels)
  bFactor	<- twAnovaFactor(
    name="B",
    levels=factorBlevels,
    marginalSums=0,
    sum=0
  )
  
  simpleEffects <- vector()    
  bFactorMarginalSums <- rep(0,length(factorBlevels))
  aFactorMarginalSums <- vector()
  for(factorAIndex in 1:length(factorAlevels)) {
    aMarginal <- 0
    for(factorBIndex in 1:length(factorBlevels)) {
      factorAName <- factorAlevels[factorAIndex]
      factorBName <- factorBlevels[factorBIndex]
      observations <- as.numeric(unlist(data[seq(3,nrow(data)),(factorBIndex + length(factorBlevels) * (factorAIndex-1))]))
      currentAB <- sum(observations)
      simpleEffects <- c(simpleEffects, 
	simpleEffect(
	  factorA=factorAName,
	  factorB=factorBName,
	  values=observations,
	  AB=currentAB
	)
      )
      aMarginal <- aMarginal + currentAB
      bFactorMarginalSums[factorBIndex] <- bFactorMarginalSums[factorBIndex] + currentAB
    }
    aFactorMarginalSums <- c(aFactorMarginalSums,aMarginal)
  }    
  
  aFactor@marginalSums <- aFactorMarginalSums  
  bFactor@marginalSums <- bFactorMarginalSums
  aFactor@sum <- sum(aFactorMarginalSums)
  bFactor@sum <- sum(bFactorMarginalSums)
    
  # Basic reasons calculation
  A <- sum(aFactor@marginalSums * aFactor@marginalSums) / ( b * n)
  B <- sum(bFactor@marginalSums * bFactor@marginalSums) / ( a * n)
  AB <- 0
  for(s in 1:length(simpleEffects)){
	simpleEffect <- simpleEffects[[s]]
	AB <- AB + (simpleEffect@AB)^2
  }
  AB <- AB / n
  
  Y <- 0
  for(row in 3:nrow(data)){
    for(col in 1:ncol(data)){
      val <- as.numeric(data[row,col])
      Y <-  Y + (val * val)
    }
  }
  
  T <- (aFactor@sum * aFactor@sum) / (a * b * n)
  
  partials <- twAnovaPartials(
    A=A,
    B=B,
    AB=AB,
    T=T,
    Y=Y
  )
  
  # Squared sums calculation
  SCa 	<- A - T
  SCb	<- B - T
  SCab 	<- AB - A - B + T
  SCsab <- Y - AB
  SCt 	<- Y - T
    
  DFa 	<- a-1
  DFb	<- b-1
  DFab	<- (a-1)*(b-1)
  DFsab	<- a*b*(n-1)
  DFt	<- a*b*n-1
  
  MCa 	<- SCa/DFa
  MCb 	<- SCb/DFb
  MCab 	<- SCab/DFab
  MCsab <- SCsab/DFsab
  
  Fa	<- MCa/MCsab
  pa	<- 1-pf(Fa, DFa,DFsab)
  Fb	<- MCb/MCsab
  pb	<- 1-pf(Fb, DFb,DFsab)
  Fab	<- MCab/MCsab
  pab	<- 1-pf(Fab, DFab,DFsab)
  
  interactionResult <- twAnovaInteractionResult(
    Fa=Fa,
    pa=pa,    
    Fb=Fb,
    pb=pb,
    Fab=Fab,
    pab=pab,
    
    SCa=SCa,
    DFa=DFa,
    MCa=MCa,
    
    SCb=SCb,
    DFb=DFb,
    MCb=MCb,
    
    SCab=SCab,
    DFab=DFab,
    MCab=MCab,
    
    SCsab=SCsab,
    DFsab=DFsab,
    MCsab=MCsab,
      
    SCt=SCt,
    DFt=DFt
  )
  
  # Calculation without interaction effect
  
  SCsabWI <- SCab + SCsab
  DFsabWI <- DFab + DFsab
  MCsabWI <- SCsabWI / DFsabWI
  
  FaWI	<- MCa/MCsabWI
  paWI	<- 1-pf(FaWI, DFa,DFsabWI)
  
  FbWI	<- MCb/MCsabWI
  pbWI	<- 1-pf(FbWI, DFb,DFsabWI)
    
  withoutInteractionResult <- twAnovaWithoutInteractionResult(
    Fa=FaWI,
    pa=paWI,    
    Fb=FbWI,
    pb=pbWI,
    
    SCa=SCa,
    DFa=DFa,
    MCa=MCa,
    
    SCb=SCb,
    DFb=DFb,
    MCb=MCb,    
    
    SCsab=SCsabWI,
    DFsab=DFsabWI,
    MCsab=MCsabWI,
      
    SCt=SCt,
    DFt=DFt
  )
  
  list(
    interactionResult=interactionResult,
    withoutInteractionResult=withoutInteractionResult,
    twAnovaPartials=partials,
    aFactor=aFactor,
    bFactor=bFactor,
    simpleEffects=simpleEffects
  )
}
