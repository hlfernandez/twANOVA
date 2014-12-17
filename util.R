appendText <- function(text, toAppend) {
  paste(text, toAppend, sep="")
}

dataFrameToHTML <- function(data) {
  html <- "<table><tr>"
  for(col in 1:ncol(data)){
    html <- appendText(html, "<th>")
    html <- appendText(html, as.character(data[1,col]))
    html <- appendText(html, "</th>")
  }
  html <- appendText(html, "</tr><tr>")
  for(col in 1:ncol(data)){  
    html <- appendText(html, "<th>")
    html <- appendText(html, as.character(data[2,col]))
    html <- appendText(html, "</th>")
  }
  html <- appendText(html, "</tr>")
  for(row in 3:nrow(data)) {
    html <- appendText(html, "<tr>")
    for(col in 1:ncol(data)){
      html <- appendText(html, "<td>")
      html <- appendText(html, as.character(data[row,col]))
      html <- appendText(html, "</td>")
    }
    html <- appendText(html, "</tr>")
  }
  html <- appendText(html, "</table>")
  html
}

ABMatrixToHTML <- function(aFactor, bFactor, simpleEffects){
  factorAlevels <- aFactor@levels
  factorBlevels <- bFactor@levels
  html <- "<table><tr><td>&nbsp;</td>"
  for(factorAIndex in 1:length(factorAlevels)) {
    html <- appendText(html, paste("<td>",factorAlevels[factorAIndex],"</td>", sep=""))
  }
  html <- appendText(html, paste("<td>B</td>", sep=""))
  html <- appendText(html, "</tr>")
  for(factorBIndex in 1:length(factorBlevels)) {
    html <- appendText(html, "<tr>")
    factorBName <- factorBlevels[factorBIndex]
    html <- appendText(html, paste("<td>",factorBName,"</td>", sep=""))
    for(s in 1:length(simpleEffects)){
      simpleEffect <- simpleEffects[[s]]
      if(simpleEffect@factorB == factorBName){
	html <- appendText(html, paste("<td>",simpleEffect@AB,"</td>", sep=""))
      }
    }
    html <- appendText(html, paste("<td>",bFactor@marginalSums[factorBIndex],"</td>", sep=""))
    html <- appendText(html, "</tr>")
  } 
  html <- appendText(html, "<tr><td>A</td>")
  for(factorAIndex in 1:length(factorAlevels)) {
    html <- appendText(html, paste("<td>",aFactor@marginalSums[factorAIndex],"</td>", sep=""))
  }
  html <- appendText(html, paste("<td>",aFactor@sum,"</td>", sep=""))
  html <- appendText(html, "</tr></table>")
  html
} 

basicCoefficientsToHTML <- function(partials) {
  html <- paste("<p>[A] =  (&sum; A<sup>2</sup>) &divide; (bn) = ", partials@A, "<p/>", sep="")
  html <- appendText(html,paste("[B] =  (&sum; B<sup>2</sup>) &divide; (an) = ", partials@B, "<br/>", sep=""))
  html <- appendText(html,paste("[AB] =  (&sum;&sum; AB<sup>2</sup>) &divide; (n) = ", partials@AB, "<br/>", sep=""))
  html <- appendText(html,paste("[Y] =  (&sum;&sum;&sum; Y<sup>2</sup>) = ", partials@Y, "<br/>",sep=""))
  html <- appendText(html,paste("[T] =  (T<sup>2</sup>) &divide; (abn) = ", partials@T, "<br/>", sep=""))
  html
}

twAnovaInteractionResultToHTML <- function(object) {
    html <- "<table width=\"100%\"><tr>"
    html <- appendText(html, "<td>S.V</td><td>Sum Sq</td><td>Df</td><td>Mean Sq</td><td>F value</td><td>Pr(>f)</td></tr>")
    html <- appendText(html, paste("<tr><td>A</td><td>",round(object@SCa,2),"</td><td>",object@DFa,"</td><td>",		round(object@MCa,2),"</td><td>",round(object@Fa,4),"</td><td>",	round(object@pa,6),significanceCode(object@pa),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>B</td><td>",round(object@SCb,2),"</td><td>",		object@DFb,"</td><td>",	round(object@MCb,2),"</td><td>",round(object@Fb,4),"</td><td>",	round(object@pb,6),significanceCode(object@pb),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>AxB</td><td>",round(object@SCab,2),"</td><td>",		object@DFab,"</td><td>",	round(object@MCab,2),"</td><td>",	round(object@Fab,4),"</td><td>",	round(object@pab,6),significanceCode(object@pab),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>S/AxB</td><td>",	round(object@SCsab,2),"</td><td>",object@DFsab,"</td><td>",	round(object@MCsab,2),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>Total</td><td>",round(object@SCsab,2),"</td><td>",object@DFt,"</td></tr>"))
    html <- appendText(html, "</table>")
}

twAnovaWithoutInteractionResultToHTML <- function(object) {
    html <- "<table width=\"100%\"><tr>"
    html <- appendText(html, "<td>S.V</td><td>Sum Sq</td><td>Df</td><td>Mean Sq</td><td>F value</td><td>Pr(>f)</td></tr>")
    html <- appendText(html, paste("<tr><td>A</td><td>",round(object@SCa,2),"</td><td>",object@DFa,"</td><td>",		round(object@MCa,2),"</td><td>",round(object@Fa,4),"</td><td>",	round(object@pa,6),significanceCode(object@pa),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>B</td><td>",round(object@SCb,2),"</td><td>",		object@DFb,"</td><td>",	round(object@MCb,2),"</td><td>",round(object@Fb,4),"</td><td>",	round(object@pb,6),significanceCode(object@pb),"</td></tr>"))   
    html <- appendText(html, paste("<tr><td>S/AxB</td><td>",	round(object@SCsab,2),"</td><td>",object@DFsab,"</td><td>",	round(object@MCsab,2),"</td></tr>"))
    html <- appendText(html, paste("<tr><td>Total</td><td>",round(object@SCsab,2),"</td><td>",object@DFt,"</td></tr>"))
    html <- appendText(html, "</table>")
    html
}