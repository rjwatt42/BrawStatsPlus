
possibleGraphPanel<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("LikelihoodPlot"),
               style =paste("background:", maincolours$graphC, ";")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleGraphPanel1<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("LikelihoodPlot1"),
               style =paste("background:", maincolours$graphC, ";")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleReportPanel<-function() {
  
  if (switches$doPossible) {
  possibleReportPanel<-
    tabPanel("Possible",class="Graphs",
             plotOutput("LikelihoodReport"),
             style =paste("background:", maincolours$graphC, ";")
    )
  } else {
    possibleReportPanel<-c()
  }  
}
