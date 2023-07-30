
possibleGraphPanel<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("LikelihoodPlot")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleGraphPanel1<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("LikelihoodPlot1")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleReportPanel<-function() {
  
  if (switches$doPossible) {
  possibleReportPanel<-
    tabPanel("Possible",class="Graphs",
             plotOutput("LikelihoodReport")
    )
  } else {
    possibleReportPanel<-c()
  }  
}
