
possibleGraphPanel<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("PossiblePlot")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleGraphPanel1<-function() {
  
  if (switches$doPossible) {
    possibleGraphPanel<-
      tabPanel("Possible",class="Graphs",
               plotOutput("PossiblePlot1")
      )
  } else {
    possibleGraphPanel<-c()
  }
}

possibleReportPanel<-function() {
  
  if (switches$doPossible) {
  possibleReportPanel<-
    tabPanel("Possible",class="Graphs",
             plotOutput("PossibleReport")
    )
  } else {
    possibleReportPanel<-c()
  }  
}
