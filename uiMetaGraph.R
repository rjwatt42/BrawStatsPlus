
metaGraphPanel<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             plotOutput("MetaAnalysisPlot"),
             style =paste("background:", maincolours$graphC, ";")
    )
  } else {
    c()
  }
}

metaGraphPanel1<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             plotOutput("MetaAnalysisPlot1"),
             style =paste("background:", maincolours$graphC, ";")
    )
  } else {
    c()
  }
}

  metaReportPanel<-function() {
    if (switches$doMetaAnalysis) {
      tabPanel("MetaAnalysis",class="Graphs",
               plotOutput("MetaAnalysisReport"),
               style =paste("background:", maincolours$graphC, ";")
      )
  } else {
    c()
  }
  }