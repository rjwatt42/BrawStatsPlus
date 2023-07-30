
metaGraphPanel<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             plotOutput("MetaAnalysisPlot")
    )
  } else {
    c()
  }
}

metaGraphPanel1<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             plotOutput("MetaAnalysisPlot1")
    )
  } else {
    c()
  }
}

  metaReportPanel<-function() {
    if (switches$doMetaAnalysis) {
      tabPanel("MetaAnalysis",class="Graphs",
               plotOutput("MetaAnalysisReport")
      )
  } else {
    c()
  }
  }