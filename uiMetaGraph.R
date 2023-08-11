
metaGraphPanel<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             style="margin:0px;padding:0px;",
             plotOutput("MetaAnalysisPlot")
    )
  } else {
    c()
  }
}

metaGraphPanel1<-function() {
  if (switches$doMetaAnalysis) {
    tabPanel("MetaAnalysis",class="Graphs",
             style="margin:0px;padding:0px;",
             plotOutput("MetaAnalysisPlot1")
    )
  } else {
    c()
  }
}

  metaReportPanel<-function() {
    if (switches$doMetaAnalysis) {
      tabPanel("MetaAnalysis",class="Graphs",
               style="margin:0px;padding:0px;",
               plotOutput("MetaAnalysisReport")
      )
  } else {
    c()
  }
  }