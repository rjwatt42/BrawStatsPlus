
  metaGraphPanel<-function() {
      tabPanel("MetaAnalysis",class="Graphs",
               tags$table(width = "100%",class="myTable",
                          tags$tr(
                            tags$td(width = "97%",plotOutput("MetaAnalysisPlot")),
                            tags$td(width = "3%",valign="top",actionButton("LGMetaStart",label=expandLabel)),
                          )
               ),
               style =paste("background:", maincolours$graphC, ";")
      )
  }
  
  metaReportPanel<-function() {
      tabPanel("MetaAnalysis",class="Graphs",
               tags$table(width = "100%",class="myTable",
                          tags$tr(
                            tags$td(width = "97%",plotOutput("MetaAnalysisReport")),
                          )
               ),
               style =paste("background:", maincolours$graphC, ";")
      )
  }