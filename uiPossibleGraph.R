
if (switches$doPossible) {
  possibleGraphPanel<-
    tabPanel("Possible",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "90%",plotOutput("LikelihoodPlot")),
                          tags$td(width = "7%"),
                          tags$td(width = "3%",valign="top",actionButton("LGPossibleStart",label=expandLabel)),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  possibleReportPanel<-
    tabPanel("Possible",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "97%",plotOutput("LikelihoodReport")),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  
} else {
  possibleGraphPanel<-c()
  possibleReportPanel<-c()
}
