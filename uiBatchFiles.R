if (switches$doBatchFiles) 
  {
  batchTab<-tabPanel("Batch",
                     style = paste("background: ",subpanelcolours$filesC), 
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, "Files:")),
                                    tags$td(width = "30%", 
                                            selectInput("batchFile_length",label=NULL,
                                                        c("10" = 10,
                                                          "20" = 20,
                                                          "50" = 50,
                                                          "100" = 100),
                                                        selected = "10",
                                                        selectize=FALSE)
                                    ),
                                    tags$td(width = "15%", tags$div(style = localStyle, "vars:")),
                                    tags$td(width = "20%", 
                                            selectInput("batchFile_nVars",label=NULL,
                                                        c("2" = "2",
                                                          "3" = "3",
                                                          "either"="either"),
                                                        selected = "2",
                                                        selectize=FALSE)
                                    ),
                                    tags$td(width = "20%",actionButton("batchFileRun", "Make")
                                    )
                                  )
                       )
  )
} else {
  batchTab<-c()
}