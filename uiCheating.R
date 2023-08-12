uiCheating<-function(prefix="") {
  conditionalPanel(condition="input.LoadExtras",
                   tags$table(width = "100%",class="myTable",id=paste0(prefix,"Cheating"),
                              tags$tr(
                                tags$td(width = "30%", tags$div(style = localStyle, "Cheating:")),
                                tags$td(width = "25%", 
                                        selectInput(paste0(prefix,"sCheating"),label=NULL,
                                                    choices=list("None"="None",
                                                                 "Particiants"=list(
                                                                   "Grow"="Grow",
                                                                   "Prune"="Prune",
                                                                   "Replace"="Replace"
                                                                 ),
                                                                 "Studies"=list(
                                                                   "Retry"="Retry",
                                                                   "Add"="Add"
                                                                 )
                                                    ),
                                                    selected=design$sCheating,
                                                    selectize=FALSE
                                        )
                                ),
                                tags$td(width = "25%",
                                        conditionalPanel(condition="input.sCheating=='Retry' || input.sCheating=='Add'",
                                                         tags$table(width = "100%",class="myTable",
                                                                    tags$td(width = "100%",
                                                                            selectInput(paste0(prefix,"sCheatingLimit"),label=NULL,
                                                                                        choices=list("Fixed"="Fixed","Budget"="Budget"),
                                                                                        selected=design$sCheatingLimit,
                                                                                        selectize=FALSE
                                                                            )
                                                                    )
                                                         )
                                        ),
                                        conditionalPanel(condition="input.sCheating=='Grow' || input.sCheating=='Prune' || input.sCheating=='Replace'",
                                                         tags$table(width = "100%",class="myTable",
                                                                    tags$td(width = "100%",tags$div(style = localStyle, "Amount:")
                                                                    )
                                                         )
                                        )
                                ),
                                tags$td(width = "25%",
                                        conditionalPanel(condition="(input.sCheating=='Retry' || input.sCheating=='Add') && input.sCheatingLimit=='Budget'",
                                                         tags$table(width = "100%",class="myTable",
                                                                    tags$td(width = "100%",
                                                                            numericInput(paste0(prefix,"sCheatingBudget"),label=NULL,value=design$sCheatingBudget)
                                                                    )
                                                         )
                                        ),
                                        conditionalPanel(condition="(input.sCheating=='Grow' || input.sCheating=='Prune' || input.sCheating=='Replace') || ((input.sCheating=='Retry' || input.sCheating=='Add') && input.sCheatingLimit=='Fixed')",
                                                         tags$table(width = "100%",class="myTable",
                                                                    tags$td(width = "100%",
                                                                            numericInput(paste0(prefix,"sCheatingAmount"),label=NULL,value=design$sCheatingAmount)
                                                                    )
                                                         )
                                        ),
                                )
                              )
                   )
  )
}
