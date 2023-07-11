uiCheating<-function(prefix="") {
  conditionalPanel(condition="input.LoadExtras",
    tags$table(width = "100%",class="myTable",id=paste0(prefix,"Cheating"),
               tags$tr(
                 tags$td(width = "30%", tags$div(style = localStyle, "Cheating:")),
                 tags$td(width = "30%", 
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
                 tags$td(width = "20%", tags$div(style = localStyle, "Amount:")),
                 tags$td(width = "20%",
                         numericInput(paste0(prefix,"sCheatingAmount"),label=NULL,value=design$sCheatingAmount))
               ),
    )
  )
}
