uiCheating<-function(prefix="") {
    tags$table(width = "100%",class="myTable",id=paste0(prefix,"Cheating"),
               tags$tr(
                 tags$td(width = "30%", tags$div(style = localStyle, "Cheating:")),
                 tags$td(width = "30%", 
                         selectInput(paste0(prefix,"sCheating"),label=NULL,
                                     choices=c("None"="None",
                                               "Grow"="Grow",
                                               "Prune"="Prune",
                                               "Replace"="Replace",
                                               "Retry"="Retry"
                                     ),
                                     selected=design$sCheating,
                                     selectize=FALSE
                         )
                 ),
                 tags$td(width = "20%", tags$div(style = localStyle, "Amount:")),
                 tags$td(width = "20%",
                         numericInput(paste0(prefix,"sCheatingK"),label=NULL,value=design$sCheatingK))
               ),
    )
}