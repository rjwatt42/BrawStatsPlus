
  t3<-
    conditionalPanel(condition = "input.LoadExtras",
                     tags$table(width = "100%",class="myTable",
                                tags$tr(
                                  tags$td(width = "25%", tags$div(style = localStyle, " ")),
                                  tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                  tags$td(width = "30%", tags$div(style = localStyle, paste0("Variable ",alphaChar,":"))),
                                  tags$td(width = "5%",
                                          checkboxInput("sReplVarAlpha",label=NULL,value=design$sReplVarAlpha)
                                  ),
                                  tags$td(width = "35%", numericInput("sReplAlpha",label=NULL,value=design$sReplAlpha)
                                  ),
                                )
                     )
    )
  
  t4<- 
    conditionalPanel(condition = "input.LoadExtras",
                     tags$table(width = "100%",class="myTable",
                                tags$tr(
                                  tags$td(width = "30%", id="sBudget1", tags$div(style = localStyle, "No Reps:")),
                                  tags$td(width = "10%", 
                                          numericInput("sReplRepeats",label=NULL,value=design$sReplRepeats,min=0, max=100, step=1)
                                  ),
                                  tags$td(width = "20%", tags$div(style = localStyle, "Budget")),
                                  tags$td(width = "5%", 
                                          checkboxInput("sReplUseBudget",label=NULL,value=design$sReplUseBudget)
                                  ),
                                  tags$td(width = "15%", id="sBudget2", tags$div(style = localStyle, "Total:")),
                                  tags$td(width = "20%", numericInput("sReplBudget",label=NULL,value=design$sReplBudget)
                                  )
                                )
                     )
    )

replicationTab<-function(prefix="") {
  replicationTabReserve<-
                     tabPanel("Replicate",
                                  style = paste("background: ",subpanelcolours$designC), 
                                  wellPanel(
                                    style = paste("background: ",subpanelcolours$designC,";"),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "25%", tags$div(style = localStyle, "Replication:")),
                                                 tags$td(width = "10%", 
                                                         checkboxInput("sReplicationOn",label=NULL,value=design$sReplicationOn)
                                                 ),
                                                 tags$td(width = "25%", tags$div(style = localStyle, "In:")),
                                                 tags$td(width = "40%", 
                                                         selectInput("sReplSigOnly",label=NULL,
                                                                     choices=c("any"="No","sig only"="Yes"),
                                                                     selected=design$sReplSigOnly,selectize=FALSE)
                                                 ),
                                               )
                                    ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "10%"), 
                                                 tags$td(width = "25%", tags$div(style = localStyle, "Out:")),
                                                 tags$td(width = "40%", 
                                                         selectInput("sReplKeep",label=NULL,
                                                                     choices=c("last","joint","cautious","median","largest n"="largeN","smallest p"="smallP"),
                                                                     selected=design$sReplKeep,selectize=FALSE)
                                                 ),
                                               ),
                                    ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "16%", tags$div(style = localStyle, "Power: ")),
                                                 tags$td(width = "7%", 
                                                         checkboxInput("sReplPowerOn",label=NULL,value=design$sReplPowerOn)
                                                 ),
                                                 tags$td(width = "15%", 
                                                         numericInput("sReplPower",label=NULL,value=design$sReplPower,min=0, max=1, step=0.1)
                                                 ),
                                                 tags$td(width = "25%", selectInput("sReplTails",label=NULL,
                                                                                    choices=c("2-tail"=2,"1-tail"=1),
                                                                                    selected=design$sReplTails,selectize=FALSE)
                                                 ),
                                                 tags$td(width = "10%", tags$div(style = localStyle, "Prior: ")),
                                                 tags$td(width = "22%", 
                                                         selectInput("sReplCorrection",label=NULL,
                                                                     choices=c("None","World","Prior"),selected=design$sReplCorrection,selectize=FALSE)
                                                 )
                                               ),
                                    ),
                                    t3,
                                    t4
                                  )
  )

  if (switches$doReplications){
    replicationTab<-replicationTabReserve
  } else {
    replicationTab<-c()
  }
}
