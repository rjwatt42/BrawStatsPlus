
replicationTab<-function(prefix="") {
  replicationTabReserve<-
                     tabPanel("Replicate",
                                  style = paste("background: ",subpanelcolours$designC), 

                              tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "20%", tags$div(style = localStyle, "Replication:")),
                                                 tags$td(width = "5%", 
                                                         checkboxInput("sReplicationOn",label=NULL,value=design$sReplicationOn)
                                                 ),
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "25%")
                                               ),
                                    ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "20%", tags$div(style = localStyle, "In:")),
                                                 tags$td(width = "30%", 
                                                         selectInput("sReplSigOnly",label=NULL,
                                                                     choices=c("any"="No","sig only"="Yes"),
                                                                     selected=design$sReplSigOnly,selectize=FALSE)
                                                 ),
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "25%")
                                                 ),
                                    ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "20%", id="sBudget1", tags$div(style = localStyle, "Repeats:")),
                                                 tags$td(width = "30%", 
                                                         selectInput("sReplType",label=NULL,
                                                                     choices=c("Fixed","Budget"),
                                                                     selected=design$sReplType,selectize=FALSE)
                                                 ),
                                                 tags$td(width = "50%",
                                                         conditionalPanel(condition="input.sReplType=='Fixed'",
                                                                          tags$table(width = "100%",class="myTable",
                                                                                     tags$tr(
                                                                                       tags$td(width = "50%",tags$div(style = localPlainStyle, "number:")),
                                                                                       tags$td(width = "50%", 
                                                                                               numericInput("sReplRepeats",label=NULL,value=design$sReplRepeats,min=0, max=100, step=1)
                                                                                       )
                                                                                     )
                                                                          )
                                                         ),
                                                         conditionalPanel(condition="input.sReplType=='Budget'",
                                                                          tags$table(width = "100%",class="myTable",
                                                                                     tags$tr(
                                                                                       tags$td(width = "50%",tags$div(style = localPlainStyle, "available:")),
                                                                                       tags$td(width = "50%", 
                                                                                               numericInput("sReplBudget",label=NULL,value=design$sReplBudget)
                                                                                       )
                                                                                     )
                                                                          )
                                                         ),
                                                 )
                                               )
                                    ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "20%", tags$div(style = localStyle, "Power: ")),
                                                 tags$td(width = "5%", 
                                                         checkboxInput("sReplPowerOn",label=NULL,value=design$sReplPowerOn)
                                                 ),
                                                 tags$td(width="50%",
                                                         conditionalPanel(condition="input.sReplPowerOn",
                                                                          tags$table(width = "100%",class="myTable", 
                                                                                     tags$tr(
                                                                                       tags$td(width = "50%", 
                                                                                               numericInput("sReplPower",label=NULL,value=design$sReplPower,min=0, max=1, step=0.1)
                                                                                       ),
                                                                                       tags$td(width = "50%", selectInput("sReplTails",label=NULL,
                                                                                                                          choices=c("2-tail"=2,"1-tail"=1),
                                                                                                                          selected=design$sReplTails,selectize=FALSE)
                                                                                       )
                                                                                     )
                                                                          )
                                                         )
                                                 )
                                               )
                                    ),
                                    conditionalPanel(condition="input.sReplPowerOn",
                                      tags$table(width = "100%",class="myTable", id="extraRep1",
                                               tags$tr(
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "20%"),
                                                 tags$td(width = "5%"),
                                                 tags$td(width = "25%", tags$div(style = localPlainStyle, "Prior: ")),
                                                 tags$td(width = "25%", 
                                                         selectInput("sReplCorrection",label=NULL,
                                                                     choices=c("None","World","Prior"),selected=design$sReplCorrection,selectize=FALSE)
                                                 )
                                               )
                                    ),
                                    ),
                                    # tags$table(width = "100%",class="myTable", id="extraRep2",
                                    #            tags$tr(
                                    #              tags$td(width = "25%"),
                                    #              tags$td(width = "20%", tags$div(style = localStyle, "Budget:")),
                                    #              tags$td(width = "5%", 
                                    #                      checkboxInput("sReplUseBudget",label=NULL,value=design$sReplUseBudget)
                                    #              ),
                                    #              tags$td(width = "50%", id="sBudget2", 
                                    #                      conditionalPanel(condition="input.sReplUseBudget",
                                    #                      tags$table(width = "100%",class="myTable", 
                                    #                                 tags$tr(
                                    #                                   tags$td(width = "50%",tags$div(style = localPlainStyle, "Total:")),
                                    #                                   tags$td(width = "50%", numericInput("sReplBudget",label=NULL,value=design$sReplBudget))
                                    #                                 )
                                    #                      )
                                    #                      )
                                    #              )
                                    #            )
                                    # ),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$td(width = "20%", tags$div(style = localStyle, "Out:")),
                                                 tags$td(width = "30%", 
                                                         selectInput("sReplKeep",label=NULL,
                                                                     choices=c("last","joint","cautious","median","largest n"="largeN","smallest p"="smallP"),
                                                                     selected=design$sReplKeep,selectize=FALSE)
                                                 ),
                                                 tags$td(width = "25%"),
                                                 tags$td(width = "25%")
                                               )
                                    )
  )

  if (switches$doReplications){
    replicationTab<-replicationTabReserve
  } else {
    replicationTab<-c()
  }
}
