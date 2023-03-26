source("uiCheating.R")

designPanel <- function(prefix="") {
  wellPanel(
    style = paste("background: ",subpanelcolours$designC,";"),
    tabsetPanel(id="LGDesign", type="tabs",
                tabPanel("Design:",value="Design",
                ),
                # single tab
                tabPanel("Sampling",value="Sampling",id="uiLGSampling",
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "35%", tags$div(style = localStyle, "sample size:")),
                                        tags$td(width = "15%", 
                                                numericInput(paste0(prefix,"sN"), label=NULL,value=design$sN)
                                        ),
                                        tags$td(width="15%",
                                                conditionalPanel(condition="input.LGsNRand",
                                                                 tags$div(style = localPlainStyle, "gamma:",id=paste0(prefix,"gamma"))
                                                                 )
                                        ),
                                        tags$td(width = "15%", 
                                                conditionalPanel(condition="input.LGsNRand",
                                                                 numericInput(paste0(prefix,"sNRandK"),label=NULL,value=design$sNRandK,min=0,step=0.5))
                                        ),
                                        tags$td(width = "10%", 
                                                checkboxInput(paste0(prefix,"sNRand"),label=NULL,value=design$sNRand)
                                        ),
                                        tags$td(width = "10%")
                                      )
                           ),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "35%", tags$div(style = localStyle, "method:")),
                                        tags$td(width = "65%", 
                                                selectInput(paste0(prefix,"sMethod"),label=NULL,c("Random","Stratified","Cluster","Convenience","Snowball"),
                                                            selected=design$sMethod,
                                                            selectize=FALSE)
                                        )
                                      ),
                           )
                           )
                         ),
                tabPanel("Anomalies",value="Anomalies",id="uiLGAnomalies",
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           uiCheating(prefix)
                         )
                )
    )
  )
  
}