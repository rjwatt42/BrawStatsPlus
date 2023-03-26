source("uiHypothesisPart.R")
source("uiDesignPart.R")

LGmodalPossible <-
  bsModal(id="LGmodalPossible", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     hypothesisPanel("LGlikelihood"),
                     designPanel("LGlikelihood"),
                     
                     wellPanel(
                       style = paste("background: ",subpanelcolours$likelihoodC,";"),
                       tabsetPanel(id="LGshowPossible", type="tabs",
                                 tabPanel("Possible:",value="Possible",
                                 ),
                                 tabPanel("Samples",value="Samples",id="uiLGSamples",
                                          wellPanel(
                                            style = paste("background: ",subpanelcolours$likelihoodC,";"),
                                            tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                                       tags$tr(
                                                         tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                                         tags$td(width = "20%", numericInput("LGlikelihoodSampRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                                         tags$td(width = "15%",tags$div(style = localStyle, "source:")),
                                                         tags$td(width = "20%",
                                                                 selectInput("LGlikelihoodUseSource",label=NULL,
                                                                             choices=c("world","prior"),selected=likelihood$UseSource,
                                                                             selectize=FALSE)
                                                         )
                                                       )
                                            ),
                                            tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                                       tags$tr(
                                                         tags$td(width = "35%", tags$div(style = localStyle, "cut:")),
                                                         tags$td(width = "15%", checkboxInput("LGlikelihood_cutaway",label=NULL,value=likelihood$cutaway)),
                                                         tags$td(width = "35%", tags$div(style = localStyle, "Sig Only:")),
                                                         tags$td(width = "15%", checkboxInput("LGlikelihood_sigonly",label=NULL,value=likelihood$sigOnly))
                                                       )
                                            ),
                                            tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                                         tags$td(width = "20%", 
                                                                 selectInput("LGlikelihood_length", label=NULL,
                                                                             SlikelihoodLengthChoices,selectize=FALSE)
                                                         ),
                                                         tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                                         tags$td(width = "10%", checkboxInput("LGlikelihood_append", label=NULL)),
                                                         tags$td(width = "10%", actionButton("LGlikelihood_run", "Run"))
                                                       )
                                            ),
                                          )
                                          ),
                                 tabPanel("Populations",value="Populations",id="uiLGPopulations",
                                          wellPanel(
                                            style = paste("background: ",subpanelcolours$likelihoodC,";"),
                                            tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                                     tags$tr(
                                                       tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                                       tags$td(width = "20%", numericInput("LGlikelihoodPSampRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                                       tags$td(width = "15%",tags$div(style = localStyle, "prior:")),
                                                       tags$td(width = "20%",
                                                               selectInput("LGlikelihoodUsePrior",label=NULL,
                                                                           choices=c("none","world","prior"),selected=likelihood$UsePrior,
                                                                           selectize=FALSE)
                                                       )
                                                     )
                                            ),
                                            tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                                         tags$td(width = "20%", 
                                                                 selectInput("LGlikelihoodP_length", label=NULL,
                                                                             PlikelihoodLengthChoices,selectize=FALSE)
                                                         ),
                                                         tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                                         tags$td(width = "10%", checkboxInput("LGlikelihoodP_append", label=NULL)),
                                                         tags$td(width = "10%", actionButton("LGlikelihoodP_run", "Run")),
                                                       )
                                            ),
                                          )
                                 ),
                                 tabPanel("Prior",value="Prior",id="uiLGPrior",
                                          wellPanel(
                                            style = paste("background: ",subpanelcolours$likelihoodC,";"),
                                            priorPanel("LGlikelihood",asTable=TRUE)
                                          )
                                 ),
                                 tabPanel("#",value="#",id="uiLGOptions",
                                          wellPanel(
                                            style = paste("background: ",subpanelcolours$likelihoodC,";"),
                                            tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "30%",  tags$div(style = labelStyle, "Options:")),
                                                         tags$td(width = "30%"),
                                                         tags$td(width = "20%"),
                                                         tags$td(width = "20%")
                                                       ),
                                                       tags$tr(
                                                         tags$td(width = "30%", tags$div(style = localStyle, "show theory:")),
                                                         tags$td(width = "30%", checkboxInput("LGlikelihoodTheory", value=FALSE, label=NULL)),
                                                         tags$td(width = "20%"),
                                                         tags$td(width = "20%", 
                                                                 selectInput("LGlikelihoodViewRZ", label=NULL,
                                                                             c("r" = "r",
                                                                               "z" = "z"),selected=likelihood$viewRZ,selectize=FALSE)
                                                         )
                                                       )
                                            ),
                                            tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "30%", tags$div(style = localStyle, "longhand:")),
                                                         tags$td(width = "5%", checkboxInput("LGlikelihoodLongHand", value=TRUE, label=NULL)),
                                                         tags$td(width = "40%", tags$div(style = localStyle, "sim slice:")),
                                                         tags$td(width = "15%",numericInput("LGlikelihoodSimSlice",label=NULL,value=0.1,max=0.2,min=0.0001,step=0.01)),
                                                         tags$td(width = "10%", checkboxInput("LGlikelihoodCorrection", value=FALSE, label=NULL)),
                                                       )
                                            ),
                                            tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "15%", tags$div(style = localStyle, "view:")),
                                                         tags$td(width = "20%", 
                                                                 selectInput("LGlikelihoodView", label=NULL,
                                                                             c("3D" = "3D",
                                                                               "2D" = "2D"),selectize=FALSE)
                                                         ),
                                                         tags$td(width = "5%", tags$div(style = localStyle, "az:")),
                                                         tags$td(width = "15%", 
                                                                 numericInput("LGlikelihoodAzimuth",label=NULL,
                                                                              min = -180,
                                                                              max = 180,
                                                                              step = 5,
                                                                              value = likelihood$azimuth)
                                                         ),
                                                         tags$td(width = "5%", tags$div(style = localStyle, "elev:")),
                                                         tags$td(width = "15%", 
                                                                 numericInput("LGlikelihoodElevation",label=NULL,
                                                                              min = 0,
                                                                              max = 90,
                                                                              step = 5,
                                                                              value = likelihood$elevation)
                                                         ),
                                                         tags$td(width = "5%", tags$div(style = localStyle, "r:")),
                                                         tags$td(width = "20%", 
                                                                 numericInput("LGlikelihoodRange",label=NULL,
                                                                              min = 0,
                                                                              max = 100,
                                                                              step = 1,
                                                                              value = likelihood$range)
                                                         ),
                                                       )
                                            )
                                          )
                                          )
                                 )
                       ),
                     wellPanel(
                           style = paste("background: ",subpanelcolours$likelihoodC,";"),
                           tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                      tags$td(width = "20%",actionButton("LGlikelihoodClose","Close")),
                                    )
                           )
                         )
              ),# close of column
              column(width=9,plotOutput("LGshowPossibleOutput",height=LGGraphHeight))
            ) 
          )
  )
LGmodalPossible$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

