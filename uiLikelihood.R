source("uiPrior.R")

SlikelihoodLengthChoices=c("100" = "100",
                          "250" = "250",
                          "500" = "500",
                          "1000" = "1000"
)
PlikelihoodLengthChoices=c("10" = "10",
                          "50" = "50",
                          "100" = "100",
                          "500" = "500",
                          "1000" = "1000",
                          "10000" = "10000"
)

if (switches$doWorlds) {
  source2<-c("null","world","prior")
  source1<-c("none","world","prior")
} else {
  source2<-c("null","hypothesis","prior")
  source1<-c("none","prior")
}

if (switches$doPossible) {
LikelihoodTab <-

  wellPanel(ID="MainLikelihood",
    style = paste("background: ",panelcolours$likelihoodC), 
    # h5("Evidence"),
  fluidRow(headerText("Likelihood functions based on sample or population")),
  tabsetPanel(type="tabs",id="Likelihood",
              # single tab
              tabPanel("Possible:",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       fluidRow(
                       )
              ),
              tabPanel("Samples",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "20%", numericInput("likelihoodSampRho", label=NULL,min=-1,max=1, step=0.1,value=likelihood$targetSample)),
                                      tags$td(width = "15%",tags$div(style = localStyle, "from:")),
                                      tags$td(width = "25%",selectInput("likelihoodUseSource",label=NULL,
                                                                        choices=source2,selected=likelihood$UseSource,
                                                                        selectize=FALSE)
                                      ),
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "35%", tags$div(style = localStyle, "cut:")),
                                      tags$td(width = "15%", checkboxInput("likelihood_cutaway",label=NULL,value=likelihood$cutaway)),
                                      tags$td(width = "35%", tags$div(style = localStyle, "Sig Only:")),
                                      tags$td(width = "15%", checkboxInput("likelihood_sigonly",label=NULL,value=likelihood$sigOnly))
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihood_length", label=NULL,
                                                          SlikelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihood_append", label=NULL,value=likelihood$appendSim)),
                                      tags$td(width = "10%", actionButton("likelihood_run", "Run"))
                                    )
                         ),
                       )
              ),
              tabPanel("Populations",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "20%", numericInput("likelihoodPSampRho", label=NULL,min=-1,max=1, step=0.05,value=likelihood$targetSample)),
                                      tags$td(width = "15%",tags$div(style = localStyle, "prior:")),
                                      tags$td(width = "25%",selectInput("likelihoodUsePrior",label=NULL,
                                                                        choices=source1,selected=likelihood$UsePrior,
                                                                        selectize=FALSE)
                                              ),
                                    )
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihoodP_length", label=NULL,
                                                          PlikelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihoodP_append", label=NULL,value=likelihood$appendSim)),
                                      tags$td(width = "10%", actionButton("likelihoodP_run", "Run")),
                                    )
                         ),
                         width="100%"
                       )
              ),
              tabPanel("Prior",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         priorPanel("likelihood",asTable=TRUE),
                       )
              ),
              tabPanel("#",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Analysis")),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%", tags$div(style = localPlainStyle, " ")),
                                      tags$td(width = "25%", 
                                              # actionButton("LGEvidenceStart","i")
                                      )
                                    ),
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    # tags$tr(
                                    #   tags$td(width = "50%", tags$div(style = localStyle, "include nulls:")),
                                    #   tags$td(width = "30%", checkboxInput("includeNulls", value=FALSE, label=NULL)),
                                    #   tags$td(width = "10%"),
                                    #   tags$td(width = "10%",actionButton("LGdoPossible","i"))
                                    # ),
                                    tags$tr(
                                      tags$td(width = "20%", tags$div(style = localPlainStyle, "sim slice:")),
                                      tags$td(width = "20%",numericInput("likelihoodSimSlice",label=NULL,value=likelihood$likelihoodSimSlice,max=0.2,min=0.0001,step=0.01)),
                                      tags$td(width = "25%", tags$div(style = localPlainStyle, "correction:")),
                                      tags$td(width = "10%",checkboxInput("likelihoodCorrection", value=likelihood$likelihoodCorrection, label=NULL)),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "HQ:")),
                                      tags$td(width = "10%",checkboxInput("likelihoodHQ", value=likelihood$likelihoodHQ, label=NULL))
                                    ),
                                    
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%", tags$div(style = localPlainStyle, " ")),
                                      tags$td(width = "25%", 
                                              # actionButton("LGEvidenceStart","i")
                                      )
                                    ),
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "view:")),
                                      tags$td(width = "20%", 
                                              selectInput("LikelihoodView", label=NULL,
                                                          c("3D" = "3D",
                                                            "2D" = "2D"),selected=likelihood$view,selectize=FALSE)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "show:")),
                                      tags$td(width = "40%", 
                                              selectInput("LikelihoodShow", label=NULL,
                                                          c("Normal" = "Normal",
                                                            "Opp" = "Opp"),selected=likelihood$show,selectize=FALSE)
                                      )
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, "az:")),
                                      tags$td(width = "15%", 
                                              numericInput("LikelihoodAzimuth",label=NULL,
                                                           min = -180,
                                                           max = 180,
                                                           step = 5,
                                                           value = likelihood$azimuth)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, "elev:")),
                                      tags$td(width = "15%", 
                                              numericInput("LikelihoodElevation",label=NULL,
                                                           min = 0,
                                                           max = 90,
                                                           step = 5,
                                                           value = likelihood$elevation)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, "r:")),
                                      tags$td(width = "20%", 
                                              numericInput("LikelihoodRange",label=NULL,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1,
                                                           value = likelihood$range)
                                      ),
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localPlainStyle, "")),
                                      tags$td(width = "45%",tags$div(style = localPlainStyle, "show theory:")),
                                      tags$td(width = "5%", checkboxInput("likelihoodTheory", value=likelihood$likelihoodTheory, label=NULL))
                                    ),
                         )
                       )
              )
              # help tab
              ,tabPanel("?",
                        style = paste("background: ",subpanelcolours$likelihoodC),
                        wellPanel(
                          style = paste("background: ",subpanelcolours$likelihoodC,";"),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br(HTML('<b>'),"Samples:",HTML('</b>')),
                                                tags$br("Visualize the samples produced by a given population"),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Populations:",HTML('</b>')),
                                                tags$br("Visualize the populations that produce a given sample"),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), '(slower process)'),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Steps:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. choose the distribution of populations'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'uniform - the common assumption'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(extremely unlikely in practice)'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'exponential - much more likely'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(high effect sizes are rare)'),
                                                tags$br(HTML('&emsp;'), '2. choose whether to see theoretical distributions'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(these are idealized)'),
                                                tags$br(HTML('&emsp;'), '3. press "Run"'),
                                       ),
                                     )
                          ),
                        )
              )
  )
)
} else
{
  LikelihoodTab <- c()
}
