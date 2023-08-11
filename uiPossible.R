source("uiPrior.R")

SpossibleLengthChoices=c("100" = "100",
                          "250" = "250",
                          "500" = "500",
                          "1000" = "1000"
)
PpossibleLengthChoices=c("10" = "10",
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

if (switches$doPossiblePower) {
  possibleShows<-c("Normal" = "Normal",
                   "Inverse" = "Inverse",
                   "Power" = "Power")
} else {
  possibleShows<-c("Normal" = "Normal",
    "Inverse" = "Inverse")
}

if (switches$doPossible) {
PossibleTab <-

  wellPanel(ID="MainPossible",
    style = paste("background: ",panelcolours$possibleC), 
    # h5("Evidence"),
  fluidRow(headerText("Likelihood functions based on sample or population")),
  tabsetPanel(type="tabs",id="PossiblePanel",
              # single tab
              tabPanel("Possible:"
              ),
              tabPanel("Samples",
                       style = paste("background: ",subpanelcolours$possibleC), 
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "20%", numericInput("possibleSampRho", label=NULL,min=-1,max=1, step=0.1,value=possible$targetSample)),
                                      tags$td(width = "15%",tags$div(style = localStyle, "from:")),
                                      tags$td(width = "25%",selectInput("possibleUseSource",label=NULL,
                                                                        choices=source2,selected=possible$UseSource,
                                                                        selectize=FALSE)
                                      ),
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "35%", tags$div(style = localStyle, "cut:")),
                                      tags$td(width = "15%", checkboxInput("possible_cutaway",label=NULL,value=possible$cutaway)),
                                      tags$td(width = "35%", tags$div(style = localStyle, "Sig Only:")),
                                      tags$td(width = "15%", checkboxInput("possible_sigonly",label=NULL,value=possible$sigOnly))
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("possible_length", label=NULL,
                                                          SpossibleLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("possible_append", label=NULL,value=possible$appendSim)),
                                      tags$td(width = "10%", actionButton("possible_run", "Run"))
                                    )
                         ),
              ),
              tabPanel("Populations",
                       style = paste("background: ",subpanelcolours$possibleC), 
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "20%", numericInput("possiblePSampRho", label=NULL,min=-1,max=1, step=0.05,value=possible$targetSample)),
                                      tags$td(width = "15%",tags$div(style = localStyle, "prior:")),
                                      tags$td(width = "25%",selectInput("possibleUsePrior",label=NULL,
                                                                        choices=source1,selected=possible$UsePrior,
                                                                        selectize=FALSE)
                                              ),
                                    )
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("possibleP_length", label=NULL,
                                                          PpossibleLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("possibleP_append", label=NULL,value=possible$appendSim)),
                                      tags$td(width = "10%", actionButton("possibleP_run", "Run")),
                                    )
                         ),
              ),
              tabPanel("Prior",
                       style = paste("background: ",subpanelcolours$possibleC), 
                         priorPanel("possible",asTable=TRUE),
              ),
              tabPanel("#",
                       style = paste("background: ",subpanelcolours$possibleC), 
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Analysis")),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%")
                                    ),
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "20%", tags$div(style = localPlainStyle, "sim slice:")),
                                      tags$td(width = "20%",numericInput("possibleSimSlice",label=NULL,value=possible$possibleSimSlice,max=0.2,min=0.0001,step=0.01)),
                                      tags$td(width = "25%", tags$div(style = localPlainStyle, "correction:")),
                                      tags$td(width = "10%",checkboxInput("possibleCorrection", value=possible$possibleCorrection, label=NULL)),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "HQ:")),
                                      tags$td(width = "10%",checkboxInput("possibleHQ", value=possible$possibleHQ, label=NULL))
                                    ),
                                    
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%"),
                                      tags$td(width = "25%")
                                    ),
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "view:")),
                                      tags$td(width = "20%", 
                                              selectInput("possibleView", label=NULL,
                                                          c("3D" = "3D",
                                                            "2D" = "2D"),selected=possible$view,selectize=FALSE)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "show:")),
                                      tags$td(width = "30%", 
                                              selectInput("possibleShow", label=NULL,
                                                          choices=possibleShows,
                                                          selected=possible$show,selectize=FALSE)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "box:")),
                                      tags$td(width = "5%", 
                                              checkboxInput("possibleBoxed", label=NULL,value=possible$boxed)
                                              ),
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "az:")),
                                      tags$td(width = "20%", 
                                              numericInput("possibleAzimuth",label=NULL,
                                                           min = -180,
                                                           max = 180,
                                                           step = 5,
                                                           value = possible$azimuth)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "elev:")),
                                      tags$td(width = "20%", 
                                              numericInput("possibleElevation",label=NULL,
                                                           min = 0,
                                                           max = 90,
                                                           step = 5,
                                                           value = possible$elevation)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "r:")),
                                      tags$td(width = "15%", 
                                              numericInput("possibleRange",label=NULL,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1,
                                                           value = possible$range)
                                      ),
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localPlainStyle, "")),
                                      tags$td(width = "45%",tags$div(style = localPlainStyle, "show theory:")),
                                      tags$td(width = "5%", checkboxInput("possibleTheory", value=possible$possibleTheory, label=NULL))
                                    ),
                         )
              )
              # help tab
              ,tabPanel("?",
                        style = paste("background: ",subpanelcolours$possibleC),
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
} else
{
  PossibleTab <- c()
}
