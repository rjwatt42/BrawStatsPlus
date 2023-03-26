source("uiHypothesisPart.R")
source("uiDesignPart.R")
source("uiEvidencePart.R")

LGmodalExplore <-
  bsModal(id="LGmodalExplore", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3,
                     hypothesisPanel("LGExplore"),
                     designPanel("LGExplore"),
                     evidencePanel("LGExplore"),
                     
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       tabsetPanel(id="LGExploreShow", type="tabs",
                                   tabPanel("Explore:",value="Explore",
                                   ),
                                   # single tab
                                   tabPanel("Hypothesis",value="Hypothesis",id="uiLGHypothesis",
                                            style = paste("background: ",subpanelcolours$exploreC,";"),
                                            wellPanel(
                                              style = paste("background: ",subpanelcolours$exploreC,";"),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_typeH",label=NULL,
                                                                               hypothesisChoices3,selectize=FALSE)
                                                           ),
                                                           tags$td(width = "25%", 
                                                                   conditionalPanel(condition="input.LGExplore_typeH == 'IV' || input.LGExplore_typeH == 'DV' || input.LGExplore_typeH == 'IV2'",
                                                                                    selectInput("LGExplore_VtypeH",label=NULL,
                                                                                                variableChoices,selectize=FALSE)
                                                                   )
                                                           ),
                                                           tags$td(width = "25%")
                                                         ),
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_showH", label=NULL,
                                                                               showChoices,selectize = FALSE)
                                                           ),
                                                           tags$td(width = "25%", 
                                                                   conditionalPanel(condition="input.IV2choice != 'none'",
                                                                                    selectInput("LGExplore_whichShowH", label=NULL,
                                                                                                whichShowChoices, selected="Main 1",selectize = FALSE)
                                                                   )),
                                                           tags$td(width = "25%", 
                                                                   conditionalPanel(condition="input.IV2choice != 'none'",
                                                                                    selectInput("LGExplore_typeShowH", label=NULL,
                                                                                                extraShowChoices, selected="direct",selectize = FALSE)
                                                                   ))
                                                         )
                                              ),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                           tags$td(width = "30%", 
                                                                   selectInput("LGExplore_lengthH", label=NULL,
                                                                               exploreLengthChoices,selectize=FALSE)
                                                           ),
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                           tags$td(width = "10%", checkboxInput("LGExploreAppendH", label=NULL)),
                                                           tags$td(width = "20%", actionButton("LGexploreRunH", "Run"))
                                                         )
                                              )
                                            )
                                   ),
                                   tabPanel("Design",value="Design",id="uiLGDesign",
                                            style = paste("background: ",subpanelcolours$exploreC,";"),
                                            wellPanel(
                                              style = paste("background: ",subpanelcolours$exploreC,";"),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_typeD",label=NULL,
                                                                               designChoices,
                                                                               selected="Method",
                                                                               selectize=FALSE)
                                                           ),
                                                           tags$td(id="LGExplore_nRangeLabel",
                                                                   width = "15%", 
                                                                   conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize' || input.LGExplore_typeD == 'Repeats'",
                                                                                    tags$div(style = localStyle, "max:",id="LGExplore_max")
                                                                   )),
                                                           tags$td(width = "15%", 
                                                                   conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize' || input.LGExplore_typeD == 'Repeats'",
                                                                                    numericInput("LGExplore_nRange", label=NULL,value=250,min=10,step=50)
                                                                   )),
                                                           tags$td(width = "10%", 
                                                                   conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize'",
                                                                                    tags$div(style = localStyle, "log",id="LGExplore_log")
                                                                   )),
                                                           tags$td(width = "10%", 
                                                                   conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize'",
                                                                                    checkboxInput("LGExplore_xlog",label="",value=FALSE)
                                                                   )),
                                                         )
                                              ),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_showD", label=NULL,
                                                                               showChoices,width="100%",selectize = FALSE)
                                                           ),
                                                           tags$td(width = "25%", 
                                                                   conditionalPanel(condition="input.IV2choice != 'none'",
                                                                                    selectInput("LGExplore_whichShowD", label=NULL,
                                                                                                whichShowChoices, selected="Main 1",selectize = FALSE)
                                                                   )),
                                                           tags$td(width = "25%", 
                                                                   conditionalPanel(condition="input.IV2choice != 'none'",
                                                                                    selectInput("LGExplore_typeShowD", label=NULL,
                                                                                                extraShowChoices, selected="direct",selectize = FALSE)
                                                                   )),
                                                         )
                                              ),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                           tags$td(width = "30%", 
                                                                   selectInput("LGExplore_lengthD", label=NULL,
                                                                               exploreLengthChoices,selectize=FALSE)
                                                           ),
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                           tags$td(width = "10%", checkboxInput("LGExploreAppendD", label=NULL)),
                                                           tags$td(width = "20%", actionButton("LGexploreRunD", "Run"))
                                                         )
                                              )
                                            )
                                   ),
                                   tabPanel("MetaAnalysis",value="MetaAnalysis",id="uiLGMetaAnalysis",
                                            style = paste("background: ",subpanelcolours$exploreC,";"),
                                            wellPanel(
                                              style = paste("background: ",subpanelcolours$exploreC,";"),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_typeM",label=NULL,
                                                                               metaChoices,selectize=FALSE)
                                                           ),
                                                           tags$td(id="LGExplore_metaRangeLabel",width = "25%", tags$div(style = localStyle, "ns-range:")),
                                                           tags$td(width = "25%",
                                                                   numericInput("LGExplore_metaRange", label=NULL,value=10000)
                                                           ),
                                                           tags$td(width = "25%")
                                                         ),
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                           tags$td(width = "40%", 
                                                                   selectInput("LGExplore_showM", label=NULL,
                                                                               showMetaChoices,selectize = FALSE)
                                                           ),
                                                           tags$td(width = "25%"),
                                                           tags$td(width = "25%")
                                                         )
                                              ),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                           tags$td(width = "30%", 
                                                                   selectInput("LGExplore_lengthM", label=NULL,
                                                                               exploreLengthChoices,selectize=FALSE)
                                                           ),
                                                           tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                           tags$td(width = "10%", checkboxInput("LGExploreAppendM", label=NULL)),
                                                           tags$td(width = "20%", actionButton("LGexploreRunM", "Run"))
                                                         )
                                              )
                                            )
                                   ),
                                   tabPanel("#",value="#",id="uiLGOptions",
                                            style = paste("background: ",subpanelcolours$exploreC,";"),
                                            wellPanel(
                                              style = paste("background: ",subpanelcolours$exploreC,";"),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "no points:")),
                                                           tags$td(width = "15%", 
                                                                   numericInput("LGExplore_npoints", label=NULL,value=13)
                                                           ),
                                                           tags$td(width = "30%", id="Explore_esRangeLabel", tags$div(style = localPlainStyle, "r-range:")),
                                                           tags$td(width = "20%", 
                                                                   numericInput("LGExplore_esRange", label=NULL,value=0.8)
                                                           ),
                                                           tags$td(width="5%"),
                                                           tags$td(width="5%"),
                                                         ),
                                                         tags$tr(
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "quantiles:")),
                                                           tags$td(width = "15%", 
                                                                   numericInput("LGExplore_quants", label=NULL,value=0.95, step = 0.01,min=0.01,max=0.99)
                                                           ),
                                                           tags$td(width = "30%"),
                                                           tags$td(width = "20%"),
                                                           tags$td(width="5%"),
                                                           tags$td(width="5%")
                                                         ),
                                                         tags$tr(
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "full y-lim:")),
                                                           tags$td(width = "15%", checkboxInput("LGExploreFull_ylim", label=NULL,value=FALSE)),
                                                           tags$td(width = "30%", tags$div(style = localPlainStyle, "anom-range:")),
                                                           tags$td(width = "20%", 
                                                                   numericInput("LGExplore_anomRange", label=NULL,value=0.9)
                                                           ),
                                                           tags$td(width="5%"),
                                                           tags$td(width="5%")
                                                         )
                                              )
                                            )
                                   ),
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                    tags$td(width = "20%",actionButton("LGExploreClose","Close",class="other")),
                                  )
                       )
                     )
              ),# close of column
              column(offset=0,width=9,
                     plotOutput("LGExploreShowOutput",height=LGGraphHeight)
                     )
              )
          )
  )
LGmodalExplore$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

