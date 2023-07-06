source("uiEffectPart.R")
source("uiDesignPart.R")

LGmodalMeta <-
  bsModal(id="LGmodalMeta", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     hypothesisPanel("LGMeta"),
                     designPanel("LGMeta"),
                     
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tabsetPanel(id="LGEvidenceShow", type="tabs",
                                   tabPanel("Evidence:",value="Evidence" ),
                                   # single tab
                                   metaPanel("LG"),
                                   tabPanel("#",id="EvidenceOptions",
                                            style = paste("background: ",subpanelcolours$simulateC),
                                            wellPanel(
                                              style = paste("background: ",subpanelcolours$simulateC,";"),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "p-scale:")),
                                                           tags$td(width = "25%", selectInput("LGpScale", label=NULL, c("linear"="linear","log10"="log10"), selected=evidence$pScale, selectize=FALSE)),
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "w-scale:")),
                                                           tags$td(width = "25%", selectInput("LGwScale", label=NULL, c("linear"="linear","log10"="log10"), selected=evidence$wScale, selectize=FALSE)),
                                                         ),
                                                         tags$tr(
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "n-scale:")),
                                                           tags$td(width = "25%", selectInput("LGnScale", label=NULL, c("linear"="linear","log10"="log10"), selected=evidence$nScale, selectize=FALSE)),
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, "")),
                                                           tags$td(width = "25%", tags$div(style = localPlainStyle, ""))
                                                         ),
                                              ),
                                              tags$table(width = "100%",class="myTable",
                                                         tags$tr(
                                                           tags$td(width = "50%", tags$div(style = localPlainStyle, "")),
                                                           tags$td(width = "45%", tags$div(style = localPlainStyle, "show theory:")),
                                                           tags$td(width = "5%", checkboxInput("LGevidenceTheory",label=NULL,value=evidence$showTheory)),
                                                         )),
                                            )
                                   ),
                                   selected="MetaAnalysis"
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                    tags$td(width = "20%",actionButton("LGMetaClose","Close")),
                                  )
                       )
                     )
              ),# close of column
              column(width=9,plotOutput("LGMetaShowOutput",height=LGGraphHeight))
            ) 
          )
  )
LGmodalMeta$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

