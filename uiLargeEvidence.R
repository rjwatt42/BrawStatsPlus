source("uiHypothesisPart.R")
source("uiDesignPart.R")

LGmodalEvidence <-
  bsModal(id="LGmodalEvidence", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     hypothesisPanel("LGEvidence"),
                     designPanel("LGEvidence"),
                     
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tabsetPanel(id="LGEvidenceShow", type="tabs",
                                   tabPanel("Evidence:",value="Evidence",
                                   ),
                                   # single tab
                                   tabPanel("Single",value="Single",id="uiSingle",
                                            style = paste("background: ",subpanelcolours$simulateC,";"),
                                            wellPanel(id="Single",
                                                      style = paste("background: ",subpanelcolours$simulateC,";"),
                                                      tags$table(width = "100%",class="myTable",
                                                                 tags$tr(
                                                                   tags$td(width = "15%", tags$div(style = localStyle, "Show:")),
                                                                   tags$td(width = "35%", 
                                                                           selectInput("LGEvidenceInfer_type",label=NULL,
                                                                                       c("Basic" = "EffectSize",
                                                                                         "Power" = "Power",
                                                                                         "log(lrs)" = "log(lrs)",
                                                                                         "log(lrd)" = "log(lrd)"),
                                                                                       selectize=FALSE)
                                                                   ),
                                                                   tags$td(width = "25%"), # actionButton("LGEvidenceEvidenceHypothesisApply", "Analyze")),
                                                                   tags$td(width = "25%", actionButton("LGEvidencenewSample", "New Sample"))
                                                                 )
                                                      )
                                            )
                                   ),
                                   tabPanel("Multiple",value="Multiple",id="uiMultiple",
                                            style = paste("background: ",subpanelcolours$simulateC,";"),
                                            wellPanel(id="Multiple",
                                                      style = paste("background: ",subpanelcolours$simulateC,";"),
                                                      tags$table(width = "100%",class="myTable",
                                                       tags$tr(
                                                         tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                         tags$td(width = "40%", 
                                                                 selectInput("LGEvidenceExpected_type",label=NULL,
                                                                             c("Basic" = "EffectSize",
                                                                               "Power" = "Power",
                                                                               "NHST errors" = "NHSTErrors",
                                                                               "CI limits" = "CILimits",
                                                                               "log(lrs)" = "log(lrd)",
                                                                               "log(lrd)" = "log(lrs)",
                                                                               "2D"="2D"
                                                                             ),
                                                                             selectize=FALSE),
                                                         ),
                                                         tags$td(width = "25%",
                                                                                  selectInput("LGEvidenceExpected_par1", label=NULL, 
                                                                                              list("Basic"=list("r"="r","p"="p","log(lrs)"="log(lrs)","log(lrd)"="log(lrd)"),
                                                                                                   "Power"=list("w"="w","nw"="nw"),
                                                                                                   "World"=list("n"="n","rp"="rp","wp"="wp"),
                                                                                                   "Replication"=list("r1"="r1","p1"="p1")
                                                                                              ),
                                                                                              selected="p", selectize=FALSE),
                                                         ),
                                                         tags$td(width = "25%",
                                                                                  selectInput("LGEvidenceExpected_par2", label=NULL, 
                                                                                              list("Basic"=list("r"="r","p"="p","log(lrs)"="log(lrs)","log(lrd)"="log(lrd)"),
                                                                                                   "Power"=list("w"="w","nw"="nw"),
                                                                                                   "World"=list("n"="n","rp"="rp","wp"="wp"),
                                                                                                   "Replication"=list("r1"="r1","p1"="p1")
                                                                                              ),
                                                                                              selected="r", selectize=FALSE),
                                                         )
                                                       ),
                                                       tags$tr(
                                                         tags$td(width = "10%", tags$div(style = localStyle, "")),
                                                         tags$td(width = "40%", 
                                                                 conditionalPanel(condition="input.IV2choice != 'none'",
                                                                                  selectInput("LGEvidenceEffect_type",label=NULL,
                                                                                              c("direct" = "direct",
                                                                                                "unique" = "unique",
                                                                                                "total" = "total",
                                                                                                "all" = "all",
                                                                                                "coefficients" = "coefficients"),
                                                                                              selectize=FALSE)
                                                                 )
                                                         ),
                                                         tags$td(width = "25%", tags$div(style = localStyle, "")),
                                                         tags$td(width = "25%", tags$div(style = localStyle, "")),
                                                       )
                                                       ),
                                                      tags$table(width = "100%",class="myTable",
                                                                 tags$tr(
                                                         tags$td(width = "15%", tags$div(style = localStyle, "Runs:")),
                                                         tags$td(width = "50%", 
                                                                 selectInput("LGEvidenceExpected_length",label=NULL,
                                                                             c("10" = "10",
                                                                               "50" = "50",
                                                                               "100" = "100",
                                                                               "250" = "250",
                                                                               "500" = "500",
                                                                               "1000" = "1000"),
                                                                             selected = "10",
                                                                             selectize=FALSE)
                                                         ),
                                                         tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                         tags$td(width = "5%", checkboxInput("LGEvidenceExpected_append", label=NULL)),
                                                         tags$td(width = "20%",actionButton("LGEvidenceExpectedRun", "Run")
                                                         )
                                                       )
                                                      )
                                            )
                                   ),
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
                                                            tags$td(width = "45%", tags$div(style = localPlainStyle, "long hand:")),
                                                            tags$td(width = "5%", checkboxInput("LGevidenceLongHand",label=NULL,value=evidence$longHand)),
                                                            tags$td(width = "45%", tags$div(style = localPlainStyle, "show theory:")),
                                                            tags$td(width = "5%", checkboxInput("LGevidenceTheory",label=NULL,value=evidence$showTheory)),
                                                          )),
                                             )
                                   )                       
                                   )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                    tags$td(width = "20%",actionButton("LGEvidenceClose","Close")),
                                  )
                       )
                     )
              ),# close of column
              column(width=9,
                     tabsetPanel(id="LGEvidenceGraphs", type="tabs",
                                 # single tab
                                 tabPanel("Sample",value="Sample",id="uiLGSample",
                                          plotOutput("LGshowSampleOutput",height=LGGraphHeight)
                                          ),
                                 tabPanel("Describe",value="Describe",id="uiLGDescribe",
                                          plotOutput("LGshowDescribeOutput",height=LGGraphHeight)
                                          ),
                                 tabPanel("Infer",value="Infer",id="uiLGInfer",
                                          plotOutput("LGshowInferOutput",height=LGGraphHeight)
                                 ),
                                 tabPanel("Expect",value="Expect",id="uiLGExpect",
                                          plotOutput("LGshowExpectOutput",height=LGGraphHeight)
                                 )
                     )
              )
            )
          )
  )
LGmodalEvidence$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

