source("uiBatchFiles.R")
source("uiMetaFiles.R")

FilesTab <-

wellPanel(
  style = paste("background: ",panelcolours$filesC), 
  # h5("Evidence"),
  fluidRow(headerText("Import and export files")),
  tabsetPanel(type="tabs",id="FileTab",
              # single tab
              tabPanel("Files:",id="Files"
              ),
              tabPanel("Hypothesis",
                       style = paste("background: ",subpanelcolours$filesC), 
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "import:")),
                                      tags$td(width = "90%", style="border-top: 1px solid black;margin-top:4px;padding-top:4px;",
                                              fileInput("wsInputFile",label=NULL,accept=c(".xlsx",".xls"),buttonLabel="Choose: ")
                                      ),
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-right: 1px solid black;")
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "sheet:")),
                                      tags$td(width = "90%", style="border-bottom: 1px solid black;",
                                              selectInput("wsInputSheet",label=NULL,choices=c("sheet name"),selectize = FALSE)
                                      ),
                                      tags$td(width = "5%",style="border-bottom: 1px solid black;border-right: 1px solid black;",
                                              actionButton("wsInputFileLoad","Load"))
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "export:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              textInput("wsOutputFile",label=NULL)),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("WSOutputFileSave","Save")),
                                    ),
                         ),
                         conditionalPanel(condition="switches.doClipboard",
                                          tags$table(width = "100%",class="myTable",
                                                     tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "clipboard:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              actionButton("wsPaste","Import")),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("wsCopy","Export")),
                                    ))
                         )
              ),
              tabPanel("Data",
                       style = paste("background: ",subpanelcolours$filesC), 
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "import:")),
                                      tags$td(width = "90%", style="border-top: 1px solid black;;height:20px;margin:0px;padding:0px;margin-top:4px;padding-top:4px;",
                                              fileInput("dataInputFile",label=NULL,accept=c(".xlsx",".xls"),buttonLabel="Choose: ")
                                      ),
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-right: 1px solid black;")
                                      ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "sheet:")),
                                      tags$td(width = "90%", style="border-bottom: 1px solid black;",
                                              selectInput("dataInputSheet",label=NULL,choices=c(""),selectize = FALSE)
                                      ),
                                      tags$td(width = "5%",style="border-bottom: 1px solid black;border-right: 1px solid black;", 
                                              actionButton("dataInputFileLoad","Load"))
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "export:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              textInput("dataOutputFile",label=NULL)),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("dataOutputFileSave","Save")),
                                    ),
                         ),
                         conditionalPanel(condition="switches.doClipboard",
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "clipboard:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              actionButton("dPaste","Import")),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("dCopy","Export")),
                                    ))
                         )
              ),
              batchTab
              # options tab
              ,tabPanel("#",value="##",
                        style = paste("background: ",subpanelcolours$filesC),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$td(width = "40%", tags$div(style = localPlainStyle, "Import Ordinals: ")),
                                       tags$td(width = "5%", checkboxInput("ImportOrdinals", value=switches$importOrdinals, label=NULL)),
                                       tags$td(width = "35%", tags$div(style = localPlainStyle, "Ordinal Range: ")),
                                       tags$td(width = "20%", numericInput("MaxOrdinal", value=9, label=NULL))
                                     ),
                                     tags$tr(
                                       tags$td(width = "40%", tags$div(style = localPlainStyle, "Export Header: ")),
                                       tags$td(width = "5%", checkboxInput("ExportHeader", value=FALSE, label=NULL)),
                                       tags$td(width = "35%", tags$div(style = localPlainStyle, "Short Form: ")),
                                       tags$td(width = "20%", checkboxInput("ExportShortForm", value=TRUE, label=NULL)),
                                     )
                          )
              )
              # help tab
              ,tabPanel(helpChar,value="?",
                        style = paste("background: ",subpanelcolours$filesC),

                                                  tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br("Import/export workspace/sample:"),
                                                tags$br(HTML('<b>'),"Importing:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. select the file (.xls .xlsx) and sheet'),
                                                tags$br(HTML('&emsp;'), '2. Press "Load"'),
                                                tags$br(HTML('&emsp;'), '3. go to Hypothesis:Variables to set up hypothesis'),
                                                tags$br(HTML('<b>'),"Exporting:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. provide a file name (.xlsx)'),
                                                tags$br(HTML('&emsp;'), '2. press "Save"'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'current workspace/sample is saved'),
                                       ),
                                     )
                          )
              )
  )
)
