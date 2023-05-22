# world tab

metaPanel<-function(prefix="") {
  tabPanel("MetaAnalysis",value="MetaAnalysis",
           style = paste("background: ",subpanelcolours$evidenceC),
           wellPanel(
             style = paste("background: ",subpanelcolours$simulateC,";"),
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "30%",tags$div(style = localStyle, "Source:")
                          )
                        ),
                        tags$tr(
                          tags$td(width = "30%", tags$div(style = localPlainStyle, "No studies:")
                          ),
                          tags$td(width = "30%", tags$div(style = localPlainStyle, ""),
                                  numericInput(paste0(prefix,"meta_nStudies"), label=NULL,value=metaAnalysis$nstudies,step=100)
                          ),
                          tags$td(width = "15%"),
                          tags$td(width = "5%"),
                          tags$td(width = "15%", tags$div(style = localPlainStyle, "sigOnly ")
                          ),
                          tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                  checkboxInput(paste0(prefix,"meta_psigStudies"), label=NULL,value=metaAnalysis$sig_only)
                          ),
                        ),
                        tags$tr(
                          tags$td(width = "30%",tags$div(style = localStyle, "Analysis:")
                          )
                        ),
                        tags$tr(
                          tags$td(width = "30%",
                                  selectInput(paste0(prefix,"meta_fixedAnal"),label=NULL,
                                              choices=c("fixed","random"),
                                              selected=metaAnalysis$meta_fixedAnal,
                                              selectize=FALSE
                                  )
                          ),
                          tags$td(width = "30%",
                                  selectInput(paste0(prefix,"meta_pdf"),label=NULL,
                                              choices=c("All","Single","Gauss","Exp"),
                                              selected=metaAnalysis$meta_pdf,
                                              selectize=FALSE
                                  )
                          ),
                          tags$td(width = "15%", tags$div(style = localPlainStyle, "+nulls")),
                          tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                  checkboxInput(paste0(prefix,"meta_nullAnal"), label=NULL,value=metaAnalysis$meta_nullAnal)
                          ),
                          tags$td(width = "15%", tags$div(style = localPlainStyle, "sigOnly")),
                          tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                  checkboxInput(paste0(prefix,"meta_psigAnal"), label=NULL,value=metaAnalysis$meta_psigAnal)
                          ),
                        ),
                        tags$tr(
                          conditionalPanel(condition="input.meta_pdf == 'All'",
                                           tags$td(width = "30%",tags$div(style = localStyle, "Show:")
                                           )
                          ),
                          tags$td(width = "30%",
                                  conditionalPanel(condition="input.meta_pdf == 'All'",
                                                   selectInput(paste0(prefix,"meta_showAnal"),label=NULL,
                                              choices=c("All","Single","Gauss","Exp"),
                                              selected=metaAnalysis$meta_showAnal,
                                              selectize=FALSE
                                  )
                                  )
                          ),
                          tags$td(width = "30%",
                                  selectInput(paste0(prefix,"meta_showParams"),label=NULL,
                                              choices=c("n-k","S-k","S-S"),
                                              selected=metaAnalysis$meta_showParams,
                                              selectize=FALSE
                                  )
                          ),
                        )
             ),
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "15%", tags$div(style = localStyle, "Runs:")),
                          tags$td(width = "35%", 
                                  selectInput(paste0(prefix,"meta_runlength"),label=NULL,
                                              c("1" = "1",
                                                "2" = "2",
                                                "10" = "10",
                                                "50" = "50",
                                                "100" = "100",
                                                "250" = "250",
                                                "500" = "500",
                                                "1000" = "1000"),
                                              selected = "1",
                                              selectize=FALSE)
                          ),
                          tags$td(width = "15%", tags$div(style = localStyle, "")),
                          tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                          tags$td(width = "5%", checkboxInput(paste0(prefix,"meta_append"), label=NULL,value=metaAnalysis$append)),
                          tags$td(width = "20%",actionButton(paste0(prefix,"metaRun"), "Run")
                          )
                        )
             )
           )
  )
}
if (switches$doMetaAnalysis){
  metaAnalysisPanel<-metaPanel()
} else {
  metaAnalysisPanel<-c()
}

