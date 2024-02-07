# meta-analysis panel

metaAnalysisPanel<-function(){
  MetAnalPanel<-tabPanel("MetaAnal",value="MetaAnalysis",
                         style = paste("background: ",subpanelcolours$evidenceC),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%",tags$div(style = localStyle, "Source:")
                                      ),
                                      tags$td(width = "70%")
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localPlainStyle, "No studies:")
                                      ),
                                      tags$td(width = "30%", tags$div(style = localPlainStyle, ""),
                                              numericInput("meta_nStudies", label=NULL,value=metaAnalysis$nstudies,step=100)
                                      ),
                                      # tags$td(width = "15%"),
                                      # tags$td(width = "5%"),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "sigOnly ")
                                      ),
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                              checkboxInput("meta_psigStudies", label=NULL,value=metaAnalysis$sig_only)
                                      ),
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%",tags$div(style = localStyle, "Analysis:")
                                      ),
                                      tags$td(width = "30%",
                                              selectInput("meta_pdf",label=NULL,
                                                          choices=c("All","Single","Gauss","Exp"),
                                                          selected=metaAnalysis$meta_pdf,
                                                          selectize=FALSE
                                              )
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "+nulls")),
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                              checkboxInput("meta_nullAnal", label=NULL,value=metaAnalysis$meta_nullAnal)
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, "sigOnly")),
                                      tags$td(width = "5%", tags$div(style = localPlainStyle, ""),
                                              checkboxInput("meta_psigAnal", label=NULL,value=metaAnalysis$meta_psigAnal)
                                      )
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%",tags$div(style = localStyle, "Show:")
                                      ),
                                      tags$td(width = "30%",
                                              conditionalPanel(condition="input.meta_pdf == 'All'",
                                                               selectInput("meta_showAnal",label=NULL,
                                                                           choices=c("All","Single","Gauss","Exp"),
                                                                           selected=metaAnalysis$meta_showAnal,
                                                                           selectize=FALSE
                                                               )
                                              )
                                      ),
                                      tags$td(width = "25%",
                                              selectInput("meta_showParams",label=NULL,
                                                          choices=c(paste0(Pchar,"-",Lchar),paste0("S-",Lchar),"S-S"),
                                                          selected=metaAnalysis$meta_showParams,
                                                          selectize=FALSE
                                              )
                                      ),
                                      tags$td(width = "15%", tags$div(style = localPlainStyle, ""))
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "25%", 
                                              selectInput("meta_runlength",label=NULL,
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
                                      tags$td(width = "10%", tags$div(style = localPlainStyle, "")),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "5%", checkboxInput("meta_append", label=NULL,value=metaAnalysis$append)),
                                      tags$td(width = "10%",actionButton("metaRun", "Run")
                                      )
                                    )
                         )
  )
  
  if (switches$doMetaAnalysis){
    MetAnalTab<-MetAnalPanel
  } else {
    MetAnalTab<-c()
  }
  return(MetAnalTab)
}
