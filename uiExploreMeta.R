
worldsList<-list("pdf"="PDF","k"="k","pNull"="pNull")
names(worldsList)[3]<-pPlusLabel
names(worldsList)[2]<-Lchar

metaChoices=list("meta"=list("NoStudies"="NoStudies",
                             "Sample Size"="SampleSize",
                             "SampleGamma"="SampleGamma",
                             "sig_only"="sig_only"
                        ),
                  "Worlds"=worldsList
)
showMetaChoices=c(worldsList, "log(lk)"="S")

exploreLengthChoices=c("10" = "10",
                       "50" = "50",
                       "100" = "100",
                       "500" = "500",
                       "1000" = "1000"
)

exploreMeta<-function(){
    tabPanel("MetaAnalysis",id="ExM",
             style = paste("background: ",subpanelcolours$exploreC),
               tags$table(width = "100%",class="myTable",
                          tags$tr(
                            tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                            tags$td(width = "40%",
                                    selectInput("Explore_typeM",label=NULL,
                                                metaChoices,selectize=FALSE)
                            ),
                            conditionalPanel(condition="input.Explore_typeM == 'NoStudies' | input.Explore_typeM=='SampleSize'",
                                             tags$td(id="Explore_metaRangeLabel",width = "15%", tags$div(style = localStyle, "max:"))
                            ),
                            conditionalPanel(condition="input.Explore_typeM == 'NoStudies' | input.Explore_typeM=='SampleSize'",
                                             tags$td(width = "25%",
                                                     numericInput("Explore_metaRange", label=NULL,value=10000)
                                             )
                            ),
                            tags$td(width = "5%", 
                                    conditionalPanel(condition="input.Explore_typeM == 'NoStudies' | input.Explore_typeM=='SampleSize'",
                                                     tags$div(style = localStyle, "log")
                                    )),
                            tags$td(width = "5%", 
                                    conditionalPanel(condition="input.Explore_typeM == 'NoStudies' | input.Explore_typeM=='SampleSize'",
                                                     checkboxInput("Explore_Mxlog",label="",value=TRUE)
                                    )),
                          ),
                          tags$tr(
                            tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                            tags$td(width = "40%",
                                    selectInput("Explore_showM", label=NULL,
                                                showMetaChoices,width="100%",selectize = FALSE)
                            )
                          )),
               tags$table(width = "100%",class="myTable",
                          tags$tr(
                            tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                            tags$td(width = "30%",
                                    selectInput("Explore_lengthM", label=NULL,
                                                exploreLengthChoices,width="100%",selectize=FALSE)
                            ),
                            tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                            tags$td(width = "10%", checkboxInput("ExploreAppendM", label=NULL)),
                            tags$td(width = "20%", actionButton("exploreRunM", "Run"))
                          )
               )
    ) 
}

