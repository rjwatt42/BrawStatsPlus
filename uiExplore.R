source("uiExploreMeta.R")

hypothesisChoicesV3Plain=list("Variables"=list("IV" = "IV",
                                         "IV2" = "IV2",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType")
)

hypothesisChoicesV3=list("Variables"=list("IV" = "IV",
                                         "IV2" = "IV2",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType"),
                        "Effects"=list("Effect Size1" = "EffectSize1",
                                       "Effect Size2" = "EffectSize2",
                                       "Interaction" = "Interaction",
                                       "Covariation" = "Covariation",
                                       "Heteroscedasticity" = "Heteroscedasticity"
                        )
)

hypothesisChoicesV2Plain=list("Variables"=list("IV" = "IV",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType")
)

hypothesisChoicesV2=list("Variables"=list("IV" = "IV",
                                          "DV" = "DV",
                                          "IV/DV Types" = "IVDVType"),
                         "Effects"=list("Effect Size" = "EffectSize",
                                        "Heteroscedasticity" = "Heteroscedasticity")
)

if (switches$doWorlds) {
  hypothesisChoicesV2<-list(hypothesisChoicesV2,
                              list("Worlds"=worldsList)
                              )
}

hypothesisChoicesV2Extra=list("Variables"=list("IV" = "IV",
                                          "DV" = "DV",
                                          "IV/DV Types" = "IVDVType"),
                         "Effects"=list("Effect Size" = "EffectSize",
                                        "Heteroscedasticity" = "Heteroscedasticity"),
                         "Worlds"=worldsList
)


variableChoices=list("& type"="Type",
                     "& skew"="skew",
                     "& kurtosis"="kurtosis",
                     "& cats"="cats",
                     "& levels"="levels",
                     "& proptn"="prop"
)

designChoices=list("Sampling"=list("Sample Size" = "SampleSize",
                                   "Sampling Method" = "Method",
                                   "Sample Usage" = "Usage"),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "IV Range" = "IVRange",
                                    "DV Range" = "DVRange")
)
if (switches$doCheating) {
  designChoices<-c(designChoices,list("Cheating"=list("Method" = "Cheating",
                                                      "Cheating amount" = "CheatingAmount"))
  )
}
if (switches$doReplications) {
  designChoices<-c(designChoices,list("Replications"=list("SigOnly"="SigOnly",
                                                          "Repl Power"="Power",
                                                          "Repl Repeats" = "Repeats"))
  )
}

designChoicesExtra=list("Sampling"=list("Sample Size" = "SampleSize",
                                   "Sampling Method" = "Method",
                                   "Sample Usage" = "Usage",
                                   "Sample Gamma" = "SampleGamma",
                                   "Alpha" = "Alpha"),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "IV Range" = "IVRange",
                                    "DV Range" = "DVRange"),
                   "Cheating"=list("Method" = "Cheating",
                                   "Cheating amount" = "CheatingAmount"),
                   "Replications"=list("SigOnly"="SigOnly",
                                       "Repl Power"="Power",
                                       "Repl Repeats" = "Repeats")
)
names(designChoicesExtra$Sampling)[5]<-alphaChar

effectChoices=list("IV1-DV"="MainEffectIV",
                   "IV2-DV"="MainEffectIV2",
                   "IV1xIV2-DV"="InteractionEffect")

showInfer<-list("p-value" = "p",
                "p(sig)" = "p(sig)",
                "Power" = "w",
                "NHST errors" = "NHSTErrors"
                )

showWorlds<-list("False Discovery" = "FDR",
                 "FDR & FMR"="FDR;FMR",
                 "Sample Size"="SampleSize"
)

showLike<-list("log(lrs)" = "log(lrs)",
               "log(lrd)" = "log(lrd)",
               "likelihood" = "likelihood"
)

showChoicesExtra=list("Describe" = list("Effect Size" = "EffectSize"),
                 "Infer" = showInfer,
                 "Worlds" = showWorlds,
                 "Lk" = showLike
)
use<-c("Describe","Infer")
if (switches$doWorlds) use<-c(use,"Worlds")
if (switches$doLikelihoodInfer) use<-c(use,"Lk")
showChoices<-showChoicesExtra[use]

if (switches$doVariablesExplore) {
  showChoices<-c(showChoices,
                 list("Variables"=list("mean(IV)","sd(IV)","skew(IV)","kurtosis(IV)",
                                       "mean(DV)","sd(DV)","skew(DV)","kurtosis(DV)")
                 )
  )
} 


extraShowChoices=c("direct"="direct",
                   "unique"="unique",
                   "total"="total",
                   "all"="all")

whichShowChoices=c("Main 1" = "Main 1",
                   "Main 2" = "Main 2",
                   "Interaction" = "Interaction",
                   "Mains" = "Mains",
                   "All" = "All")

exploreLengthChoices=c("10" = "10",
                       "50" = "50",
                       "100" = "100",
                       "500" = "500",
                       "1000" = "1000",
                       "10000"="10000"
                       )

ExploreTab <-
    wellPanel(id="uiExplore",
            style = paste("background: ",panelcolours$exploreC), 
            fluidRow(headerText("Explore design decisions")),
            tabsetPanel(type="tabs",id="ExploreTab",
                        # sampling tab
                        tabPanel("Explore:",value="Explore"
                        ),
                        tabPanel("Hypothesis",id="ExH",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeH",label=NULL,
                                                                    hypothesisChoicesV3,selectize=FALSE)
                                                ),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.Explore_typeH == 'IV' || input.Explore_typeH == 'DV' || input.Explore_typeH == 'IV2'",
                                                                         selectInput("Explore_VtypeH",label=NULL,
                                                                    variableChoices,selectize=FALSE)
                                                        )
                                                ),
                                                tags$td(width = "25%")
                                                # tags$td(id="Explore_esRangeLabel",width = "25%", tags$div(style = localStyle, "range:")),
                                                # tags$td(width = "25%", 
                                                #         conditionalPanel(condition="input.Explore_typeH != 'IV'",
                                                #                          numericInput("Explore_esRange", label=NULL,value=0.8)
                                                #         )
                                                # ),
                                              ),
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_showH", label=NULL,
                                                                    showChoices,selectize = FALSE)
                                                ),
                                                tags$td(width = "15%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_whichShowH", label=NULL,
                                                                    whichShowChoices, selected="Main 1",selectize = FALSE)
                                                )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_typeShowH", label=NULL,
                                                                    extraShowChoices, selected="direct",selectize = FALSE)
                                                )),
                                                conditionalPanel(condition="Explore_showH == 'p(sig)' || input.Explore_typeH == 'p' || input.Explore_typeH == 'FDR'",
                                                                 tags$td(width = "5%", tags$div(style = localStyle, "log"))
                                                ),
                                                conditionalPanel(condition="Explore_showH == 'p(sig)' || input.Explore_typeH == 'p' || input.Explore_typeH == 'FDR'",
                                                                 tags$td(width = "5%", checkboxInput("Explore_ylogH",label="",value=FALSE))
                                                ),
                                              )),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                tags$td(width = "30%", 
                                                        selectInput("Explore_lengthH", label=NULL,
                                                                    exploreLengthChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                tags$td(width = "10%", checkboxInput("ExploreAppendH", label=NULL)),
                                                tags$td(width = "20%", actionButton("exploreRunH", "Run"))
                                              )
                                   )
                        ),
                        tabPanel("Design",id="ExD",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeD",label=NULL,
                                                                    designChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "15%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats' || input.Explore_typeD == 'CheatingAmount'",
                                                                         tags$div(style = localStyle, "max:")
                                                        )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats' || input.Explore_typeD == 'CheatingAmount'",
                                                                         numericInput("Explore_nRange", label=NULL,value=250,min=10,step=50)
                                                )),
                                                tags$td(width = "5%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats' || input.Explore_typeD == 'CheatingAmount' || input.Explore_typeD == 'Alpha'",
                                                                         tags$div(style = localStyle, "log")
                                                        )),
                                                tags$td(width = "5%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats' || input.Explore_typeD == 'CheatingAmount' || input.Explore_typeD == 'Alpha'",
                                                                         checkboxInput("Explore_xlogD",label="",value=FALSE)
                                                        )),
                                              ),
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_showD", label=NULL,
                                                                    showChoices,width="100%",selectize = FALSE)
                                                ),
                                                tags$td(width = "15%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_whichShowD", label=NULL,
                                                                    whichShowChoices, selected="Main 1",selectize = FALSE)
                                                )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_typeShowD", label=NULL,
                                                                    extraShowChoices, selected="direct",selectize = FALSE)
                                                )),
                                                conditionalPanel(condition="Explore_showD == 'p(sig)' || input.Explore_typeD == 'p' || input.Explore_typeD == 'FDR'",
                                                                 tags$td(width = "5%", tags$div(style = localStyle, "log"))
                                                                 ),
                                                conditionalPanel(condition="Explore_showD == 'p(sig)' || input.Explore_typeD == 'p' || input.Explore_typeD == 'FDR'",
                                                                 tags$td(width = "5%", checkboxInput("Explore_ylogD",label="",value=FALSE))
                                                                 ),
                                              )),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                tags$td(width = "30%", 
                                                        selectInput("Explore_lengthD", label=NULL,
                                                                    exploreLengthChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                tags$td(width = "10%", checkboxInput("ExploreAppendD", label=NULL)),
                                                tags$td(width = "20%", actionButton("exploreRunD", "Run"))
                                              )
                                   )
                        ),                        
                        # exploreMeta(),
                        tabPanel("#",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Analysis")),
                                                tags$td(width = "15%"),
                                                tags$td(width = "30%"),
                                                tags$td(width = "25%"),
                                                tags$td(width = "5%")
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localPlainStyle, "no points:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_npoints", label=NULL,value=13)
                                                ),
                                                tags$td(width="5%"),
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", id="Explore_esRangeLabel", tags$div(style = localPlainStyle, "r-range:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_esRange", label=NULL,value=0.8,step=0.1)
                                                ),
                                                tags$td(width = "30%", tags$div(style = localPlainStyle, "anom-range:")),
                                                tags$td(width = "25%", 
                                                        numericInput("Explore_anomRange", label=NULL,value=0.9,step=0.1)
                                                ),
                                                tags$td(width="5%")
                                                ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                                tags$td(width = "15%"),
                                                tags$td(width = "30%"),
                                                tags$td(width = "25%"),
                                                tags$td(width = "5%")
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localPlainStyle, "quantiles:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_quants", label=NULL,value=0.95, step = 0.01,min=0.01,max=0.99)
                                                ),
                                                tags$td(width = "30%", tags$div(style = localPlainStyle, "full y-lim:")),
                                                tags$td(width = "25%", checkboxInput("ExploreAny_ylim", label=NULL,value=explore$ExploreAny_ylim)),
                                                tags$td(width = "5%")
                                              ),
                                              # tags$tr(
                                              #   tags$td(width = "45%", tags$div(style = localPlainStyle, "long hand:")),
                                              #   tags$td(width = "5%"),
                                              #   tags$td(width = "45%", tags$div(style = localPlainStyle, "show theory:")),
                                              #   tags$td(width="5%",checkboxInput("exploreTheory",label=NULL,value=TRUE))
                                              # )
                                   )
                        )
                        # help tab
                        ,tabPanel(helpChar,value="?",
                                  style = paste("background: ",subpanelcolours$exploreC),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$div(style = helpStyle, 
                                                          tags$br(HTML('<b>'),"Before starting:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '1. set up a basic hypothesis with other panels'),
                                                          tags$br(HTML('<b>'),"Set up:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '2. choose the decision to explore'),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(these are split into separate groups)'),
                                                          tags$br(HTML('&emsp;'), '3. choose the outcome to examine'),
                                                          tags$br(HTML('<b>'),"Run: ",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '4. press the "Run" button'),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(can be slow - it is working hard!)'),
                                                 ),
                                               )
                                    )
                                  )

            )
                                                      
)
