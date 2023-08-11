source("uiEffect.R")
source("uiWorld.R")
source("uiQuickHyp.R")

HypothesisTab <-
  
  wellPanel(id="HypothesisTabset",
            style = paste("background: ",panelcolours$hypothesisC), 
            # h5("Hypothesis"),
            fluidRow(headerText("Build a hypothesis: variables & effect-size")),
            tabsetPanel(id="Hypothesis",
                        # Hypothesis tab
                        tabPanel("Hypothesis:",value="Hypothesis"
                        ),
                        
                        # variables tab
                        tabPanel("Variables",value="Variables",
                                 style = paste("background: ",subpanelcolours$hypothesisC), 
                                 conditionalPanel(condition="input.Using!='OK'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "40%", selectInput("Using", label = NULL,
                                                                                                  choices=c("Simulations"="Simulations","OK"="OK"),
                                                                                                  selected="OK",
                                                                                                  selectize=FALSE
                                                               )),
                                                               tags$td(width = "60%", tags$div(style = localStyle, " ")),
                                                             )
                                                  )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "5%", tags$div(style = localStyle, "IV:")),
                                              tags$td(width = "45%", selectInput("IVchoice", label = NULL,
                                                                                 choices=IV$name,
                                                                                 selected=IV$name,
                                                                                 selectize=FALSE
                                              )),
                                              tags$td(width = "25%", actionButton("editIV","Edit IV")),
                                              tags$td(width = "25%", tags$div(style = localStyle, " ")),
                                              tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                              tags$td(width = "10%", actionButton("inspectIV","i")),
                                            ),
                                            tags$tr(
                                              tags$td(width = "5%", tags$div(style = localStyle, "IV2:")),
                                              tags$td(width = "45%", selectInput("IV2choice", label = NULL,
                                                                                 choices=IV2$name,
                                                                                 selected=IV2$name,
                                                                                 selectize=FALSE
                                              )),
                                              tags$td(width = "25%", 
                                                      conditionalPanel(condition = "input.IV2choice != 'none'",
                                                                       actionButton("editIV2","Edit IV2")
                                                      )),
                                              tags$td(width = "5%", tags$div(style = localStyle, " "))
                                            ),
                                            tags$tr(
                                              tags$td(width = "5%", tags$div(style = localStyle, "DV:")),
                                              tags$td(width = "45%", selectInput("DVchoice", label = NULL,
                                                                                 choices=DV$name,
                                                                                 selected=DV$name,
                                                                                 selectize=FALSE
                                              )),
                                              tags$td(width = "25%", actionButton("editDV","Edit DV")),
                                              tags$td(width = "25%", tags$div(style = localStyle, " ")),
                                              tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                              tags$td(width = "10%", actionButton("inspectDV","i")),
                                            ),
                                 )
                        ),
                        
                        # prediction tab
                        tabPanel("Effects",id="Effects",
                                 style = paste("background: ",subpanelcolours$hypothesisC), 
                                 effectPanel(""),
                        ),
                        
                        # world tab
                        worldPanel(),
                        
                        # options tab
                        tabPanel("#",
                                 style = paste("background: ",subpanelcolours$hypothesisC),
                                 conditionalPanel(condition="input.Using=='Data'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "45%", tags$div(style = localPlainStyle, "Allow Resampling:")),
                                                               tags$td(width = "30%", 
                                                                       checkboxInput("AllowResampling",label=NULL,value=switches$doBootstrap),
                                                               ),
                                                               tags$td(width = "25%")                                        )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.Using!='Data'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "45%", tags$div(style = localPlainStyle, "Heteroscedasticity:")),
                                                               tags$td(width = "30%", 
                                                                       numericInput("Heteroscedasticity",label=NULL,value=effect$Heteroscedasticity,min=-2, max=2, step=0.1),
                                                               ),
                                                               tags$td(width = "25%")
                                                             ),
                                                             tags$tr(
                                                               tags$td(width = "45%", tags$div(style = localPlainStyle, "Residuals:")),
                                                               tags$td(width = "30%", 
                                                                       selectInput("ResidDistr",label=NULL,
                                                                                   choices=list("normal"="normal","Cauchy"="Cauchy","uniform"="uniform"),selected=effect$ResidDistr,selectize=FALSE),
                                                               ),
                                                               tags$td(width = "25%")
                                                             )
                                                  )
                                 ),
                                 quickHypotheses
                        ),
                        # help tab
                        tabPanel(helpChar,value="?",
                                 style = paste("background: ",subpanelcolours$hypothesisC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle, 
                                                       tags$br(HTML("<b>"),"Variables:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. choose one or two IVs and a DV by name'),
                                                       tags$br(HTML('&emsp;'), '2. edit the variable name/type/details if needed'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'eg. mean, sd , skew, kurtosis'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'or no cases, case names, proportions'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Hypothesis diagram)'),
                                                       tags$br(HTML("<b>"),"Effects: ",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '3. select effect size or sizes (for 2 IVs)'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'these are normalized and range from -1 to +1'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Population or Prediction diagram)')
                                              ),
                                            )
                                 )
                        )
            )
  )
