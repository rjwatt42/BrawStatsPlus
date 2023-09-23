HelpTab <-
  
  
  wellPanel(id="HelpTabset",
            style = paste("background: ",panelcolours$helpC,";","margin-left:0px"),
            fluidRow(headerText(HTML("Follow these 4 steps. The <strong>?</strong> tabs give more help."))),
            tabsetPanel(id="Help",
                        # Help tab
                        tabPanel("Help:",
                        ),
                        
                        # Step 1 tab
                        tabPanel("Step1",id="Step1",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"Make a Hypothesis:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. Variables: choose variables'),
                                                       tags$br(HTML('&emsp;'), '2. Effects: set expected effect-sizes'),
                                              )
                                            )
                                 )
                        ),
                        
                        # Step 2 tab
                        tabPanel("Step2",id="Step2",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"Set a Design:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. Sampling: choose sample size & method'),
                                                       tags$br(HTML('&emsp;'), '2. Anomalies: build in sampling anomalies'),
                                              )
                                            )
                                 )
                        ),
                        
                        # Step 3 tab
                        tabPanel("Step3",id="Step3",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"Evidence:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. Single: simulate a sample and analyse it'),
                                                       tags$br(HTML('&emsp;'), '2. Multiple: run many samples at once'),
                                              )
                                            )
                                 )
                        ),
                        
                        # Step 4 tab
                        tabPanel("Step4",id="Step4",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"Explore:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. Look at the consequences of your decisions'),
                                                       tags$br(HTML('&emsp;'), '2. Vary any of them and look at outputs'),
                                              )
                                            )
                                 )
                        ),
                        
                        # Files tab
                        tabPanel("More",id="More",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"Data:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. Export data to a file or clipboard'),
                                                       tags$br(HTML('&emsp;'), '2. Import data from a file or the clipboard'),
                                              )
                                            ),
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"  ",HTML("</b>"))
                                              )
                                            ),
                                            tags$tr(
                                              tags$div(style = helpStyle,
                                                       tags$br(HTML("<b>"),"  ",HTML("</b>"))
                                              )
                                            ),
                                            tags$tr(
                                              tags$div(width="50%",style = helpStyle,
                                                       HTML("<b>Web information:</b>&emsp;<a href='https://doingpsychstats.wordpress.com/resources/brawstats-software/' target='_blank'>Here</a>")
                                              )
                                            )
                                 )
                        ),
                        tabPanel("#",id="#",
                                 style = paste("background: ",panelcolours$helpC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width="30%",tags$div(style = localPlainStyle, "Load extras:")),
                                              tags$td(width="15%",checkboxInput("LoadExtras", label=NULL,value=switches$loadExtrasValue)),
                                              tags$td(width="30%",tags$div(style = localPlainStyle, "Large graphs:")),
                                              tags$td(width="25%",checkboxInput("LargeGraphs", label=NULL,value=FALSE))
                                            ),
                                 ),
                                 conditionalPanel(condition="input.LoadExtras",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width="30%",tags$div(style = localPlainStyle, "Short hand:")),
                                                               tags$td(width="15%",checkboxInput("shortHand",value=FALSE, label=NULL)),
                                                               tags$td(width="30%",tags$div(style = localPlainStyle, "Shorthand x:")),
                                                               tags$td(width="25%",numericInput("shortHandGain",value=10, label=NULL))
                                                             ),
                                                             tags$tr(
                                                               tags$td(width="30%",tags$div(style = localPlainStyle, "White graphs:")),
                                                               tags$td(width="15%",checkboxInput("WhiteGraphs", label=NULL,value=FALSE)),
                                                               tags$td(width="30%",tags$div(style = localPlainStyle, "Display:")),
                                                               tags$td(width="25%",selectInput("RZ",label=NULL, c("r"="r","z"="z"), selected=RZ, selectize=FALSE))
                                                             )
                                                  )
                                 ),
                                 # conditionalPanel(condition="input.LoadExtras",
                                 #                  tags$table(width = "100%",class="myTable",
                                 #                             tags$tr(
                                 #                               tags$td(width="30%",tags$div(style = localPlainStyle, "Notation:")),
                                 #                               tags$td(width="30%",selectInput("Notation1",label=NULL, 
                                 #                                                               c("psig"="psig","w"="w"), 
                                 #                                                               selected="psig", selectize=FALSE)),
                                 #                               tags$td(width="20%",selectInput("Notation2",label=NULL, 
                                 #                                                               c("U"="U","D"="D"), 
                                 #                                                               selected="D", selectize=FALSE)),
                                 #                               tags$td(width="20%",selectInput("Notation3",label=NULL, 
                                 #                                                               c("+"="+","0"="0","-"="-"), 
                                 #                                                               selected="0", selectize=FALSE)),
                                 #                             )
                                 #                  )
                                 # )
                        )
            )
  )


