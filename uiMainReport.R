MainReports <-
  wellPanel(id="MainReports",
            # style=paste("min-width:", graphWidth, ";"),
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 plotOutput("SampleReport"),
                                 style =paste("background:", maincolours$graphC, ";")),
                        tabPanel("Describe",   
                                 plotOutput("DescriptiveReport"),
                                 style =paste("background:", maincolours$graphC, ";")),
                        tabPanel("Infer",      
                                 plotOutput("InferentialReport"),
                                 style =paste("background:", maincolours$graphC, ";"))
                        ,tabPanel("Expect",value="Expect",   
                                  plotOutput("ExpectedReport"),
                                  style =paste("background:", maincolours$graphC, ";"))
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  plotOutput("ExploreReport"),
                                  style =paste("background:", maincolours$graphC, ";"))
                        ,possibleReportPanel()
            )
            # ,width=fullPanelWidth
  )


MainReports1 <-
  wellPanel(id="MainReports",
            # style=paste("min-width:", graphWidth, ";"),
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 plotOutput("SampleReport1"),
                                 style =paste("background:", maincolours$graphC, ";")),
                        tabPanel("Describe",   
                                 plotOutput("DescriptiveReport1"),
                                 style =paste("background:", maincolours$graphC, ";")),
                        tabPanel("Infer",      
                                 plotOutput("InferentialReport1"),
                                 style =paste("background:", maincolours$graphC, ";"))
                        ,tabPanel("Expect",value="Expect",   
                                  plotOutput("ExpectedReport1"),
                                  style =paste("background:", maincolours$graphC, ";"))
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  plotOutput("ExploreReport1"),
                                  style =paste("background:", maincolours$graphC, ";"))
                        ,possibleReportPanel()
            )
            # ,width=fullPanelWidth
  )


