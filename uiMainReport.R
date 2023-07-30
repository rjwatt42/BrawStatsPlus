MainReports <-
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 plotOutput("SampleReport")
                                 ),
                        tabPanel("Describe",   
                                 plotOutput("DescriptiveReport")
                                 ),
                        tabPanel("Infer",      
                                 plotOutput("InferentialReport")
                                 )
                        ,tabPanel("Expect",value="Expect",   
                                  plotOutput("ExpectedReport"))
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  plotOutput("ExploreReport")
                                  )
                        ,possibleReportPanel()
            )
  )


MainReports1 <-
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 plotOutput("SampleReport1")
                                 ),
                        tabPanel("Describe",   
                                 plotOutput("DescriptiveReport1")
                                 ),
                        tabPanel("Infer",      
                                 plotOutput("InferentialReport1")
                                 )
                        ,tabPanel("Expect",value="Expect",   
                                  plotOutput("ExpectedReport1")
                                  )
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  plotOutput("ExploreReport1")
                                  )
                        ,possibleReportPanel()
            )
  )


