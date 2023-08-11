MainReports <- function() {
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 style="margin:0px;padding:0px;",
                                 plotOutput("SampleReport")
                                 ),
                        tabPanel("Describe",   
                                 style="margin:0px;padding:0px;",
                                 plotOutput("DescriptiveReport")
                                 ),
                        tabPanel("Infer",      
                                 style="margin:0px;padding:0px;",
                                 plotOutput("InferentialReport")
                                 )
                        ,tabPanel("Expect",value="Expect",   
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExpectedReport"))
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExploreReport")
                                  )
                        ,possibleReportPanel()
            )
  )
}

MainReports1 <- function() {
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 style="margin:0px;padding:0px;",
                                 plotOutput("SampleReport1")
                                 ),
                        tabPanel("Describe",   
                                 style="margin:0px;padding:0px;",
                                 plotOutput("DescriptiveReport1")
                                 ),
                        tabPanel("Infer",      
                                 style="margin:0px;padding:0px;",
                                 plotOutput("InferentialReport1")
                                 )
                        ,tabPanel("Expect",value="Expect",   
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExpectedReport1")
                                  )
                        ,metaGraphPanel()
                        ,tabPanel("Explore",value="Explore",
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExploreReport1")
                                  )
                        ,possibleReportPanel()
            )
  )
}

