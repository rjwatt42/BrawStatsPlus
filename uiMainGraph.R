source("uiMetaGraph.R")
source("uiPossibleGraph.R")


MainGraphs <-
  wellPanel(
    # style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         plotOutput("SamplePlot")
                ),
                tabPanel("Describe",class="Graphs",
                         plotOutput("DescriptivePlot")
                ),
                tabPanel("Infer",
                         plotOutput("InferentialPlot")
                ),
                tabPanel("Expect",value="Expect", 
                         plotOutput("ExpectedPlot")
                )
                ,metaGraphPanel()
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExplorePlot")
                )
                ,possibleGraphPanel()
    ),
    # ,width=fullPanelWidth
  )

MainGraphs1 <-
  wellPanel(
    # style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         plotOutput("SamplePlot1")
                ),
                tabPanel("Describe",class="Graphs",
                         plotOutput("DescriptivePlot1")
                ),
                tabPanel("Infer",
                         plotOutput("InferentialPlot1")
                ),
                tabPanel("Expect",value="Expect", 
                         plotOutput("ExpectedPlot1")
                )
                ,metaGraphPanel1()
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExplorePlot1")
                )
                ,possibleGraphPanel1()
    ),
    # ,width=fullPanelWidth
  )
