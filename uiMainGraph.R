source("uiMetaGraph.R")
source("uiPossibleGraph.R")


MainGraphs <-
  wellPanel(
    # style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         plotOutput("SamplePlot"),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Describe",class="Graphs",
                         plotOutput("DescriptivePlot"),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Infer",
                         plotOutput("InferentialPlot"),
                         style =paste("background:", maincolours$graphC, ";"),
                ),
                tabPanel("Expect",value="Expect", 
                         plotOutput("ExpectedPlot"),
                         style =paste("background:", maincolours$graphC, ";")
                )
                ,metaGraphPanel()
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExplorePlot"),
                         style =paste("background:", maincolours$graphC, ";")
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
                         plotOutput("SamplePlot1"),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Describe",class="Graphs",
                         plotOutput("DescriptivePlot1"),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Infer",
                         plotOutput("InferentialPlot1"),
                         style =paste("background:", maincolours$graphC, ";"),
                ),
                tabPanel("Expect",value="Expect", 
                         plotOutput("ExpectedPlot1"),
                         style =paste("background:", maincolours$graphC, ";")
                )
                ,metaGraphPanel1()
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExplorePlot1"),
                          style =paste("background:", maincolours$graphC, ";")
                )
                ,possibleGraphPanel1()
    ),
    # ,width=fullPanelWidth
  )
