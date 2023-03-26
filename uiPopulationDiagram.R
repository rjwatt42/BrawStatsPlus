
PopulationDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Theory",
                tabPanel("Population",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PopulationPlot")
                         ),
                tabPanel("Prediction",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PredictionPlot")
                         )
    ),
    width="8cm"
  )
    