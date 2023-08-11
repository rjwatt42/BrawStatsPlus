
PopulationDiagram <- function() {
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Theory",
                tabPanel("Population",
                         style = paste("background: ",maincolours$graphC,";","margin:0px;padding:0px;"), 
                         plotOutput("PopulationPlot")
                         ),
                tabPanel("Prediction",
                         style = paste("background: ",maincolours$graphC,";","margin:0px;padding:0px;"), 
                         plotOutput("PredictionPlot")
                         )
    ),
    width="8cm"
  )
}
