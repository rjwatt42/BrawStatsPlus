
worldDiagram<-function() {
  if (switches$doWorlds) {
    tabPanel("World",
             style = paste("background: ",maincolours$graphC), 
             plotOutput("WorldPlot"),
             plotOutput("WorldPlot2")
    )
  } else {
    c()
  }
}


  HypothesisDiagram <-
    wellPanel(
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("HypothesisPlot")
                  ),
                  worldDiagram()
      ),
      width="8cm"
    )
  
