
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
  

worldDiagram<-function() {
  if (switches$doWorlds) {
    tabPanel("World",
         style = paste("background: ",maincolours$graphC), 
         plotOutput("WorldPlot")
    )
  } else {
    c()
  }
}

