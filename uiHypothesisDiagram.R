
worldDiagram<-function() {
  if (switches$doWorlds) {
    tabPanel("World",
             style = paste("background: ",maincolours$graphC,"margin:0px;padding:0px;"), 
             plotOutput("WorldPlot"),
             plotOutput("WorldPlot2")
    )
  } else {
    c()
  }
}


  HypothesisDiagram <-function() {
    wellPanel(
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC,"margin:0px;padding:0px;"), 
                           plotOutput("HypothesisPlot")
                  ),
                  worldDiagram()
      ),
      width="8cm"
    )
  }
  
