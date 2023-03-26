

inspectDiagram <-
  bsModal(id="inspectOutput", title="Inspect Variable", trigger="inspectButton", size = "large",
          wellPanel(
          style = paste("background: ",maincolours$graphC,";"),
          fluidRow(
            column(offset=1,width=1, 
                   fluidRow(
                     selectInput("inspectOrder","Display:",choices=c("unsorted","sorted","piled"),selected="unsorted",selectize=FALSE)
                   ),
                   fluidRow(
                     checkboxInput("showMean","Mean")
                   ),
                   fluidRow(
                     checkboxInput("showSD","Sd")
                   ),
                   fluidRow(
                     actionButton("inspectNewSample",label="New Sample")
                   ),
            ),
            column(width=6,plotOutput("mainInspect")),
            column(width=4,plotOutput("penaltyInspect"))
          ),
          fluidRow(
            column(offset=1,width=1,
                   fluidRow(
                   checkboxInput("showResiduals","Show Residuals"),selectInput("whichResiduals",label=NULL,choices=c("1st Order"="1","2nd Order"="2"),selected="1",selectize=FALSE)
                   ),
            ),
            column(width=6,conditionalPanel(condition="input.showResiduals",
                           sliderInput("ResidVal",label=NULL,value=-0.8,min=-1,max=1, step=0.1, ticks=FALSE, width="100%")
                           )
            ),
            column(offset=1,width=3,htmlOutput("explainResiduals")
            ),
          ),
          )
  )

