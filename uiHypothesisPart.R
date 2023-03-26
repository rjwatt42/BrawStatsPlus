source("uiVariablesPart.R")
source("uiEffectPart.R")
source("uiWorld.R")

hypothesisPanel <- function(prefix="") {
  wellPanel(
    style = paste("background: ",subpanelcolours$hypothesisC,";"),
    tabsetPanel(id="LGHypothesisPart", type="tabs",
                tabPanel("Hypothesis:",value="Hypothesis",
                ),
                # # single tab
                # tabPanel("Variables",value="Variables",id="uiLGVariables",
                #          variablesPanel(prefix,asTable = TRUE),
                # ),
                # single tab
                tabPanel("Effect",value="Effect",id="uiLGEffect",
                         effectPanel(prefix,asTable = TRUE),
                ),
                tabPanel("World",value="World",id="uiLGWorld",
                         worldPanel(prefix,asTable = TRUE),
                )
    )
  )
}
