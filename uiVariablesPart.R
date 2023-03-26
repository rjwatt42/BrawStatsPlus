
variablesPanel <- function(prefix="",asTable=FALSE) {
  
  
vT<-tags$table(width = "100%",class="myTable",
           tags$tr(
             tags$td(width = "5%", tags$div(style = localStyle, "IV:")),
             tags$td(width = "45%", selectInput(paste0(prefix,"IVchoice"), label = NULL,
                                                choices=IV$name,
                                                selected=IV$name,
                                                selectize=FALSE
             )),
             tags$td(width = "25%", actionButton(paste0(prefix,"editIV"),"Edit IV")),
             tags$td(width = "25%", tags$div(style = localStyle, " ")),
             tags$td(width = "5%", tags$div(style = localStyle, " ")),
             tags$td(width = "10%", actionButton(paste0(prefix,"inspectIV"),"i")),
           ),
           tags$tr(
             tags$td(width = "5%", tags$div(style = localStyle, "IV2:")),
             tags$td(width = "45%", selectInput(paste0(prefix,"IV2choice"), label = NULL,
                                                choices=IV2$name,
                                                selected=IV2$name,
                                                selectize=FALSE
             )),
             tags$td(width = "25%", 
                     conditionalPanel(condition = "input.IV2choice != 'none'",
                                      actionButton(paste0(prefix,"editIV2"),"Edit IV2")
                     )),
             tags$td(width = "5%", tags$div(style = localStyle, " "))
           ),
           tags$tr(
             tags$td(width = "5%", tags$div(style = localStyle, "DV:")),
             tags$td(width = "45%", selectInput(paste0(prefix,"DVchoice"), label = NULL,
                                                choices=DV$name,
                                                selected=DV$name,
                                                selectize=FALSE
             )),
             tags$td(width = "25%", actionButton(paste0(prefix,"editDV"),"Edit DV")),
             tags$td(width = "25%", actionButton(paste0(prefix,"EvidenceHypothesisApply1"),"Apply")),
             tags$td(width = "5%", tags$div(style = localStyle, " ")),
             tags$td(width = "10%", actionButton(paste0(prefix,"inspectDV"),"i")),
           ),
)
}




