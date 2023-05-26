

metaFilePanel<-function() {
  tabPanel("Meta",
         style = paste("background: ",subpanelcolours$filesC), 
         wellPanel(
           style = paste("background: ",subpanelcolours$filesC,";"),
           tags$table(width = "100%",class="myTable",
                      tags$tr(
                        tags$td(width = "5%", style="border-top: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "import:")),
                        tags$td(width = "90%", style="border-top: 1px solid black;;height:20px;margin:0px;padding:0px;margin-top:4px;padding-top:4px;",
                                fileInput("metaInputFile",label=NULL,accept=c(".xlsx",".xls"),buttonLabel="Choose: ")
                        ),
                        tags$td(width = "5%", style="border-top: 1px solid black;border-right: 1px solid black;")
                      ),
                      tags$tr(
                        tags$td(width = "5%", style="border-bottom: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "sheet:")),
                        tags$td(width = "90%", style="border-bottom: 1px solid black;",
                                selectInput("metaInputSheet",label=NULL,choices=c(""),selectize = FALSE)
                        ),
                        tags$td(width = "5%",style="border-bottom: 1px solid black;border-right: 1px solid black;", 
                                actionButton("metaInputFileLoad","Load"))
                      ),
                      tags$tr(
                        tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "export:")),
                        tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                textInput("metaOutputFile",label=NULL)),
                        tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                actionButton("metaOutputFileSave","Save")),
                      ),
           )
         )
)

}
