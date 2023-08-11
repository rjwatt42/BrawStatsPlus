
effectPanel <- function(prefix="",asTable=FALSE,full=TRUE) {

  eT1<-tags$table(width = "100%",class="myTable",
                 tags$tr(
                   tags$td(width = "40%", tags$div(style = labelStyle, "Hypothesis:")),
                   tags$td(width = "20%"),
                   tags$td(width = "40%"),
                 ),
                 tags$tr(
                   tags$td(width = "40%", tags$div(style = localStyle, "IV",HTML("&rarr;"),"DV :")),
                   tags$td(width = "20%", 
                           numericInput(paste0(prefix,"rIV"), label=NULL,value=effect$rIV,min=-1,max=1,step=0.05)
                   ),
                   tags$td(width = "40%"),
                 )
  )
                 
eT2<-tags$table(width = "100%",class="myTable",
             tags$tr(
               tags$td(width = "40%", tags$div(style = labelStyle, "Hypothesis:")),
               tags$td(width = "20%"),
               tags$td(width = "40%"),
             ),
             tags$tr(
               tags$td(width = "40%", tags$div(style = localStyle, "IV",HTML("&rarr;"),"DV :")),
               tags$td(width = "20%", 
                       numericInput(paste0(prefix,"rIV"), label=NULL,value=effect$rIV,min=-1,max=1,step=0.05)
               ),
               tags$td(width = "40%"),
             ),
             tags$tr(
               tags$td(width = "40%", 
                       conditionalPanel(condition=paste0("input.IV2choice != 'none'"),
                                        tags$div(style = localStyle, "IV2",HTML("&rarr;"),"DV :",id=paste0(prefix,"IV2-DV"))
                       )),
               tags$td(width = "20%", 
                       conditionalPanel(condition=paste0("input.IV2choice != 'none'"),
                                        numericInput(paste0(prefix,"rIV2"), label = NULL,
                                                     min = -1,
                                                     max = 1,
                                                     step = 0.05,
                                                     value = effect$rIV2
                                        ))),
               tags$td(width = "40%", 
                       tags$div(style = localStyle, " ")
               )
               
             ),
             tags$tr(
               tags$td(width = "40%", 
                       conditionalPanel(condition=paste0("input.IV2choice != 'none'"),
                                        tags$div(style = localStyle, "IV",HTML("&rarr;"),"IV2 :",id=paste0(prefix,"IV1-IV2"))
                       )),
               tags$td(width = "20%", 
                       conditionalPanel(condition=paste0("input.IV2choice != 'none'"),
                                        numericInput(paste0(prefix,"rIVIV2"), label = NULL,
                                                     min = -1,
                                                     max = 1,
                                                     step = 0.05,
                                                     value = effect$rIVIV2
                                        ))),
               tags$td(width = "40%", tags$div(style = localStyle, " "))
               
             ),
             tags$tr(id=paste0(prefix,"IV*IV2*DV"),
               tags$td(width = "40%", 
                       conditionalPanel(condition="input.IV2choice != 'none'",
                                        tags$div(style = localStyle,"IV*IV2",HTML("&rarr;"),"DV :",id=paste0(prefix,"IV1-IV2-DV"))
                       )),
               tags$td(width = "20%",id="IV*IV2", 
                       conditionalPanel(condition=paste0("input.IV2choice != 'none'"),
                                        numericInput(paste0(prefix,"rIVIV2DV"), label = NULL,
                                                     min = -1,
                                                     max = 1,
                                                     step = 0.05,
                                                     value = effect$rIVIV2DV
                                        ))),
               tags$td(width = "40%", tags$div(style = localStyle, " "))
             ),
  )
if (full) {
  eT<-eT2
} else {
  eT<-eT1
}
eT
}
