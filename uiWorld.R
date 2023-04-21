# world tab

worldPanel<-function(prefix="",asTable=FALSE,doAnyway=FALSE) {
  
  worldTable<-
    tags$table(width = "100%",class="myTable",
               tags$tr(
                 tags$td(width = "40%", tags$div(style = localStyle, "World:")),
                 tags$td(width = "30%",
                         checkboxInput(paste0(prefix, "world_on"), label=NULL, value=effect$world$worldOn)
                 ),
                 tags$td(width = "15%"),
                 tags$td(width = "15%")
               ),
               tags$tr(
                 tags$td(width = "40%", tags$div(style = localStyle, "Population pdf:")),
                 tags$td(width = "30%",
                         selectInput(paste0(prefix, "world_distr"), label=NULL,
                                     c("Single" = "Single",
                                       "Uniform" = "Uniform",
                                       "Gauss"="Gauss",
                                       "Exp" = "Exp"),width="100%",
                                     selected=effect$world$populationPDF,
                                     selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         selectInput(paste0(prefix,"world_distr_rz"), label=NULL,
                                     c("r" = "r",
                                       "z" = "z"),width="100%",
                                     selected=effect$world$populationRZ,
                                     selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         conditionalPanel(condition=paste0("input.",prefix,"world_distr!=='Uniform'"),
                         numericInput(paste0(prefix, "world_distr_k"),label=NULL,
                                      min = -1,
                                      max = 1,
                                      step = 0.05,
                                      value = effect$world$populationPDFk)
                         )
                 )
               ),
               tags$tr(
                 tags$td(width = "40%", tags$div(style = localStyle, "p(null):")),
                 tags$td(width = "30%", numericInput(paste0(prefix, "world_distr_Nullp"), label=NULL,min=0,max=1, step=0.05,value=effect$world$populationNullp)),
                 tags$td(width = "15%", tags$div(style = localStyle, "abs:")),
                 tags$td(width = "15%",
                   checkboxInput(paste0(prefix, "world_abs"), label=NULL, value=effect$world$worldAbs)
                 )
               )
    )
  
  if (!asTable) {
    worldTable<-tabPanel("World",value="World",
                         style = paste("background: ",subpanelcolours$hypothesisC),
                         worldTable)
  }
  if (switches$doWorlds || doAnyway){
    return(worldTable)
  } else {
    return(c())
  }
}

