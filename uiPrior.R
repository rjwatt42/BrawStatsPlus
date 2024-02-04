# world tab
priorPanel<-function(prefix="",asTable=FALSE) {

  priorTable<-tags$table(width = "100%",class="myTable",
               tags$tr(
                 tags$td(width = "30%", tags$div(style = localStyle, "pdf:")),
                 tags$td(width = "30%",
                         selectInput(paste0(prefix, "Prior_distr"), label=NULL,
                                     c("Single" = "Single",
                                       "Double" = "Double",
                                       "Uniform" = "Uniform",
                                       "Gauss"="Gauss",
                                       "Exp" = "Exp",
                                       ">"=">",
                                       "<"="<"),
                                     selected=evidence$prior$populationPDF,
                                     width="100%",selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         selectInput(paste0(prefix,"Prior_distr_rz"), label=NULL,
                                     c("r" = "r",
                                       "z" = "z"),
                                     selected=evidence$prior$populationRZ,
                                     width="100%",selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         conditionalPanel(condition=paste0("input.",prefix,"Prior_distr!=='Uniform'"),
                                          numericInput(paste0(prefix, "Prior_distr_k"),label=NULL,
                                                       min = 0,
                                                       max = 1,
                                                       step = 0.05,
                                                       value = evidence$prior$populationPDFk)
                         )
                 ),
                 tags$td(width="10%")
               ),
               tags$tr(
                 tags$td(width = "30%", tags$div(style = localStyle, pPlusLabel)),
                 tags$td(width = "30%", numericInput(paste0(prefix, "Prior_Nullp"), label=NULL,
                                                     min=0,max=1, step=0.025,
                                                     value=evidence$prior$populationNullp)
                         ),
                 tags$td(width = "15%"),
                 tags$td(width = "15%"),
                 tags$td(width="10%")
               )
    )
  if (asTable) {
    return(priorTable)
  } else {
    return(
      wellPanel(
        style = paste("background: ",subpanelcolours$evidenceC,";"),
        priorTable,
        width="100%"
      )
    )
  }
}

