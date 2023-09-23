varTypes<- c("Interval" = "Interval",
             "Ordinal" = "Ordinal",
             "Categorical" = "Categorical"
)

variableDialog<-wellPanel(
  style = paste("background: ",subpanelcolours$hypothesisC,";"),
  tags$table(width = "100%",class="myTable",
             tags$tr(
               tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
               tags$td(width = "70%", textInput("MVname", value = MV$name, label = NULL))
             ),
             tags$tr(
               tags$td(width = "30%", div(style = localStyle, "Type:")),
               tags$td(width = "70%", 
                       selectInput("MVtype", label= NULL,
                                   varTypes,selected=MV$type,
                                   selectize=FALSE
                       )
               )
             )
  ),
  # conditionalPanel(condition="input.MVtype == 'Interval'",
                   tags$table(id="MVIntVal",width = "100%",class="myTable",
                              tags$tr(id="MVIntVal1",
                                      tags$td(width = "30%",  tags$div(id="MVTmu",style = localStyle, "Mean:")),
                                      tags$td(width = "20%", numericInput("MVmu", value = MV$mu, label = NULL)),
                                      tags$td(width = "30%",  tags$div(id="MVTsd",style = localStyle, "Sd:")),
                                      tags$td(width = "20%", numericInput("MVsd", value = MV$sd, label = NULL))
                              ),
                              tags$tr(id="MVIntVal2",
                                      tags$td(width = "30%",  tags$div(id="MVTskew",style = localStyle, "Skew:")),
                                      tags$td(width = "20%",  numericInput("MVskew", value = MV$skew, step=0.1, label = NULL)),
                                      tags$td(width = "30%",  tags$div(id="MVTkurt",style = localStyle, "Kurtosis:")),
                                      tags$td(width = "20%",  numericInput("MVkurt", value = MV$kurtosis, step=0.1, label = NULL))
                              ),
                   ),
                   # ),
  # conditionalPanel(condition="input.MVtype == 'Ordinal'",
                   tags$table(id="MVOrdVal",width = "100%",class="myTable",
                              tags$tr(id="MVOrdVal1",
                                      tags$td(width = "30%",  tags$div(id="MVTnlevs",style = localStyle, "No levels:")),
                                      tags$td(width = "20%", numericInput("MVnlevs", value = MV$nlevs, label = NULL,step=1,min=2)),
                                      # tags$td(width = "30%",  tags$div(style = localStyle, "Spread:")),
                                      # tags$td(width = "20%", textInput("MVspread", value = MV$iqr, label = NULL)),
                                      # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                              ),
                              tags$tr(id="MVOrdVal2",
                                      tags$td(width = "30%",  tags$div(id="MVTcentre",style = localStyle, "Median:")),
                                      tags$td(width = "20%", numericInput("MVcentre", value = (MV$nlevs+1)/2, label = NULL,step=1)),
                                      tags$td(width = "30%",  tags$div(id="MVTmu",style = localStyle, "IQR:")),
                                      tags$td(width = "20%", numericInput("MVspread", value = (MV$nlevs-1)/2, label = NULL,step=0.5)),
                                      # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                              ),
                              tags$tr(id="MVOrdVal3",
                                      tags$td(width = "30%",  tags$div(id="MVTdiscrete",style = localStyle, "Source:")),
                                      tags$td(width = "20%", selectInput("MVdiscrete", choices=c("continuous","discrete"),selected = MV$discrete, label = NULL,selectize=FALSE)),
                              ),
                   ),
  # ),
  # conditionalPanel(condition="input.MVtype == 'Categorical'",
                 tags$table(id="MVCatVala",width = "100%",class="myTable",
                            tags$tr(id="MVCatVal1",
                                    tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                    tags$td(width = "20%", numericInput("MVncats", value = MV$ncats, label = NULL,step=1,min=2)),
                                    tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                    tags$td(width = "20%", textInput("MVprop", value = MV$proportions, label = NULL)),
                                    # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                            ),
                 ),
                 tags$table(id="MVCatValb",width = "100%",class="myTable",
                            tags$tr(id="MVCatVal2",
                                    tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                    tags$td(width = "60%", textInput("MVcases", value = MV$cases, label = NULL)),
                                    tags$td(width = "10%",  tags$div(style = localStyle, " "))
                            ),
                            tags$tr(id="MVCatVal3",
                                    tags$td(width = "30%",  tags$div(id="MVTsource",style = localStyle, "Source:")),
                                    tags$td(width = "20%", selectInput("MVsource", choices=c("continuous","discrete"),selected = MV$source, label = NULL,selectize=FALSE)),
                            ),
                 ),
  # ),
# tags$table(id="Inspect",width = "100%",class="myTable",
#            tags$tr(id="MVCatVal2",
#                    tags$td(width = "90%",  tags$div(style = localStyle, " ")),
#                    tags$td(width = "10%", actionButton("MVInspect", label = "Inspect")),
#            )
# ),

width="100%")


