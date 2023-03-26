#################################################################
## inspect variables functions

inspectData<-c()  
inspectVar<-c()
inspectSource<-c()
inspectHistory<-c()

updateInspect<-function() {
  inspect<-list(inspectOrder=input$inspectOrder,whichResiduals=input$whichResiduals,
                showResiduals=input$showResiduals,inspectHistory=inspectHistory,
                ResidVal=input$ResidVal,n=input$sN,
                data=inspectData)
}

inspectVariable<-function(var) {
  inspectVar<<-var
  if (!simData) {
    use<-which(variables$name==var$name)
    inspectData<<-importedData[,use+1]
  } else {
    inspectData<<-c()
  }
  inspectHistory<<-c()
  
  updateCheckboxInput(session,"showResiduals",value=FALSE)
  switch (var$type,
          "Categorical"={
            updateCheckboxInput(session,"showMean",label="Mode")
            updateCheckboxInput(session,"showSD",label=" ")
            # c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10
            updateSliderInput(session,"ResidVal",min=1-(var$ncats-1)/10,max=var$ncats+(var$ncats-1)/10,value=1.7,step=0.25)
            updateSelectInput(session,"inspectOrder",choices=c("unsorted","piled"),selected="unsorted")
            shinyjs::hideElement(id= "showSD")
          },
          "Ordinal"={
            updateCheckboxInput(session,"showMean",label="Median")
            updateCheckboxInput(session,"showSD",label="IQR")
            # c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10
            updateSliderInput(session,"ResidVal",min=1-(var$nlevs-1)/10,max=var$nlevs+(var$nlevs-1)/10,value=1.7,step=0.25)
            updateSelectInput(session,"inspectOrder",choices=c("unsorted","sorted","piled"),selected="unsorted")
            shinyjs::showElement(id= "showSD")
          },
          "Interval"={
            updateCheckboxInput(session,"showMean",label="Mean")
            updateCheckboxInput(session,"showSD",label="SD")
            updateSliderInput(session,"ResidVal",min=var$mu-var$sd*3,max=var$mu+var$sd*3,value=var$mu-var$sd,step=var$sd/4)
            updateSelectInput(session,"inspectOrder",choices=c("unsorted","sorted","piled"),selected="unsorted")
            shinyjs::showElement(id= "showSD")
          }
  )
  toggleModal(session, modalId = "inspectOutput", toggle = "open")
  
}

observeEvent(input$inspectIV,{
  var<-updateIV()
  inspectSource<<-"inspectIV"
  inspectVariable(var)
}
)
observeEvent(input$inspectDV,{
  var<-updateDV()
  inspectSource<<-"inspectDV"
  inspectVariable(var)
}
)

observeEvent(input$ResidVal, {
  inspectHistory<<-c(inspectHistory,input$ResidVal)
  
}
)

observeEvent(input$inspectNewSample,{
  IV<-updateIV()
  DV<-updateDV()
  effect<-updateEffect()
  design<-updateDesign()
  sample<-makeSample(IV,NULL,DV,effect,design)
  switch (inspectSource,
          "inspectIV"={inspectData<<-sample$iv},
          "inspectDV"={inspectData<<-sample$dv}
  )
  
})

getInspect1<-eventReactive(c(input$inspectOrder,input$inspectNewSample,input$showResiduals,input$whichResiduals,input$ResidVal,input$showMean,input$showSD),{
  
  inspect<-list(inspectOrder=input$inspectOrder,whichResiduals=input$whichResiduals,
                showResiduals=input$showResiduals,inspectHistory=inspectHistory,
                ResidVal=input$ResidVal,n=input$sN,
                showMean=input$showMean,showSd=input$showSD,
                var=inspectVar,
                data=inspectData)
}
)

output$mainInspect<-renderPlot( {
  doIt<-input$inspectNewSample
  inspect<-getInspect1()
  return(inspectMainGraph(inspect))
}
)

output$penaltyInspect<-renderPlot( {
  doIt<-input$inspectNewSample
  inspect<-getInspect1()
  return(inspectPenaltyGraph(inspect))
}
)

output$explainResiduals<-renderText( {
  if (input$showResiduals) {
    switch (inspectVar$type,
            "Categorical"={txt1<-"<br> 0: v<sub>i</sub> = v&#772;<br> 1: v<sub>i</sub> &ne; v&#772;"
            txt2<-"<b>mode</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (nearest to 0)"
            },
            "Ordinal"={txt1<-"<br> -1: v<sub>i</sub> < v&#772;<br> +1: v<sub>i</sub> more than v&#772;"
            txt2<-"<b>median</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (nearest to 0)"
            },
            "Interval"={txt1<-"v<sub>i</sub>-v&#772;"
            txt2<-"<b>mean</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (equals 0)"
            }
    )
    paste0("<p style='font-size:12px'>", "<b>residual:</b> ",txt1, "<br>",  txt2, "</p>")
  } else {""}
}
)
#################################################################
