##################################################################################    
# SINGLE SAMPLE
# UI changes
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    
applyingAnalysis<-FALSE

oldResult<-NULL
onlyAnalysis<-FALSE
observeEvent(c(input$Welch,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn),{
  onlyAnalysis<<-TRUE
},priority=100)
observeEvent(c(input$EvidencenewSample),{
  onlyAnalysis<<-FALSE
},priority=100)

# UI changes
# go to the sample tabs 
sampleUpdate<-observeEvent(c(input$Single,input$EvidencenewSample,input$EvidenceHypothesisApply),{
  if (any(c(input$Single,input$EvidencenewSample))>0) {
    if (!is.element(input$Graphs,c("Sample","Describe","Infer","Possible")))
    {updateTabsetPanel(session, "Graphs",
                       selected = "Sample")
      updateTabsetPanel(session, "Reports",
                        selected = "Sample")
    }
  }
}
)

whichAnalysisSample<-observeEvent(input$EvidencenewSample,{
  applyingAnalysis<<-FALSE
},priority=100)
whichAnalysisApply<-observeEvent(input$EvidenceHypothesisApply,{
  applyingAnalysis<<-TRUE
},priority=100)

# single sample calculations
doSampleAnalysis<-function(IV,IV2,DV,effect,design,evidence){
  if (debug) debugPrint(". doSampleAnalysis")
  if (IV$type=="Ordinal") {
    if (warnOrd==FALSE) {
      hmm("Ordinal IV will be treated as Interval.")
      warnOrd<<-TRUE
    }
  }
  if (!is.null(IV2)) {
    if (IV2$type=="Ordinal") {
      if (warnOrd==FALSE) {
        hmm("Ordinal IV2 will be treated as Interval.")
        warnOrd<<-TRUE
      }
    }
  }
  result<-runSimulation(IV,IV2,DV,effect,design,evidence,FALSE,onlyAnalysis,oldResult)
  if (debug) debugPrint(". doSampleAnalysis - exit")
  
  result
}

# eventReactive wrapper
sampleAnalysis<-eventReactive(c(input$EvidenceHypothesisApply,input$EvidencenewSample,
                                input$Welch,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn),{
  if (any(input$EvidenceHypothesisApply,input$EvidencenewSample)>0){
    validSample<<-TRUE
    IV<-updateIV()
    IV2<-updateIV2()
    DV<-updateDV()
    
    effect<-updateEffect()
    design<-updateDesign()
    evidence<-updateEvidence()

    showNotification("Sample: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
    oldShortHand<-shortHand
    shortHand<<-FALSE
    result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
    shortHand<-oldShortHand
    ResultHistory<<-result$ResultHistory
    
    # set the result into likelihood: populations
    if (!is.na(result$rIV)) {
      updateNumericInput(session,"possiblePSampRho",value=result$rIV)
      updateNumericInput(session,"possibleSampRho",value=result$rIV)
    }
    removeNotification(id = "counting")
  } else {
    result<-NULL
  }
  result
})


# SINGLE graphs
# single sample graph
makeSampleGraph <- function () {
  doIt<-editVar$data
  
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  # make the sample
  result<<-sampleAnalysis()
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  oldResult<<-result
  
  # draw the sample
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
     scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  switch (no_ivs,{
    g<-g+annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
  },
  { 
    effect1<-effect
    effect2<-effect
    effect2$rIV<-effect2$rIV2
    effect3<-effect
    effect3$rIV<-effect3$rIVIV2
    
    result1<-result
    result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
    result3<-list(IVs=result$IVs, DVs=result$IV2s, rIV=result$rIVIV2, ivplot=result$ivplot,dvplot=result$iv2plot)
    
    g<-g+
      annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect1,result1)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
      annotation_custom(grob=ggplotGrob(drawSample(IV2,DV,effect2,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
      annotation_custom(grob=ggplotGrob(drawSample(IV,IV2,effect3,result3)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
  }
  )
  g
}

# single descriptive graph
makeDescriptiveGraph <- function(){
  doIt<-editVar$data
  # doIt<-input$MVok
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  # make the sample
  result<-sampleAnalysis()
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  if (is.na(result$rIV)) {
    validate("IV has no variability")
    return(ggplot()+plotBlankTheme)
  }
  
  # draw the description
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  switch (no_ivs, 
          {
            g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
          },
          { 
            if (evidence$rInteractionOn==FALSE){
              effect2<-effect
              effect2$rIV<-effect2$rIV2
              
              result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
              
              g<-g+
                annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
            }
            else{
              if (showInteractionOnly){
                if (DV$type=="Categorical") {
                  if (IV2$type=="Interval") {
                    effect1<-effect
                    result1<-result
                    use<-result1$iv2<median(result$iv2)
                    result1$iv<-result$iv[use]
                    result1$dv<-result$dv[use]
                    result1$IVs$vals<-result$iv[use]
                    result1$DVs$vals<-result$dv[use]
                    
                    effect2<-effect
                    result2<-result
                    result2$iv<-result$iv[!use]
                    result2$dv<-result$dv[!use]
                    result2$IVs$vals<-result$iv[!use]
                    result2$DVs$vals<-result$dv[!use]
                    
                    g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,">",format(median(result$iv2),digits=3)))),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                    g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect2,design,result2)+gridTheme+ggtitle(paste0(IV2$name,"<",format(median(result$iv2),digits=3)))),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                  } else {
                    switch (IV2$ncats,
                            {},
                            {xmin<-c(0.5,5.5)
                            xmax<-c(4.5,9.5)
                            ymin<-c(0,0)
                            ymax<-c(5,5)},
                            {xmin<-c(0.5,5.5,3)
                            xmax<-c(4.5,9.5,7)
                            ymin<-c(0,0,5)
                            ymax<-c(4.25,4.25,9.25)},
                            {xmin<-c(0.5,5.5,0.5,5.5)
                            xmax<-c(4.5,9.5,4.5,9.5)
                            ymin<-c(0,0,5,5)
                            ymax<-c(4.25,4.25,9.25,9.25)},
                            {}
                    )
                    for (i in 1:IV2$ncats) {
                      effect1<-effect
                      result1<-result
                      use<-result1$iv2<-as.numeric(result$iv2)==i
                      result1$iv<-result$iv[use]
                      result1$dv<-result$dv[use]
                      result1$IVs$vals<-result$iv[use]
                      result1$DVs$vals<-result$dv[use]
                      
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,"==",IV2$cases[i]))),xmin=xmin[i],xmax=xmax[i],ymin=ymin[i],ymax=ymax[i])
                    }
                  }
                  # effect2<-effect
                  # result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                } else {
                  g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
                }
              } else{
                effect2<-effect
                effect2$rIV<-effect2$rIV2
                
                result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, iv=result$iv, dv=result$dv, ivplot=result$iv2plot,dvplot=result$dvplot)
                
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
              }
            }
          }
  )
  g
}

# single inferential graph
makeInferentialGraph <- function() {
  doit<-c(input$EvidenceInfer_type,input$evidenceTheory,
          input$Welch,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn)
  doIt<-editVar$data
  llrConsts<-c(input$llr1,input$llr2)
  
  # doIt<-input$MVok
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  result<-sampleAnalysis()
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  
  # if (is.null(result)) {
  #   result<-list(rIV=NA,effect=effect,design=design,evidence=evidence)
  # }
  if (is.na(result$rIV) && validSample) {
    validate("IV has no variability")
    return(ggplot()+plotBlankTheme)
  }
  
  result$showType<-evidence$showType
  result$evidence$showTheory<-input$evidenceTheory
  
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
    switch (input$EvidenceInfer_type,
          "EffectSize"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"r")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "Power"= {
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"w")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"nw")
          },
          "2D"= {
            g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,result,"r","p")
            return(g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=1,xmax=9,ymin=0,ymax=10))
          },
          "log(lrs)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrs)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          },
          "log(lrd)"={
            g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrd)")
            g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
          }
  )
  g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)
  g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
  return(g)
}

output$SamplePlot <- renderPlot({
  if (debug) debugPrint("SamplePlot")
  doIt<-editVar$data
  g<-makeSampleGraph()
  if (debug) debugPrint("SamplePlot - exit")
  g
})

output$DescriptivePlot <- renderPlot({
  if (debug) debugPrint("DescriptivePlot")
  doIt<-editVar$data
  g<-makeDescriptiveGraph()
  if (debug) debugPrint("DescriptivePlot - exit")
  g
})

output$InferentialPlot <- renderPlot({
  if (debug) debugPrint("InferentialPlot")
  doIt<-editVar$data
  g<-makeInferentialGraph()
  if (debug) debugPrint("InferentialPlot - exit")
  g
})

output$SamplePlot1 <- renderPlot({
  if (debug) debugPrint("SamplePlot")
  doIt<-editVar$data
  g<-makeSampleGraph()
  if (debug) debugPrint("SamplePlot - exit")
  g
})

output$DescriptivePlot1 <- renderPlot({
  if (debug) debugPrint("DescriptivePlot")
  doIt<-editVar$data
  g<-makeDescriptiveGraph()
  if (debug) debugPrint("DescriptivePlot - exit")
  g
})

output$InferentialPlot1 <- renderPlot({
  if (debug) debugPrint("InferentialPlot")
  doIt<-editVar$data
  g<-makeInferentialGraph()
  if (debug) debugPrint("InferentialPlot - exit")
  g
})

# SINGLE reports    
# single sample report
makeSampleReport <- function()  {
  
  doIt<-editVar$data
  # doIt<-input$MVok
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  
  result<-sampleAnalysis()        
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  
  reportSample(IV,IV2,DV,design,result)
}

# single descriptive report
makeDescriptiveReport <- function()  {
  doIt<-c(editVar$data,input$input$evidenceCaseOrder,input$rInteractionOn,input$dataType)
  # doIt<-input$MVok
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  result<-sampleAnalysis()
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  if (is.na(result$rIV)) {
    validate("IV has no variability")
    return(ggplot()+plotBlankTheme)
  }
  result$showType<-evidence$showType
  
  reportDescription(IV,IV2,DV,evidence,result)

}

# single inferential report
makeInferentialReport <- function()  {
  doIt<-c(editVar$data,input$Welch,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn)
  llrConsts<-c(input$llr1,input$llr2)
  
  # doIt<-input$MVok
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  result<-sampleAnalysis()
  if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  if (is.na(result$rIV)) {
    validate("IV has no variability")
    return(ggplot()+plotBlankTheme)
  }
  
  result$showType<-evidence$showType
  reportInference(IV,IV2,DV,effect,evidence,result)        
}

output$SampleReport <- renderPlot({
  if (debug) debugPrint("SampleReport")
  doIt<-editVar$data
  g<-makeSampleReport()
  if (debug) debugPrint("SampleReport - exit")
  g
})

output$DescriptiveReport <- renderPlot({
  if (debug) debugPrint("DescriptiveReport")
  doIt<-editVar$data
  g<-makeDescriptiveReport()
  if (debug) debugPrint("DescriptiveReport - exit")
  g
})

output$InferentialReport <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeInferentialReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

output$SampleReport1 <- renderPlot({
  if (debug) debugPrint("SampleReport")
  doIt<-editVar$data
  g<-makeSampleReport()
  if (debug) debugPrint("SampleReport - exit")
  g
})

output$DescriptiveReport1 <- renderPlot({
  if (debug) debugPrint("DescriptiveReport")
  doIt<-editVar$data
  g<-makeDescriptiveReport()
  if (debug) debugPrint("DescriptiveReport - exit")
  g
})

output$InferentialReport1 <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeInferentialReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

##################################################################################    
