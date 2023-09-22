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
  g<-graphSample(IV,IV2,DV,effect,design,evidence,result)
  return(g)
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
  g<-graphDescription(IV,IV2,DV,effect,design,evidence,result)
  return(g)
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
  
  g<-graphInference(IV,IV2,DV,effect,design,evidence,result,input$EvidenceInfer_type)
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
  
  rpt<-reportSample(IV,IV2,DV,design,result)
  reportPlot(rpt$outputText,rpt$nc,rpt$nr)        
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
  
  rpt<-reportDescription(IV,IV2,DV,evidence,result)
  reportPlot(rpt$outputText,rpt$nc,rpt$nr)        
  
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
  rpt<-reportInference(IV,IV2,DV,effect,evidence,result)        
  reportPlot(rpt$outputText,rpt$nc,rpt$nr)        
  
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
