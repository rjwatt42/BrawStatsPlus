##################################################################################    
# EXPECTED    
# UI changes  
# set expected variable from UI
# calculations
# outputs (2 graphs and report)
# 
showProgress<-TRUE

# function to clear 
resetExpected<-function(){
  expectedResult<<-list(result=list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                        nullresult=list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                        count=0,
                        nullcount=0,
                        nsims=0,
                        running=FALSE)
}
# and do it at the start
resetExpected()

notRunningExpected<-TRUE

# here's where we start a run
observeEvent(c(input$EvidenceExpectedRun),{
  if (notRunningExpected) {
    startTime<<-Sys.time()
    cycleTime<<-0
    cycleCount<<-0
    if (!input$EvidenceExpected_append) {resetExpected()} 
    if (!shortHand) {
      expectedResult$nsims<<-expectedResult$count+as.numeric(input$EvidenceExpected_length)
    } else {
      expectedResult$nsims<<-expectedResult$count+as.numeric(input$EvidenceExpected_length)*shortHandGain
    }
    if (input$EvidenceExpectedRun>0) {
      updateActionButton(session,"EvidenceExpectedRun",label=stopLabel)
      notRunningExpected<<-FALSE
    }
  } else {
    expectedResult$nsims<<-expectedResult$count
    updateActionButton(session,"EvidenceExpectedRun",label="Run")
    notRunningExpected<<-TRUE
  }
})


# UI changes
# go to the expected tabs 
expectedUpdate<-observeEvent(input$EvidenceExpectedRun,{
  if (input$EvidenceExpectedRun>0) {
    updateTabsetPanel(session, "Graphs",selected = "Expect")
    updateTabsetPanel(session, "Reports",selected = "Expect")
    validExpected<<-TRUE
  }
}
,priority=100
)

observeEvent(input$EvidenceExpected_type,{
  if (!is.element(input$EvidenceExpected_type,c("NHSTErrors","FDR","CILimits","2D"))) {
    switch(input$EvidenceExpected_type,
           "EffectSize"={
             updateSelectInput(session,"EvidenceExpected_par1",selected="r")
             updateSelectInput(session,"EvidenceExpected_par2",selected="p")
           },
           "Power" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="nw")
             updateSelectInput(session,"EvidenceExpected_par2",selected="w")
           },
           "log(lrs)" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="p")
             updateSelectInput(session,"EvidenceExpected_par2",selected="log(lrs)")
           },           
           "log(lrd)" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="p")
             updateSelectInput(session,"EvidenceExpected_par2",selected="log(lrd)")
           }           
    )
  }
})

# set expected variable from UI
updateExpected<-function(){
  list(
    type=input$EvidenceExpected_type,
    Expected_par1=input$EvidenceExpected_par1,Expected_par2=input$EvidenceExpected_par2,
    nsims=as.numeric(input$EvidenceExpected_length),
    append=input$EvidenceExpected_append
  )
}    


# make this a stand-alone function to be called from observEvent
doExpectedAnalysis<-function(IV,IV2,DV,effect,design,evidence,expected,result,nsim=1) {
  if (debug) {debugPrint("     doExpectedAnalysis")}
  
  append<-TRUE
  # if (evidence$shortHand || design$sReplicationOn) {
  if (IV$process=="data" && DV$process=="data"){
    design$sMethod="Resample"
  }
  result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,nsim,append,result,sigOnly=evidence$sigOnly,showProgress=!showProgress)
  # } else {
  #   result<-sampleShortCut(IV,IV2,DV,effect,design,evidence,nsim,append,result,sigOnly=FALSE)
  # }
  if (debug) {debugPrint("     doExpectedAnalysis - exit")}
  result
}

# Expected outputs
# show expected result    
makeExpectedGraph <- function() {
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,input$EvidenceEffect_type,
          input$evidenceTheory,
          input$STMethod,input$alpha,
          input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
          input$EvidenceExpectedRun)
  
  cycleCount<<-cycleCount+1
  if (cycleCount<2) {
    silentTime<<-0
    pauseWait<<-10
  } else {
    cycleTime<-Sys.time()-time2
    silentTime<<-max(silentTime,cycleTime-pauseWait/1000)
    pauseWait<<-500
  }

  llrConsts<-c(input$llr1,input$llr2)
  
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  if (variablesHeld=="Data" && !applyingAnalysis && switches$doBootstrap) {design$sMethod<-"Resample"}
  
  expected<-updateExpected()
  
  expectedResult$result$showType<<-input$EvidenceEffect_type
  expectedResult$result$effect<<-effect
  expectedResult$result$design<<-design
  expectedResult$result$evidence<<-evidence
  expectedResult$nullresult$showType<<-input$EvidenceEffect_type
  expectedResult$nullresult$effect<<-nulleffect
  expectedResult$nullresult$design<<-design
  expectedResult$nullresult$evidence<<-evidence
  
  if (debug) {print("ExpectedPlot1 - start")}
  stopRunning<-TRUE
  
  if (!validExpected) {return(ggplot()+plotBlankTheme)}

  if (switches$showAnimation) {
    min_ns<-floor(log10(expectedResult$nsims/100))
    ns<-10^(floor(max(min_ns,log10(expectedResult$count))))
    if (expectedResult$count+ns>expectedResult$nsims) {
      ns<-expectedResult$nsims-expectedResult$count
    }
  } else {
    ns<-expectedResult$nsims-expectedResult$count
  }
  
  if (ns>0) {
      expected$doingNull<-FALSE
      if (showProgress) {
        if (expectedResult$count==0) {
          showNotification("Expected: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
        } else {
          showNotification(paste0("Expected: ",format(expectedResult$count),"/",format(expectedResult$nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
        }
      }
      expectedResult$result<<-doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected,expectedResult$result,ns)
  }
  
  if (expectedResult$count<expectedResult$nsims) {
    stopRunning<-FALSE
  }
  
  if (expected$type=="NHSTErrors" && 
        (!effect$world$worldOn || (effect$world$worldOn && effect$world$populationNullp==0)) &&
         expectedResult$nullcount<expectedResult$nsims) {
        ns<-expectedResult$count-expectedResult$nullcount
      if (ns>0) {
        expected$doingNull<-TRUE
        if (showProgress) {
          showNotification(paste0("Expected|Null: ",format(expectedResult$nullcount),"/",format(expectedResult$nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
        }
        expectedResult$nullresult<<-doExpectedAnalysis(IV,IV2,DV,updateEffect(NULL),design,evidence,expected,expectedResult$nullresult,ns)
      }
      if (expectedResult$nullcount<expectedResult$nsims) {
        stopRunning<-FALSE
      }
    }
    # wind up
    
    expectedResult$count<<-length(expectedResult$result$rIV)
    expectedResult$nullcount<<-length(expectedResult$nullresult$rIV)
    
    if (effect$world$worldOn && is.element(expected$type,c("NHSTErrors","FDR"))){
      nulls<-expectedResult$result$rpIV==0
      expectedResult$nullresult$rpIV<-expectedResult$result$rpIV[nulls]
      expectedResult$nullresult$roIV<-expectedResult$result$roIV[nulls]
      expectedResult$nullresult$rIV<-expectedResult$result$rIV[nulls]
      expectedResult$nullresult$pIV<-expectedResult$result$pIV[nulls]
      expectedResult$nullresult$nval<-expectedResult$result$nval[nulls]
      expectedResult$nullresult$df1<-expectedResult$result$df1[nulls]
      
      expectedResult$result$rpIV<-expectedResult$result$rpIV[!nulls]
      expectedResult$result$roIV<-expectedResult$result$roIV[!nulls]
      expectedResult$result$rIV<-expectedResult$result$rIV[!nulls]
      expectedResult$result$pIV<-expectedResult$result$pIV[!nulls]
      expectedResult$result$nval<-expectedResult$result$nval[!nulls]
      expectedResult$result$df1<-expectedResult$result$df1[!nulls]
      
      expectedResult$count<-length(expectedResult$result$rIV)
      expectedResult$nullcount<-length(expectedResult$nullresult$rIV)
    }
    
    
    # ? stop running
    if (stopRunning) {
      if (showProgress) {removeNotification(id = "counting")}
    }

  # if (expectedResult$count==0) { return(ggplot()+plotBlankTheme) }
  
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))
  g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  if (expected$type=="2D") {
    if (expectedResult$count==0) {
      g<-ggplot()+plotBlankTheme
    } else {
      g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1,expected$Expected_par2)
      g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=1,xmax=9,ymin=0,ymax=10)
    }
  } else {
    if (is.element(expected$type,c("NHSTErrors","FDR","CILimits","LLRDErrors"))) {
      switch (expected$type,
              "NHSTErrors"={
                g1<-e2_plot(expectedResult$result,effect=effect,nullresult=expectedResult$nullresult)
                g2<-e1_plot(expectedResult$nullresult,effect=effect,result=expectedResult$result)
              },
              "FDR"={
                g1<-e1_plot(expectedResult$nullresult,effect=effect)
                g2<-e2_plot(expectedResult$result,effect=effect)
              },
              "CILimits"=  {
                g1<-ci1_plot(expectedResult$result,effect=effect)
                g2<-ci2_plot(expectedResult$result,effect=effect)
              }
      )
    } else {
      g1<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1)
      g2<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par2)
    }
    g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)
    g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
  }
  
  if (!stopRunning) {
    time2<<-Sys.time()
    if (doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(10)
    }
  } else {
    updateActionButton(session,"EvidenceExpectedRun",label="Run")
    notRunningExpected<<-TRUE
  }
  return(g)
}

output$ExpectedPlot <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,input$EvidenceEffect_type,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot - exit")}
  g
})

output$ExpectedPlot1 <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,input$EvidenceEffect_type,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot - exit")}
  g
})

makeExpectedReport<-function() {
  llrConsts<-c(input$llr1,input$llr2)
  
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  expected<-updateExpected()
  expectedResult$result$showType<-input$EvidenceEffect_type
  
  if (expectedResult$count>1) {
    
    if (effect$world$worldOn && expected$type=="NHSTErrors"){
      nulls<-expectedResult$result$rpIV==0
      expectedResult$nullresult$rpIV<-expectedResult$result$rpIV[nulls]
      expectedResult$nullresult$roIV<-expectedResult$result$roIV[nulls]
      expectedResult$nullresult$rIV<-expectedResult$result$rIV[nulls]
      expectedResult$nullresult$pIV<-expectedResult$result$pIV[nulls]
      expectedResult$nullresult$nval<-expectedResult$result$nval[nulls]
      expectedResult$nullresult$df1<-expectedResult$result$df1[nulls]
      
      expectedResult$result$rpIV<-expectedResult$result$rpIV[!nulls]
      expectedResult$result$roIV<-expectedResult$result$roIV[!nulls]
      expectedResult$result$rIV<-expectedResult$result$rIV[!nulls]
      expectedResult$result$pIV<-expectedResult$result$pIV[!nulls]
      expectedResult$result$nval<-expectedResult$result$nval[!nulls]
      expectedResult$result$df1<-expectedResult$result$df1[!nulls]
      
      expectedResult$count<-length(expectedResult$result$rIV)
      expectedResult$nullcount<-length(expectedResult$nullresult$rIV)
    }
    
    g<-reportExpected(IV,IV2,DV,effect,evidence,expected,expectedResult$result,expectedResult$nullresult)
  } else {
    g<-ggplot()+plotBlankTheme
  }
  
  ns<-expectedResult$nsims-expectedResult$count
  if (effect$world$worldOn  && expected$type=="NHSTErrors") {
    ns<-expectedResult$nsims-expectedResult$count-expectedResult$nullcount
  } else {
    if (expected$type=="NHSTErrors") {
      ns<-expectedResult$nsims*2-expectedResult$count-expectedResult$nullcount
    }
  }
  if (ns>0) {
    # if (debug) {print("ExpectedPlot2 - timer set ")}
    if (doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(10)
    }
    # invalidateLater(pauseWait)
  } 
  
  return(g)
}

# expected report
output$ExpectedReport <- renderPlot({
  if (debug) debugPrint("ExpectedReport")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport - exit")}
  g
})

output$ExpectedReport1 <- renderPlot({
  if (debug) debugPrint("ExpectedReport")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport - exit")}
  g
})

##################################################################################    
