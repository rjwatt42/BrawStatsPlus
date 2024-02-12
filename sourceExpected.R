##################################################################################    
# EXPECTED    
# UI changes  
# set expected variable from UI
# calculations
# outputs (2 graphs and report)
# 


mergeExpected<-function(r1,r2) {
  newResult<-list(
    rpIV=rbind(r1$rpIV,r2$rpIV),
    rIV=rbind(r1$rIV,r2$rIV),
    pIV=rbind(r1$pIV,r2$pIV),
    roIV=rbind(r1$roIV,r2$roIV),
    poIV=rbind(r1$poIV,r2$poIV),
    nval=rbind(r1$nval,r2$nval),
    df1=rbind(r1$df1,r2$df1)
  )
  if (!is.null(IV2)) {
    newResult<-c(newResult,list(
      rIV2=rbind(r1$rIV2,r2$rIV2),
      pIV2=rbind(r1$pIV2,r2$pIV2),
      rIVIV2DV=rbind(r1$rIVIV2DV,r2$rIVIV2DV),
      pIVIV2DV=rbind(r1$pIVIV2DV,r2$rIVIV2DV),
      r=list(direct=rbind(r1$r$direct,r2$r$direct),
             unique=rbind(r1$r$unique,r2$r$unique),
             total=rbind(r1$r$total,r2$r$total)
      ),
      p=list(direct=rbind(r1$p$direct,r2$p$direct),
             unique=rbind(r1$p$unique,r2$p$unique),
             total=rbind(r1$p$total,r2$p$total)
      )
    )
    )
  }
}
# function to clear 
resetExpected<-function(nsims=0,append=FALSE){
  
  if (nsims>0) {
    b<-matrix(NA,nsims,1)
    bm<-matrix(NA,nsims,3)
  } else {
    b<-NULL
    bm<-NULL
  }
  newResult<-list(
    rpIV=b,rIV=b,pIV=b,roIV=b,poIV=b,nval=b,df1=b
  )
  if (!is.null(IV2)) {
    newResult<-c(newResult,list(
      rIV2=b,pIV2=b,rIVIV2DV=b,pIVIV2DV=b,
      r=list(direct=bm,unique=bm,total=bm),
      p=list(direct=bm,unique=bm,total=bm)
    )
    )
  }
  newNullResult<-newResult
  if (append) {
    newResult<-mergeExpected(expectedResult$result,newResult)
    newNullResult<-mergeExpected(expectedResult$nullresult,newNullResult)
    count<-expectedResult$count
    nullcount<-expectedResult$nullcount
  } else {
    count<-0
    nullcount<-0
  }
  newResult<-c(newResult,list(showType=NULL))
  newNullResult<-c(newNullResult,list(showType=NULL))
  
  list(result=newResult,
       nullresult=newNullResult,
       count=count,
       nullcount=nullcount,
       nsims=nsims+count)
}

# and do it at the start
expectedResult<<-resetExpected()

runningExpected<-FALSE

# here's where we start a run
observeEvent(c(input$EvidenceExpectedRun),{
  if (debugShiny) print("EvidenceExpectedRun1")
  if (input$EvidenceExpectedRun>0) {
    if (!runningExpected) {
      
      runLength<-as.numeric(input$EvidenceExpected_length)
      if (shortHand) runLength<-runLength*shortHandGain
      
      expectedResult<<-resetExpected(runLength,append = input$EvidenceExpected_append)

      stopButton(session,"EvidenceExpectedRun")
      runningExpected<<-TRUE
      
      startTime<<-Sys.time()
      cycleTime<<-0
      cycleCount<<-0
      
    } else {
      expectedResult$nsims<<-expectedResult$count
      startButton(session,"EvidenceExpectedRun")
      runningExpected<<-FALSE
    }
  }  
})


# UI changes
# go to the expected tabs 
observeEvent(input$EvidenceExpectedRun,{
  if (debugShiny) print("EvidenceExpectedRun2")
  if (input$EvidenceExpectedRun>0) {
    updateTabsetPanel(session, "Graphs",selected = "Expect")
    updateTabsetPanel(session, "Reports",selected = "Expect")
    validExpected<<-TRUE
  }
}
,priority=100
)

observeEvent(input$EvidenceExpected_type,{
  if (debugShiny) print("EvidenceExpected_type")
  if (!is.element(input$EvidenceExpected_type,c("NHSTErrors","FDR","CILimits","2D","Single"))) {
    switch(input$EvidenceExpected_type,
           "Basic"={
             updateSelectInput(session,"EvidenceExpected_par1",selected=RZ)
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
  expected<-list(
    type=input$EvidenceExpected_type,
    Expected_par1=input$EvidenceExpected_par1,Expected_par2=input$EvidenceExpected_par2,
    nsims=as.numeric(input$EvidenceExpected_length),
    append=input$EvidenceExpected_append
  )
  if (input$EvidenceExpected_type=="Basic") {
    expected$Expected_par1<-"r"
    expected$Expected_par2<-"p"
  }
  expected
}    


# make this a stand-alone function to be called from observEvent
doExpectedAnalysis<-function(IV,IV2,DV,effect,design,evidence,expected,result,nsim=1) {
  if (debug) {debugPrint("     doExpectedAnalysis")}
  
  append<-TRUE
  # if (evidence$shortHand || design$sReplicationOn) {
  if (IV$process=="data" && DV$process=="data"){
    design$sMethod="Resample"
  }
  result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,nsim,append,result,sigOnly=evidence$sigOnly)
  
  if (debug) {debugPrint("     doExpectedAnalysis - exit")}
  result
}

# Expected outputs
# show expected result    
makeExpectedGraph <- function() {
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,input$EvidenceEffect_type1,
          input$evidenceTheory,
          input$STMethod,input$alpha,input$RZ,
          input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
          input$EvidenceExpectedRun)
  
  silentTime<<-0
  if (cycleCount>1) {
    cycleTime<-Sys.time()-time2
    silentTime<<-max(silentTime,cycleTime-pauseWait/1000)
  }

  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  if (variablesHeld=="Data" && !applyingAnalysis && switches$doBootstrap) {design$sMethod<-"Resample"}
  
  expected<-updateExpected()
  
  expectedResult$result$showType<<-evidence$showType
  expectedResult$result$effect<<-effect
  expectedResult$result$design<<-design
  expectedResult$result$evidence<<-evidence
  expectedResult$nullresult$showType<<-evidence$showType
  expectedResult$nullresult$effect<<-nulleffect
  expectedResult$nullresult$design<<-design
  expectedResult$nullresult$evidence<<-evidence
  
  stopRunning<-TRUE
  
  if (validExpected) {
    min_ns<-floor(log10(expectedResult$nsims/100))
    if (switches$showAnimation) {
      ns<-ceil(10^(floor(max(min_ns,log10(expectedResult$count)))))
      n_cycles<-1
    } else {
      if (switches$showProgress) {
        ns<-10^min_ns
        n_cycles<-ceil(expectedResult$nsims/ns)
      } else {
        ns<-expectedResult$nsims
        n_cycles<-1
      }
    }
    
    if (expectedResult$count+ns>expectedResult$nsims) {
      ns<-expectedResult$nsims-expectedResult$count
    }
    
    if (ns>0) {
      expected$doingNull<-FALSE
      if (expectedResult$count==0)
        if (switches$showProgress) 
          showNotification("Expected: starting",id="multiple",duration=Inf,closeButton=FALSE,type="message")
      for (ci in 1:n_cycles) {
        newCount<-expectedResult$count+ns
        expectedResult$result<<-doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected,expectedResult$result,ns)
        expectedResult$count<<-newCount
        if (switches$showProgress) 
          showNotification(paste0("Expected: ",format(expectedResult$count),"/",format(expectedResult$nsims)),id="multiple",duration=Inf,closeButton=FALSE,type="message")
      }
    }
    
    if (expectedResult$count<expectedResult$nsims)  stopRunning<-FALSE
    else stopRunning<-TRUE
    
    if (expected$type=="NHSTErrors" && 
        (!effect$world$worldOn || (effect$world$worldOn && effect$world$populationNullp==0)) &&
        expectedResult$nullcount<expectedResult$nsims) {
      ns<-expectedResult$count-expectedResult$nullcount
      if (ns>0) {
        expected$doingNull<-TRUE
        if (switches$showProgress)
          showNotification(paste0("Expected|Null: ",format(expectedResult$nullcount),"/",format(expectedResult$nsims)),id="multiple",duration=Inf,closeButton=FALSE,type="message")
        newCount<-expectedResult$nullcount+ns
        expectedResult$nullresult<<-doExpectedAnalysis(IV,IV2,DV,updateEffect(NULL),design,evidence,expected,expectedResult$nullresult,ns)
      }
      expectedResult$nullcount<<-newCount
      if (expectedResult$nullcount<expectedResult$nsims) {
        stopRunning<-FALSE
      }
    }
    
    # wind up
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
      
      expectedResult$count<-sum(!is.na(expectedResult$result$rIV))
      expectedResult$nullcount<-sum(!is.na(expectedResult$nullresult$rIV))
    }

    # ? stop running
    if (stopRunning) 
      if (switches$showProgress)
        removeNotification(id = "multiple")

  }
  
    switch(expected$type,
           "Basic"=           {
             g1<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,"r")
             g2<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,"p")
           },
           "2D"={
             if (expectedResult$count==0) {
               g1<-ggplot()+plotBlankTheme
             } else {
               g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1,expected$Expected_par2)
             }
             g2<-NULL
           },
           "Single"={
             g1<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1,orientation="horz")
             g2<-NULL
           },
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
           },
           {
             g1<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1)
             g2<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par2)
           }
           )
    if (!is.null(g2)) {
      g<-joinPlots(g1,g2)
    } else {
      g<-joinPlots(g1)
    }
    
  if (!stopRunning) {
    time2<<-Sys.time()
    if (switches$doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(pauseWait)
    }
  } else {
    startButton(session,"EvidenceExpectedRun")
    runningExpected<<-FALSE
  }
  return(g)
}


output$ExpectedPlot <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot - start")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,input$EvidenceEffect_type1,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot - exit")}
  g
})

output$ExpectedPlot1 <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot - start")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,input$EvidenceEffect_type1,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot - exit")}
  g
})

makeExpectedReport<-function() {

  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  expected<-updateExpected()
  expectedResult$result$showType<-evidence$showType
  
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
    if (switches$doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(pauseWait)
    }
  } 
  
  return(g)
}

# expected report
output$ExpectedReport <- renderPlot({
  if (debug) debugPrint("ExpectedReport - start")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport - exit")}
  g
})

output$ExpectedReport1 <- renderPlot({
  if (debug) debugPrint("ExpectedReport - start")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport - exit")}
  g
})

##################################################################################    
