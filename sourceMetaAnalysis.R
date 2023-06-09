# META-ANALYSIS
# UI changes
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    
validMeta<<-FALSE
notRunningMeta<-TRUE

# UI changes
observeEvent(input$metaRun,{
  if (debug) print("metaRun")
  if (input$metaRun>0) {
    if (notRunningMeta) {
      updateTabsetPanel(session, "Graphs",selected = "MetaAnalysis")
      updateTabsetPanel(session, "Reports",selected = "MetaAnalysis")
      validMeta<<-TRUE
      if (!input$meta_append) {
        resetMeta()
      }
      metaResult$nsims<<-metaResult$count+as.numeric(input$meta_runlength)
      updateActionButton(session,"metaRun",label=stopLabel)
      updateActionButton(session,"LGmetaRun",label=stopLabel)
      notRunningMeta<<-FALSE
    } else {
      metaResult$nsims<<-metaResult$count
      updateActionButton(session,"metaRun",label="Run")
      updateActionButton(session,"LGmetaRun",label="Run")
      notRunningMeta<<-TRUE
    }
  }
}
,priority=100
)

observeEvent(input$LGmetaRun,{
  if (debug) print("LGmetaRun")
  if (input$LGmetaRun>0) {
    if (notRunningMeta) {
      validMeta<<-TRUE
      if (!input$LGmeta_append) {
        resetMeta()
      }
      metaResult$nsims<<-metaResult$count+as.numeric(input$LGmeta_runlength)
      updateActionButton(session,"metaRun",label=stopLabel)
      updateActionButton(session,"LGmetaRun",label=stopLabel)
      notRunningMeta<<-FALSE
    } else {
      metaResult$nsims<<-metaResult$count
      updateActionButton(session,"metaRun",label="Run")
      updateActionButton(session,"LGmetaRun",label="Run")
      notRunningMeta<<-TRUE
    }
  }
}
,priority=100
)

applyingMetaAnalysis<-FALSE


# set expected variable from UI
updateMetaAnalysis<-function(){
  metaAnalysis<-list(
    nstudies=input$meta_nStudies,
    meta_fixedAnal="random",
    meta_pdf=input$meta_pdf,
    sig_only=input$meta_psigStudies,
    meta_psigAnal=input$meta_psigAnal,
    meta_nullAnal=input$meta_nullAnal,
    nsims=as.numeric(input$meta_runlength),
    meta_showAnal=input$meta_showAnal,
    meta_showParams=input$meta_showParams,
    showTheory=input$evidenceTheory,
    append=input$meta_append
  )
  if (metaAnalysis$meta_pdf!="All") metaAnalysis$meta_showAnal<-metaAnalysis$meta_pdf
  metaAnalysis
}    

# function to clear 
resetMeta<-function(){
  metaResult<<-list(single=list(kmax=c(),Smax=c()),
                    gauss=list(kmax=c(),Smax=c()),
                    exp=list(kmax=c(),Smax=c()),
                    bestDist=c(),
                    bestK=c(),
                    bestNull=c(),
                    bestS=c(),
                    count=0,
                    nsims=0,
                    result=list(rpIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),
                                r=list(direct=c(),unique=c(),total=c(),coefficients=c()),
                                p=list(direct=c(),unique=c(),total=c())
                    )
  )
}
# and do it at the start
resetMeta()

# make this a stand-alone function to be called from observEvent
doMetaAnalysis<-function(IV,IV2,DV,effect,design,evidence,metaAnalysis,metaResult) {
  if (debug) {print("     doMetaAnalysis - start")}
  # if (metaAnalysis$shortHand) {
  result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis$nstudies,FALSE,metaResult$result,sigOnly=metaAnalysis$sig_only,
                           showProgress=FALSE)
  # } else {
  #   nsims<-metaAnalysis$nstudies
  #   result<-sampleShortCut(IV,IV2,DV,effect,design,evidence,metaAnalysis$nstudies,FALSE,metaResult$result,sigOnly=metaAnalysis$sig_only)
  # }
  metaResult$result<-result
  metaResult<-runMetaAnalysis(metaAnalysis,metaResult)
  metaResult$effect<-effect
  metaResult$design<-design
  if (debug) {print("     doMetaAnalysis - end")}
  metaResult
}

# Expected outputs
# show expected result    
makeMetaGraph <- function() {
  doit<-c(input$metaRun,input$LGmetaRun)
  
  if (!validMeta) {return(ggplot()+plotBlankTheme)}
  
  if (metaResult$count<2) {
    silentTime<<-0
    pauseWait<<-10
  } else {
  if (metaResult$count==2) {
    silentTime<<-Sys.time()-time2
  } 
  if (metaResult$count>2 && metaResult$count<=cycles2observe) {
    silentTime<<-max(silentTime,Sys.time()-time2)
  }
  if (metaResult$count>cycles2observe) {
    pauseWait<<-100
  }
  }
  
  IV<-updateIV()
  IV2<-updateIV2()
  DV<-updateDV()
  
  effect<-updateEffect()
  design<-updateDesign()
  evidence<-updateEvidence()
  
  metaAnalysis<-updateMetaAnalysis()
  
  if (debug) {print("MetaPlot1 - start")}
  if (showProgress) {
    showNotification("MetaAnalysis: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
  }
  
  stopRunning<-TRUE
  if (metaResult$count<metaResult$nsims) {
    stopRunning<-FALSE
    metaAnalysis$append<-TRUE
    ns<-10^(min(2,floor(log10(max(1,metaResult$count)))))
    if (showProgress) {
      showNotification(paste0("A MetaAnalysis: ",metaResult$count,"/",metaResult$nsims),id="counting",duration=Inf,closeButton=FALSE,type="message")
    }
    for (i in 1:ns) {
      metaResult<<-doMetaAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis,metaResult)
    }
  }
  if (metaResult$count>=metaResult$nsims) {
    if (showProgress) {removeNotification(id = "counting")}
  }
  
  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))
  g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  if (metaAnalysis$nsims==1) {
    g1<-drawMeta(metaAnalysis,metaResult,"Plain")
    g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
    
  } else {
    switch (metaAnalysis$meta_showAnal,
            "All"={
              if (includeSingle)  {
                g1<-drawMeta(metaAnalysis,metaResult,"Single",yaxis=TRUE)
                g2<-drawMeta(metaAnalysis,metaResult,"Gauss",yaxis=FALSE)
                g3<-drawMeta(metaAnalysis,metaResult,"Exp",yaxis=FALSE)
                g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=4,ymin=0,ymax=10)+
                  annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=4,xmax=7,ymin=0,ymax=10)+
                  annotation_custom(grob=ggplotGrob(g3+gridTheme),xmin=7,xmax=10,ymin=0,ymax=10)
              } else {
                if (metaAnalysis$meta_showParams=="S-S") {
                  g2<-drawMeta(metaAnalysis,metaResult,"S-S",yaxis=TRUE)
                  g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=0.5,xmax=9.5,ymin=0,ymax=10)
                } else {
                  g2<-drawMeta(metaAnalysis,metaResult,"Gauss",yaxis=TRUE)
                  g3<-drawMeta(metaAnalysis,metaResult,"Exp",yaxis=FALSE)
                  g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=0.5,xmax=5.5,ymin=0,ymax=10)+
                    annotation_custom(grob=ggplotGrob(g3+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=10)
                }
              }  
            },
    "Single"={
      g1<-drawMeta(metaAnalysis,metaResult,"Single",yaxis=TRUE)
      g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
    },
    "Gauss"={
      g1<-drawMeta(metaAnalysis,metaResult,"Gauss",yaxis=TRUE)
      g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
    },
    "Exp"={
      g1<-drawMeta(metaAnalysis,metaResult,"Exp",yaxis=TRUE)
      g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
    }
    )
  }
  
  time2<<-Sys.time()
  if (!stopRunning) {
    if (doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(10)
    }
  } else {
    updateActionButton(session,"metaRun",label="Run")
    updateActionButton(session,"LGmetaRun",label="Run")
    notRunningMeta<<-TRUE
  }
  
  if (debug) {print("MetaPlot1 - plots done ")}
  return(g)
}

output$MetaAnalysisPlot <- renderPlot({
  doit<-c(input$metaRun)
  makeMetaGraph()
})


output$MetaAnalysisReport <- renderPlot({
  doit<-c(input$metaRun)
  
  if (metaResult$count>0) {
    g<-reportMetaAnalysis(metaResult)
  } else {
    g<-ggplot()+plotBlankTheme
  }

  if (metaResult$count<metaResult$nsims) {
    if (doStop) {
      invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
    } else {
      invalidateLater(10)
    }
  }
  
  return(g)      
})

##################################################################################    
