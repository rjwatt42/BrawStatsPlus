#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

debug<-FALSE
debugExitOnly<-TRUE
debugMainOnly<-FALSE
debugNow<-Sys.time()
debugNowLocation<<-"Start"
debugStart<-Sys.time()

debugPrint<-function(s) {
  if (debugMainOnly && substr(s,1,1)==" ") return()
  
    if (grepl("exit",s)==1) {
      z<-regexpr("[ ]*[a-zA-Z0-9]*",s)
      startStr<-regmatches(s,z)
      use<-which(debugNowLocation==startStr)
      use<-use[length(use)]
    elapsed<-as.numeric(difftime(Sys.time(),debugNow[use],units="secs"))
    print(paste0(format(Sys.time()-debugStart)," (",format(elapsed,digits=3),") ",startStr))
  } else {
    if (!debugExitOnly || s=="Opens")
    print(paste(format(Sys.time()-debugStart),s))
    debugNow<<-c(debugNow,Sys.time())
    debugNowLocation<<-c(debugNowLocation,s)
  }
  }

if (debug) debugPrint("Opens")

source("plotStatistic.R")
source("plotES.R")
source("plotReport.R")

source("drawVariable.R")
source("drawPopulation.R")
source("drawPrediction.R")

source("drawSample.R")
source("drawDescription.R")
source("drawInference.R")
source("drawMeta.R")
source("drawExplore.R")
source("drawLikelihood.R")

source("sampleMake.R")
source("sampleAnalyse.R")
source("sampleLikelihood.R")
source("samplePower.R")
source("sampleRead.R")
source("sampleCheck.R")
source("Johnson_M.R")
source("sampleShortCut.R")

source("reportSample.R")
source("reportDescription.R")
source("reportInference.R")
source("reportExpected.R")
source("reportMetaAnalysis.R")
source("reportExplore.R")
source("reportLikelihood.R")

source("runMetaAnalysis.R")
source("runExplore.R")
source("runLikelihood.R")
source("runBatchFiles.R")

source("wsRead.R")
source("typeCombinations.R")

source("drawInspect.R")
source("isSignificant.R")

graphicSource="Main"

####################################

shinyServer(function(input, output, session) {
  if (debug) debugPrint("Start")
  
  source("myGlobal.R")
  source("runDebug.R")
  
####################################
# BASIC SET UP that cannot be done inside ui.R  
  shinyjs::hideElement(id= "EvidenceHypothesisApply")
  shinyjs::hideElement(id= "LGEvidenceHypothesisApply")
  shinyjs::hideElement(id= "Using")
  shinyjs::hideElement(id="EvidenceExpectedStop")
  updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
  updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
  updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])
  
####################################
  if (debug) debugPrint("ServerKeys")
  
  source("loadExtras.R")
  source("serverKeys.R",local=TRUE)
  
  observeEvent(input$LoadExtras, {
                 loadExtras(session,input$LoadExtras)
               })
  
  observeEvent(input$shortHandGain, {
    shortHandGain<<-input$shortHandGain
  }
  )
  observeEvent(input$shortHand, {
    shortHand<<-input$shortHand
  }
  )
  
####################################
# other housekeeping
  if (debug) debugPrint("Housekeeping")
  observeEvent(input$allScatter,{
    allScatter<<-input$allScatter
  }
  )

  observeEvent(input$Explore_VtypeH, {
      if (input$Explore_VtypeH=="levels") {
        updateSelectInput(session,"Explore_typeH",selected="DV")
      }
  }
  )
  
  observeEvent(input$sN, {
    #   worldOn<-input$world_on
    # if (is.null(worldOn)) {
    #   worldOn<-FALSE
    # }
    # if (!worldOn) {
    #   n<-input$sN
    #   if (!is.null(n) && !is.na(n)) {
    #     if (n<1 && n>0) {
    #       n<-rw2n(input$rIV,n,2)
    #       updateNumericInput(session,"sN",value=n)
    #     }
    #   }
    # }
    
    before<-paste0("<div style='",localStyle,"'>")
    after<-"</div>"
      n<-input$sN
      if (!is.null(n) && !is.na(n)) {
        if (n<1 && n>0) {
          html("sNLabel",paste0(before,"Sample Power:",after))
        } else {
          html("sNLabel",paste0(before,"Sample Size:",after))
        }
      }
  }
  )
  
  observeEvent(c(input$Notation1,input$Notation2,input$Notation3), {
    oldPlus<-pPlus
    setNotation(list(psig=input$Notation1,
                     UD=input$Notation2,
                     P=input$Notation3))
    if (pPlus!=oldPlus)
      updateNumericInput(session,"world_distr_Nullp",value=1-input$world_distr_Nullp)
  })
  
  observeEvent(input$Hypothesis,{
    if (input$Hypothesis=="World") {
      updateTabsetPanel(session,"HypothesisDiagram",selected = "World")
    }
  })
  
  observeEvent(input$Evidence,{
    if (input$Evidence=="Expected") {
      updateTabsetPanel(session,"Graphs",selected = "Expected")
    }
    if (input$Evidence=="MetaAnalysis") {
      updateTabsetPanel(session,"Graphs",selected = "MetaAnalysis")
    }
  })
  
  observeEvent(input$world_distr, {
    if (input$world_distr!="Single" && input$world_distr_k==0) {
      updateNumericInput(session,"world_distr_k",value=0.2)
    }
  }
  )
  
  observeEvent(input$STMethod, {
    STMethod<<-input$STMethod
    switch (STMethod,
            "NHST"={
              updateNumericInput(session,"alpha",value=alpha)
              shinyjs::hideElement("evidencePrior")
              shinyjs::hideElement("STPrior")
              shinyjs::hideElement("evidenceLLR1")
              shinyjs::hideElement("evidenceLLR2")
              shinyjs::hideElement("llr1")
              shinyjs::hideElement("llr2")
            },
            "sLLR"={
              shinyjs::hideElement("evidencePrior")
              shinyjs::hideElement("STPrior")
              shinyjs::showElement("evidenceLLR1")
              shinyjs::showElement("evidenceLLR2")
              shinyjs::showElement("llr1")
              shinyjs::showElement("llr2")
              },
            "dLLR"={
              shinyjs::showElement("evidencePrior")
              shinyjs::showElement("STPrior")
              shinyjs::hideElement("evidenceLLR1")
              shinyjs::hideElement("evidenceLLR2")
              shinyjs::hideElement("llr1")
              shinyjs::hideElement("llr2")
            }
    )
  })
  observeEvent(input$alpha, {
    alpha<<-input$alpha
    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
  })
  
  observeEvent(input$evidenceInteractionOnly,{
    showInteractionOnly<<-input$evidenceInteractionOnly
  })
  
  observeEvent(input$pScale,{
    pPlotScale<<-input$pScale
  })
  
  observeEvent(input$wScale,{
    wPlotScale<<-input$wScale
  })
  
  observeEvent(input$nScale,{
    nPlotScale<<-input$nScale
  })
  
  observeEvent(input$RZ,{
    RZ<<-input$RZ
  })
  
  observeEvent(input$EvidenceExpected_type,{
    if (input$EvidenceExpected_type=="NHSTErrors") {
      shinyjs::hideElement("EvidenceExpected_par1")
      shinyjs::hideElement("EvidenceExpected_par2")
    } else {
      shinyjs::showElement("EvidenceExpected_par1")
      shinyjs::showElement("EvidenceExpected_par2")
    }
  })
  
####################################
# generic warning dialogue
  
  hmm<-function (cause) {
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title="Careful now!",
                  size="s",
                  cause,
                  
                  footer = tagList( 
                    actionButton("MVproceed", "OK")
                  )
      )
    )
  }
  
  observeEvent(input$MVproceed, {
    removeModal()
  })
  
####################################
# QUICK HYPOTHESES
  
  if (debug) debugPrint("QuickHypotheses")
  
  observeEvent(input$Hypchoice,{
    result<-getTypecombination(input$Hypchoice)
    validSample<<-FALSE
    
    setIVanyway(result$IV)
    setIV2anyway(result$IV2)
    setDVanyway(result$DV)
    
    updateSelectInput(session,"sIV1Use",selected=result$IV$deploy)
    updateSelectInput(session,"sIV2Use",selected=result$IV2$deploy)

    # 3 variable hypotheses look after themselves
    #
    if (!is.null(IV2)) {
      editVar$data<<-editVar$data+1
    }    
  })
  
  observeEvent(input$Effectchoice,{
    switch (input$Effectchoice,
            "e0"={
              updateNumericInput(session,"rIV",value=0)    
              updateNumericInput(session,"rIV2",value=0)    
              updateNumericInput(session,"rIVIV2",value=0)    
              updateNumericInput(session,"rIVIV2DV",value=0)    
            },
            "e1"={
              updateNumericInput(session,"rIV",value=0.3)    
              updateNumericInput(session,"rIV2",value=-0.3)    
              updateNumericInput(session,"rIVIV2",value=0.0)    
              updateNumericInput(session,"rIVIV2DV",value=0.5)    
            },
            "e2"={
              updateNumericInput(session,"rIV",value=0.2)    
              updateNumericInput(session,"rIV2",value=0.4)    
              updateNumericInput(session,"rIVIV2",value=-0.8)    
              updateNumericInput(session,"rIVIV2DV",value=0.0)    
            }
    )
    
  })
  
source("sourceUpdateData.R",local=TRUE)
  
####################################
# VARIABLES  
  if (debug) debugPrint("Variables")

    # make basic variables    
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  MV<-IV

  source("sourceInspectVariables.R",local=TRUE)
  source("sourceVariables.R",local=TRUE)
  
  source("sourceLGDisplay.R",local=TRUE)
  
  source("sourceUpdateVariables.R",local=TRUE)
  source("sourceUpdateSystem.R",local=TRUE)
  
  source("sourceSystemDiagrams.R",local=TRUE)
  
  source("sourceSingle.R",local=TRUE)
  source("sourceMetaAnalysis.R",local=TRUE)
  source("sourceExpected.R",local=TRUE)
  
  source("sourceExplore.R",local=TRUE)
  
  source("sourceLikelihood.R",local=TRUE)
  source("sourceFiles.R",local=TRUE)
  # end of everything        
  
  if (debug) debugPrint("Opens - exit")
})

