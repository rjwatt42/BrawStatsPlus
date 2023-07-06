####################################
#KEYBOARD: capture keyboard events

loadExtras<-function(which=0){
  
  # replications
  if (!switches$doReplications) {
    switches$doReplications<<-TRUE
    insertTab("Design",replicationTab(),"Anomalies","after",select=FALSE,session)
  }
  # worlds
  if (!switches$doWorlds) {
    switches$doWorlds<<-TRUE
    insertTab("Hypothesis",worldPanel(),"Effects","after",select=FALSE,session)
    insertTab("HypothesisDiagram",worldDiagram(),"Hypothesis","after",select=FALSE,session)
    updateSelectInput(session,"likelihoodUseSource",choices=c("null","world","prior"))
    updateSelectInput(session,"likelihoodUsePrior",choices=c("none","world","prior"))
  }
  # cheating
  if (!switches$doCheating) {
    switches$doCheating<<-TRUE
  }
  # likelihood inferences
  if (!switches$doLikelihoodInfer) {
    switches$doLikelihoodInfer<<-TRUE
  }
  # meta-analysis
  if (which==1 && !switches$doMetaAnalysis) {
    switches$doMetaAnalysis<<-TRUE
    insertTab("Evidence",metaPanel(),"Multiple","after",select=FALSE,session)
    insertTab("Graphs",metaGraphPanel(),"Expect","after",select=FALSE,session)
    insertTab("Reports",metaReportPanel(),"Expect","after",select=FALSE,session)
    insertTab("ExploreTab",exploreMeta(),"Design","after",select=FALSE,session)
    insertTab("FileTab",metaFilePanel(),"Data","after",select=FALSE,session)
  }
  # explore
  updateSelectInput(session,"Explore_typeH",choices=hypothesisChoicesV2Extra)
  updateSelectInput(session,"LGExplore_typeH",choices=hypothesisChoicesV2Extra)
  updateSelectInput(session,"Explore_typeD",choices=designChoicesExtra)
  updateSelectInput(session,"LGExplore_typeD",choices=designChoicesExtra)
  
  updateSelectInput(session,"Explore_showH",choices=showChoicesExtra)
  updateSelectInput(session,"LGExplore_showH",choices=showChoicesExtra)
  updateSelectInput(session,"Explore_showD",choices=showChoicesExtra)
  updateSelectInput(session,"LGExplore_showD",choices=showChoicesExtra)
  
  updateSelectInput(session,"EvidenceInfer_type",choices=singleTypeChoicesExtra)
  updateSelectInput(session,"EvidenceExpected_type",choices=multipleTypeChoicesExtra)
  updateSelectInput(session,"EvidenceExpected_par1",choices=inferTypeChoicesExtra,selected="r")
  updateSelectInput(session,"EvidenceExpected_par2",choices=inferTypeChoicesExtra,selected="p")
}

ascii<-function(ch) strtoi(charToRaw(toupper(ch)),16L)

if (switches$doKeys) {
  keyrespond<-observeEvent(input$pressedKey,{
    # 
    
    if (input$keypress==16) shiftKeyOn<<-TRUE
    if (input$keypress==17) controlKeyOn<<-TRUE
    if (input$keypress==18) altKeyOn<<-TRUE

    # control-alt-n - switch to online version
    if (is_local && input$keypress==ascii("n") && controlKeyOn && altKeyOn){
      switches$doReplications<<-FALSE
      switches$doWorlds<<-FALSE
      removeTab("Design","Replicate",session)
      removeTab("Hypothesis","World",session)
      removeTab("HypothesisDiagram","World",session)
      removeTab("FileTab","Batch",session)
    }
    
    # control-x - switch to (offline) full version
    if (input$keypress==ascii("x") && controlKeyOn){
      loadExtras()
    }
    
    # control-m - add in meta-analysis
    if (input$keypress==ascii("m") && controlKeyOn){
      loadExtras(1)
    }
    
    if (input$keypress==ascii("d") && controlKeyOn){
      updateSelectInput(session,"STMethod",selected="dLLR")
      updateSelectInput(session,"Explore_showD",selected="NHSTErrors")
      updateCheckboxInput(session,"Explore_xlog",value=TRUE)
      updateNumericInput(session,"Explore_nRange",value=10000)
    }
    
    if (input$keypress==ascii("z") && controlKeyOn){
      updateSelectInput(session,"RZ",selected="z")
    }
    
    # control-v
    if (is_local && input$keypress==ascii("v") && controlKeyOn){
      mergeVariables<<-FALSE
      # get the raw data
      raw_h1<-read_clip()
      header<-strsplit(raw_h1[1],"\t")[[1]]
      raw_data<-read_clip_tbl()
      # read_clip_tbl doesn't like some characters like | and =
      colnames(raw_data)<-header
      if (nrow(raw_data)>0 && ncol(raw_data)>0)
        getNewVariables(raw_data)
    }
    
    # control-c
    if (is_local && input$keypress==ascii("c") && controlKeyOn){
      data<-exportData()
      write_clip(data,allow_non_interactive = TRUE)
    }
    
    # control-alt-p set world to model psych
    if (input$keypress==ascii("p") && controlKeyOn && !shiftKeyOn){
      loadExtras()
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.325)
      updateNumericInput(session,"world_distr_Nullp",value=0.74)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
      
      updateSelectInput(session,"likelihoodPrior_distr",selected="Exp")
      updateSelectInput(session,"likelihoodPrior_distr_rz",selected="z")
      updateNumericInput(session,"likelihoodPrior_distr_k",value=0.325)
      updateNumericInput(session,"likelihoodPrior_Nullp",value=0.74)
    }
    
    if (input$keypress==ascii("p") && controlKeyOn && shiftKeyOn){
      loadExtras()
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.325)
      updateNumericInput(session,"world_distr_Nullp",value=0.74)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateNumericInput(session,"sNRandK",value=1.2)
      updateNumericInput(session,"sN",value=72)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-e set world to exp(0.2)
    if (input$keypress==ascii("e") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-g set world to gauss(0.2)
    if (input$keypress==ascii("g") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Gauss")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-s set world to single(0.2)
    if (input$keypress==ascii("s") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Single")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"rIV",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-l set shortHand to TRUE
    if (input$keypress==ascii("l") && controlKeyOn){
      updateCheckboxInput(session,"shortHand",value=TRUE)
      updateSelectInput(session,"EvidenceExpected_length",selected="1000")
      updateSelectInput(session,"Explore_lengthH",selected="100")
      updateSelectInput(session,"Explore_lengthD",selected="100")
    }
    
    # control-1 set up World A
    if (input$keypress==ascii("1") && controlKeyOn){
      loadExtras()
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.325)
      updateNumericInput(session,"world_distr_Nullp",value=1)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateNumericInput(session,"sNRandK",value=1.2)
      updateNumericInput(session,"sN",value=21)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-2 set up World B
    if (input$keypress==ascii("2") && controlKeyOn){
      loadExtras()
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.325)
      updateNumericInput(session,"world_distr_Nullp",value=0)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateNumericInput(session,"sNRandK",value=1.2)
      updateNumericInput(session,"sN",value=21)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-t set showTheory to TRUE
    if (input$keypress==ascii("t") && controlKeyOn){
      updateCheckboxInput(session,"evidenceTheory",value=TRUE)
      updateCheckboxInput(session,"likelihoodTheory",value=TRUE)
    }
    
    # control-alt-n set sample size to big (1000)
    if (input$keypress==78 && controlKeyOn && altKeyOn){
      updateNumericInput(session,"sN",value=1000)
    }
    
    # control-alt-f set effect size to 0.3
    if (input$keypress==70 && controlKeyOn && altKeyOn){
      updateNumericInput(session,"rIV",value=0.3)
    }
    
    # control-alt-r set replication
    if (input$keypress==82 && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"sReplicationOn",value=TRUE)
      updateNumericInput(session,"sReplRepeats",value=3)
    }
    
    # control-alt-3 set IV2
    if (input$keypress==51 && controlKeyOn && altKeyOn){
      updateSelectInput(session,"IV2choice",selected="IV2")
    }
    
    # control-alt-w set sample usage to within
    if (input$keypress==87 && controlKeyOn && altKeyOn){
      updateSelectInput(session,"sIV1Use",selected="Within")
      updateSelectInput(session,"sIV2Use",selected="Within")
    }
    
    # control-alt-d do debug
    if (input$keypress==68 && controlKeyOn && altKeyOn){
      toggleModal(session, modalId = "debugOutput", toggle = "open")
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      expected<-updateExpected()
      
      validSample<<-TRUE
      
      if (is.null(IV2)) {
        nc=7
        effect$rIV=0.3
      } else {
        nc=12
        effect$rIV=0.3
        effect$rIV2=-0.3
        effect$rIVIV2DV=0.5
      }
      design$sN<-1000
      
      expected$nSims<-100
      expected$EvidenceExpected_type<-"EffectSize"
      expected$append<-FALSE
      
      if (is.null(IV2)) {
        result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
      }
      doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
      op<-runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult)
      
      if (!is.null(IV2)) {
        effect$rIVIV2=0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
        
        effect$rIVIV2=-0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
      }
      
      output$plotPopUp<-renderPlot(reportPlot(op,nc,length(op)/nc,2))
      return()
    }
    
  })
  
  keyrespondUp<-observeEvent(input$keyrelease,{
    if (input$keyrelease==18) altKeyOn<<-FALSE
    if (input$keyrelease==17) controlKeyOn<<-FALSE
    if (input$keyrelease==16) shiftKeyOn<<-FALSE
  })
  
}

