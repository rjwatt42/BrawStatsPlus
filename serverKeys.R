####################################
#KEYBOARD: capture keyboard events

source("extras.R")
source("restoreAll.R")

ascii<-function(ch) strtoi(charToRaw(toupper(ch)),16L)

if (switches$doKeys) {
  keyrespond<-observeEvent(input$pressedKey,{
    # 
    
    if (input$keypress==16) shiftKeyOn<<-TRUE
    if (input$keypress==17) controlKeyOn<<-TRUE
    if (input$keypress==18) altKeyOn<<-TRUE
    
    # control-c
    if (is_local && input$keypress==ascii("c") && controlKeyOn){
      data<-exportData()
      write_clip(data,allow_non_interactive = TRUE)
    }
    
    # control-d
    if (is_local && input$keypress==ascii("d") && controlKeyOn){
      debug<<-!debug
    }
    
    # control-g toggle LARGE GRAPHS
    if (input$keypress==ascii("g") && controlKeyOn){
      was<-input$LargeGraphs
      updateCheckboxInput(session,"LargeGraphs",value=!was)
    }
    
    # control-l set shortHand to TRUE
    if (input$keypress==ascii("l") && controlKeyOn){
      updateCheckboxInput(session,"shortHand",value=!input$shortHand)
      updateSelectInput(session,"EvidenceExpected_length",selected="1000")
      updateSelectInput(session,"Explore_lengthH",selected="100")
      updateSelectInput(session,"Explore_lengthD",selected="100")
    }
    
    # control-m - add in meta-analysis
    if (input$keypress==ascii("m") && controlKeyOn){
      switches$doMetaAnalysis<<-TRUE
      insertTab("Evidence",metaPanel(),"Multiple","after",select=FALSE,session)
      insertTab("Graphs",metaGraphPanel(),"Expect","after",select=FALSE,session)
      insertTab("Reports",metaReportPanel(),"Expect","after",select=FALSE,session)
      insertTab("ExploreTab",exploreMeta(),"Design","after",select=FALSE,session)
      insertTab("FileTab",metaFilePanel(),"Data","after",select=FALSE,session)
    }

    # control-p set world to model psych
    if (input$keypress==ascii("p") && controlKeyOn){
      if (pPlus) {v<-0.26}
      else       {v<-0.74}
      loadExtras(session)
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.332)
      updateNumericInput(session,"world_distr_Nullp",value=v)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
      
      if (shiftKeyOn) {
        updateCheckboxInput(session,"sNRand",value=TRUE)
        updateNumericInput(session,"sNRandK",value=1.56)
        updateNumericInput(session,"sN",value=47+5)
      }
    }
    
    # control-r 
    if (input$keypress==ascii("r") && controlKeyOn) {
      updateCheckboxInput(session,"WhiteGraphs",value=TRUE)
      updateCheckboxInput(session,"evidenceTheory",value=TRUE)
      updateCheckboxInput(session,"shortHand",value=TRUE)
      updateSelectInput(session,"RZ",selected="z")
      updateSelectInput(session,"EvidenceExpected_type",selected="Single")
    }
    
    # control-s 
    if (input$keypress==ascii("s") && controlKeyOn && !shiftKeyOn) {
      state<-storeAll(input)
      write_xlsx(state,path="temp.xlsx")
      hmm("BrawStats state saved","Success:")
    }
    
    # control-shift-s 
    if (input$keypress==ascii("s") && controlKeyOn && shiftKeyOn) {
      state<-read_xlsx(path="temp.xlsx")
      restoreAll(session,state)
      hmm("BrawStats state retrieved","Success:")
    }
    
    # control-t set showTheory to TRUE
    if (input$keypress==ascii("t") && controlKeyOn){
      updateCheckboxInput(session,"evidenceTheory",value=TRUE)
      updateCheckboxInput(session,"possibleTheory",value=TRUE)
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
    
    # control-w set to power
    if (input$keypress==ascii("w") && controlKeyOn){
      updateNumericInput(session,"possiblePSampRho",value=0)
      updateSelectInput(session,"possibleShow",selected="Power")
      updateTabsetPanel(session, "PossiblePanel",selected="Populations")
    }
    
    # control-shift-w white graphs
    if (input$keypress==ascii("w") && controlKeyOn && shiftKeyOn){
      was<-input$WhiteGraphs
      updateCheckboxInput(session,"WhiteGraphs",value=!was)
    }
    
    # control-x load extras
    if (input$keypress==ascii("x") && controlKeyOn){
      updateCheckboxInput(session,"LoadExtras",value=!input$LoadExtras)
    }
    
    # control-z switch to z
    if (input$keypress==ascii("z") && controlKeyOn){
      updateSelectInput(session,"RZ",selected="z")
    }
  })
  
  keyrespondUp<-observeEvent(input$keyrelease,{
    if (input$keyrelease==18) altKeyOn<<-FALSE
    if (input$keyrelease==17) controlKeyOn<<-FALSE
    if (input$keyrelease==16) shiftKeyOn<<-FALSE
  })
  
}

