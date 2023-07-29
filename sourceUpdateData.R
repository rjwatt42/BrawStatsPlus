####################################
# change between data and simulation

changeUI2Data<-function() {
  # get the variables into the ui
  if (switches$rigidWithin) {
    if (variables$deploy[1]=="Within") {
      DVchoices<-strsplit(substring(variables$targetDeploys[1],2,nchar(variables$targetDeploys[1])-1),",")[[1]]
      use<-!(variables$name %in% DVchoices) & (sapply(variables$targetDeploys,nchar)==0)
      DVchoices<-list("applicable"=c(DVchoices," "),"not applicable"=variables$name[use])
      updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices$available[1])
    } else {
      DVchoices=variables$name[sapply(variables$targetDeploys,nchar)==0]
      updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices[length(DVchoices)])
    }
    updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
    updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
  } else {
    updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
    updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
    updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[nrow(variables)])
  }
  setIVanyway()
  setIV2anyway()
  setDVanyway()
  
  updateNumericInput(session,"sN",value=length(unique(importedData[[1]])))
  if (switches$doBootstrap) {
    shinyjs::showElement(id= "EvidenceHypothesisApply")
  }
  updateTabsetPanel(session, "Hypothesis",selected = "Variables")
  updateTabsetPanel(session, "Evidence",selected = "Single")
  updateNumericInput(session,"rIV",value=NA)
  
  shinyjs::disable(id= "rIV")
  shinyjs::disable(id= "rIV2")
  shinyjs::disable(id= "rIVIV2")
  shinyjs::disable(id= "rIVIV2DV")
  
  shinyjs::disable(id= "sN")
  shinyjs::disable(id= "sMethod")
  shinyjs::disable(id= "sIV1Use")
  shinyjs::disable(id= "sIV2Use")
  shinyjs::disable(id= "sDependence")
  shinyjs::disable(id= "sOutliers")
  shinyjs::disable(id= "sRangeOn")
  
  if (!switches$doBootstrap) {
    shinyjs::hideElement(id="EvidenceHypothesisApply")
    updateActionButton(session,"EvidencenewSample", label="Analyze")
    hideTab("Hypothesis","Effects")
    hideTab("Evidence","Multiple")
    shinyjs::hideElement(id="uiExplore")
  } else {
    updateActionButton(session,"EvidencenewSample", label="Resample")
    if (input$IV2choice=="none") {
      updateSelectInput(session,"Explore_typeH", choices=hypothesisChoicesV2Plain)
    }
    else {
      updateSelectInput(session,"Explore_typeH", choices=hypothesisChoicesV3Plain)
    }
    updateSelectInput(session,"Explore_VtypeH",choices=c("& type"="Type"))
    updateSelectInput(session,"Explore_typeD",choices=c("Sample Size" = "SampleSize"))
  }
  hideTab("Design","Anomalies")
  shinyjs::hideElement(id="DesignMethod")
  runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",darkpanelcolours$hypothesisC))
  runjs(sprintf("document.getElementById('Sampling').style.backgroundColor = '%s';",darkpanelcolours$designC))
  runjs(sprintf("document.getElementById('Single').style.backgroundColor = '%s';",darkpanelcolours$simulateC))
  runjs(sprintf("document.getElementById('Multiple').style.backgroundColor = '%s';",darkpanelcolours$simulateC))
  runjs(sprintf("document.getElementById('ExploreHypothesis').style.backgroundColor = '%s';",darkpanelcolours$exploreC))
  runjs(sprintf("document.getElementById('ExploreDesign').style.backgroundColor = '%s';",darkpanelcolours$exploreC))
  
  variablesHeld<<-"Data"
  
  updateSelectInput(session,"Using",choices=c("Simulations"="Simulations","Data"="Data"),selected="Data")
  shinyjs::showElement(id= "Using")
  
}


changeUI2Simulations<-function() {
  # get the variables into the ui
  updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
  updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
  updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])
  setIVanyway()
  setIV2anyway()
  setDVanyway()
  
  if (switches$doBootstrap) {
    shinyjs::hideElement(id= "EvidenceHypothesisApply")
  }
  updateTabsetPanel(session, "Hypothesis",selected = "Variables")
  updateNumericInput(session,"rIV",value=0)
  
  shinyjs::enable(id= "rIV")
  shinyjs::enable(id= "rIV2")
  shinyjs::enable(id= "rIVIV2")
  shinyjs::enable(id= "rIVIV2DV")
  
  shinyjs::enable(id= "sN")
  shinyjs::enable(id= "sMethod")
  shinyjs::enable(id= "sIV1Use")
  shinyjs::enable(id= "sIV2Use")
  shinyjs::enable(id= "sDependence")
  shinyjs::enable(id= "sOutliers")
  shinyjs::enable(id= "sRangeOn")
  
  updateActionButton(session,"EvidencenewSample", label="New Sample")
  showTab("Hypothesis","Effects")
  showTab("Evidence","Multiple")
  shinyjs::showElement(id="uiExplore")
  if (input$IV2choice=="none") {
    updateSelectInput(session,"Explore_typeH", choices=hypothesisChoicesV2)
  } else {
    updateSelectInput(session,"Explore_typeH", choices=hypothesisChoicesV3)
  }
  updateSelectInput(session,"Explore_VtypeH",choices=variableChoices)
  updateSelectInput(session,"Explore_typeD",choices=designChoices)
  
  showTab("Design","Anomalies")
  shinyjs::showElement(id="DesignMethod")
  
  runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",subpanelcolours$hypothesisC))
  runjs(sprintf("document.getElementById('Sampling').style.backgroundColor = '%s';",subpanelcolours$designC))
  runjs(sprintf("document.getElementById('Single').style.backgroundColor = '%s';",subpanelcolours$simulateC))
  runjs(sprintf("document.getElementById('Multiple').style.backgroundColor = '%s';",subpanelcolours$simulateC))
  runjs(sprintf("document.getElementById('ExploreHypothesis').style.backgroundColor = '%s';",subpanelcolours$exploreC))
  runjs(sprintf("document.getElementById('ExploreDesign').style.backgroundColor = '%s';",subpanelcolours$exploreC))
  
  updateSelectInput(session,"Using",choices=c("Simulations"="Simulations","Data"="Data"),selected="Simulations")
  shinyjs::showElement(id= "Using")
  
  variablesHeld<<-"Simulations"
  
}

observeEvent(input$AllowResampling,{
  if (debug) debugPrint("AllowResampling")
  switches$doBootstrap<<-input$AllowResampling
  
  switch(input$Using,
         "Simulations"={
           changeUI2Simulations()
         },
         "Data"={
           changeUI2Data()
         })
})

observeEvent(input$Using,{
  if (debug) debugPrint("Using")
  
  if (variablesHeld==input$Using) {return()}
  if (input$Using=="OK") {return()}
  
  local<-variables
  variables<<-defaultVariables
  defaultVariables<<-local
  
  switch(input$Using,
         "Simulations"={
           changeUI2Simulations()
         },
         "Data"={
           changeUI2Data()
         })
  # get the variables into the ui
  setIVanyway()
  setIV2anyway()
  setDVanyway()
})

